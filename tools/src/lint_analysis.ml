open Analysis
open SharedTypes
open Lint_shared

let starts_with_path ~prefix path =
  let rec loop prefix path =
    match (prefix, path) with
    | [], _ -> true
    | one :: prefix, two :: path when one = two -> loop prefix path
    | _ -> false
  in
  loop prefix path

type forbidden_symbol = {kind: forbidden_reference_kind; path: string list}

type resolved_symbol = {
  kind: forbidden_reference_kind;
  path: string list;
  declaration_source_path: string option;
  source_root_reference_kind: forbidden_source_root_reference_kind option;
}

type forbidden_item_match =
  | ForbiddenItemExact
  | ForbiddenItemModulePrefix of int

let forbidden_item_match (item : forbidden_reference_item)
    (symbol : forbidden_symbol) =
  match item.kind with
  | ForbiddenReferenceModule ->
    if starts_with_path ~prefix:item.path symbol.path then
      Some (ForbiddenItemModulePrefix (List.length item.path))
    else None
  | ForbiddenReferenceValue | ForbiddenReferenceType ->
    if item.kind = symbol.kind && item.path = symbol.path then
      Some ForbiddenItemExact
    else None

let forbidden_item_match_is_better candidate best =
  match (candidate, best) with
  | ForbiddenItemExact, ForbiddenItemModulePrefix _ -> true
  | ForbiddenItemModulePrefix _, ForbiddenItemExact -> false
  | ForbiddenItemExact, ForbiddenItemExact -> false
  | ( ForbiddenItemModulePrefix candidate_length,
      ForbiddenItemModulePrefix best_length ) ->
    candidate_length > best_length

let best_matching_forbidden_item items symbol =
  items
  |> List.fold_left
       (fun best (item : forbidden_reference_item) ->
         match forbidden_item_match item symbol with
         | None -> best
         | Some candidate_match -> (
           match best with
           | None -> Some (candidate_match, item)
           | Some (best_match, _best_item) ->
             if forbidden_item_match_is_better candidate_match best_match then
               Some (candidate_match, item)
             else best))
       None
  |> Option.map snd

let path_key path = String.concat "." path

let package_for_path path =
  let uri = Uri.fromPath path in
  Packages.getPackage ~uri

let declared_symbol_path ~module_name (declared : _ SharedTypes.Declared.t) =
  module_name
  :: SharedTypes.ModulePath.toPath declared.modulePath declared.name.txt

let rec source_path_of_module_path = function
  | SharedTypes.ModulePath.File (uri, _) -> Some (Uri.toPath uri)
  | SharedTypes.ModulePath.IncludedModule (_, module_path) ->
    source_path_of_module_path module_path
  | SharedTypes.ModulePath.ExportedModule {modulePath; _} ->
    source_path_of_module_path modulePath
  | SharedTypes.ModulePath.NotVisible -> None

let declared_source_path (declared : _ SharedTypes.Declared.t) =
  source_path_of_module_path declared.modulePath

let path_is_within_root ~path ~root =
  let path = if Files.exists path then Unix.realpath path else path in
  let root = if Files.exists root then Unix.realpath root else root in
  if root = "" then false
  else
    let root_length = String.length root in
    Files.pathStartsWith path root
    && (String.length path = root_length
       || root.[root_length - 1] = Filename.dir_sep.[0]
       || path.[root_length] = Filename.dir_sep.[0])

let rec take_path count path =
  if count <= 0 then []
  else
    match path with
    | [] -> []
    | head :: tail -> head :: take_path (count - 1) tail

let rec drop_path count path =
  if count <= 0 then path
  else
    match path with
    | [] -> []
    | _ :: tail -> drop_path (count - 1) tail

let tip_of_forbidden_reference_kind = function
  | ForbiddenReferenceModule -> Tip.Module
  | ForbiddenReferenceValue -> Tip.Value
  | ForbiddenReferenceType -> Tip.Type

let resolve_exported_path ~env ~package ~kind path =
  let tip = tip_of_forbidden_reference_kind kind in
  match References.exportedForTip ~env ~path ~package ~tip with
  | None -> None
  | Some (env, _name, stamp) -> (
    match kind with
    | ForbiddenReferenceModule ->
      Stamps.findModule env.file.stamps stamp
      |> Option.map (declared_symbol_path ~module_name:env.file.moduleName)
    | ForbiddenReferenceValue ->
      Stamps.findValue env.file.stamps stamp
      |> Option.map (declared_symbol_path ~module_name:env.file.moduleName)
    | ForbiddenReferenceType ->
      Stamps.findType env.file.stamps stamp
      |> Option.map (declared_symbol_path ~module_name:env.file.moduleName))

let resolve_forbidden_reference_items ~target_path items =
  match package_for_path target_path with
  | None -> items
  | Some package ->
    let resolved_module_cache = Hashtbl.create 16 in
    let resolved_cache = Hashtbl.create 16 in
    let resolve_module_prefix path =
      match Hashtbl.find_opt resolved_module_cache (path_key path) with
      | Some resolved -> resolved
      | None ->
        let resolved =
          Lint_support.SymbolPath.resolve_module_env ~package path
        in
        Hashtbl.add resolved_module_cache (path_key path) resolved;
        resolved
    in
    let resolve_item_from_module_prefix (item : forbidden_reference_item) =
      let rec loop prefix_length =
        if prefix_length <= 0 then None
        else
          let module_prefix = take_path prefix_length item.path in
          match resolve_module_prefix module_prefix with
          | None -> loop (prefix_length - 1)
          | Some env -> (
            let remainder = drop_path prefix_length item.path in
            match remainder with
            | [] -> Some [env.QueryEnv.file.moduleName]
            | _ -> resolve_exported_path ~env ~package ~kind:item.kind remainder
            )
      in
      loop (List.length item.path)
    in
    let resolve_item (item : forbidden_reference_item) =
      let key =
        forbidden_reference_kind_to_string item.kind ^ ":" ^ path_key item.path
      in
      match Hashtbl.find_opt resolved_cache key with
      | Some resolved -> resolved
      | None ->
        let resolved =
          resolve_item_from_module_prefix item
          |> Option.value ~default:item.path
        in
        let item = {item with path = resolved} in
        Hashtbl.add resolved_cache key item;
        item
    in
    items |> List.map resolve_item

module Ast = struct
  let rec is_function_expression (expression : Parsetree.expression) =
    match expression.pexp_desc with
    | Pexp_fun _ -> true
    | Pexp_constraint (expression, _)
    | Pexp_open (_, _, expression)
    | Pexp_newtype (_, expression) ->
      is_function_expression expression
    | _ -> false

  let rec collect_binding_names (pattern : Parsetree.pattern) =
    match pattern.ppat_desc with
    | Ppat_var name | Ppat_alias (_, name) -> [name.loc]
    | Ppat_constraint (pattern, _) -> collect_binding_names pattern
    | _ -> []

  let rec qualified_ident_path (expression : Parsetree.expression) =
    match expression.pexp_desc with
    | Pexp_ident {txt = Longident.Ldot _ as lid; _} ->
      Some (Utils.flattenLongIdent lid)
    | Pexp_constraint (expression, _)
    | Pexp_open (_, _, expression)
    | Pexp_newtype (_, expression) ->
      qualified_ident_path expression
    | _ -> None

  let rec qualified_module_path (module_expr : Parsetree.module_expr) =
    match module_expr.pmod_desc with
    | Pmod_ident {txt = Longident.Ldot _ as lid; _} ->
      Some (Utils.flattenLongIdent lid)
    | Pmod_constraint (module_expr, _) -> qualified_module_path module_expr
    | _ -> None

  let qualified_type_path (core_type : Parsetree.core_type) =
    match core_type.ptyp_desc with
    | Ptyp_constr ({txt = Longident.Ldot _ as lid; _}, _arguments) ->
      Some (Utils.flattenLongIdent lid)
    | _ -> None

  let alias_avoidance_finding ~path (rule : alias_avoidance_rule) ~loc
      symbol_path =
    let symbol = Some (String.concat "." symbol_path) in
    raw_finding ~rule:"alias-avoidance" ~abs_path:path ~loc
      ~severity:rule.severity
      ~message:(effective_alias_avoidance_message rule)
      ?symbol ()

  let value_alias_avoidance_findings ~path (rule : alias_avoidance_rule)
      bindings =
    bindings
    |> List.filter_map (fun (binding : Parsetree.value_binding) ->
           match collect_binding_names binding.pvb_pat with
           | [name_loc] -> (
             match qualified_ident_path binding.pvb_expr with
             | None -> None
             | Some symbol_path ->
               Some
                 (alias_avoidance_finding ~path rule ~loc:name_loc symbol_path))
           | _ -> None)

  let module_alias_avoidance_finding ~path (rule : alias_avoidance_rule)
      (module_binding : Parsetree.module_binding) =
    qualified_module_path module_binding.pmb_expr
    |> Option.map (fun symbol_path ->
           alias_avoidance_finding ~path rule ~loc:module_binding.pmb_name.loc
             symbol_path)

  let module_declaration_alias_avoidance_finding ~path
      (rule : alias_avoidance_rule)
      (module_declaration : Parsetree.module_declaration) =
    match module_declaration.pmd_type.pmty_desc with
    | Pmty_alias {txt = Longident.Ldot _ as lid; _} ->
      Some
        (alias_avoidance_finding ~path rule ~loc:module_declaration.pmd_name.loc
           (Utils.flattenLongIdent lid))
    | _ -> None

  let local_module_alias_avoidance_finding ~path (rule : alias_avoidance_rule)
      (name : string Location.loc) (module_expr : Parsetree.module_expr) =
    qualified_module_path module_expr
    |> Option.map (fun symbol_path ->
           alias_avoidance_finding ~path rule ~loc:name.loc symbol_path)

  let type_alias_avoidance_findings ~path (rule : alias_avoidance_rule) decls =
    decls
    |> List.filter_map (fun (decl : Parsetree.type_declaration) ->
           match decl.ptype_manifest with
           | None -> None
           | Some manifest -> (
             match qualified_type_path manifest with
             | None -> None
             | Some symbol_path ->
               Some
                 (alias_avoidance_finding ~path rule ~loc:decl.ptype_name.loc
                    symbol_path)))

  let preferred_type_syntax_finding ~path (rule : preferred_type_syntax_rule)
      ~(symbol_path : string list) ~loc =
    let symbol = Some (String.concat "." symbol_path) in
    raw_finding ~rule:"preferred-type-syntax" ~abs_path:path ~loc
      ~severity:rule.severity
      ~message:(effective_preferred_type_syntax_message rule)
      ?symbol ()

  let summary_of_file ?alias_avoidance_rule
      ?(preferred_type_syntax_rule : preferred_type_syntax_rule option) path =
    let local_function_bindings = ref StringSet.empty in
    let ast_findings = ref [] in
    let inspect_bindings bindings =
      bindings
      |> List.iter (fun (binding : Parsetree.value_binding) ->
             if is_function_expression binding.pvb_expr then
               collect_binding_names binding.pvb_pat
               |> List.iter (fun loc ->
                      local_function_bindings :=
                        StringSet.add (loc_key loc) !local_function_bindings));
      match alias_avoidance_rule with
      | None -> ()
      | Some rule ->
        ast_findings :=
          value_alias_avoidance_findings ~path rule bindings @ !ast_findings
    in
    let iterator =
      let open Ast_iterator in
      {
        Ast_iterator.default_iterator with
        structure_item =
          (fun iter structure_item ->
            (match structure_item.pstr_desc with
            | Pstr_value (_rec_flag, bindings) -> inspect_bindings bindings
            | Pstr_type (_rec_flag, decls) -> (
              match alias_avoidance_rule with
              | None -> ()
              | Some rule ->
                ast_findings :=
                  type_alias_avoidance_findings ~path rule decls @ !ast_findings
              )
            | Pstr_module module_binding -> (
              match alias_avoidance_rule with
              | None -> ()
              | Some rule -> (
                match
                  module_alias_avoidance_finding ~path rule module_binding
                with
                | None -> ()
                | Some finding -> ast_findings := finding :: !ast_findings))
            | Pstr_recmodule module_bindings -> (
              match alias_avoidance_rule with
              | None -> ()
              | Some rule ->
                ast_findings :=
                  (module_bindings
                  |> List.filter_map (module_alias_avoidance_finding ~path rule)
                  )
                  @ !ast_findings)
            | _ -> ());
            Ast_iterator.default_iterator.structure_item iter structure_item);
        signature_item =
          (fun iter signature_item ->
            (match signature_item.psig_desc with
            | Psig_type (_rec_flag, decls) -> (
              match alias_avoidance_rule with
              | None -> ()
              | Some rule ->
                ast_findings :=
                  type_alias_avoidance_findings ~path rule decls @ !ast_findings
              )
            | Psig_module module_declaration -> (
              match alias_avoidance_rule with
              | None -> ()
              | Some rule -> (
                match
                  module_declaration_alias_avoidance_finding ~path rule
                    module_declaration
                with
                | None -> ()
                | Some finding -> ast_findings := finding :: !ast_findings))
            | Psig_recmodule module_declarations -> (
              match alias_avoidance_rule with
              | None -> ()
              | Some rule ->
                ast_findings :=
                  (module_declarations
                  |> List.filter_map
                       (module_declaration_alias_avoidance_finding ~path rule))
                  @ !ast_findings)
            | _ -> ());
            Ast_iterator.default_iterator.signature_item iter signature_item);
        typ =
          (fun iter (core_type : Parsetree.core_type) ->
            (match preferred_type_syntax_rule with
            | Some rule when rule.enabled && rule.dict -> (
              match core_type.ptyp_desc with
              | Ptyp_constr ({txt = Longident.Ldot _ as lid; loc}, _arguments)
                when preferred_type_syntax_dict_path
                       (Utils.flattenLongIdent lid) ->
                ast_findings :=
                  preferred_type_syntax_finding ~path rule
                    ~symbol_path:(Utils.flattenLongIdent lid)
                    ~loc
                  :: !ast_findings
              | _ -> ())
            | Some _ | None -> ());
            Ast_iterator.default_iterator.typ iter core_type);
        expr =
          (fun iter expression ->
            (match expression.pexp_desc with
            | Pexp_let (_rec_flag, bindings, _) -> inspect_bindings bindings
            | Pexp_letmodule (name, module_expr, _) -> (
              match alias_avoidance_rule with
              | None -> ()
              | Some rule -> (
                match
                  local_module_alias_avoidance_finding ~path rule name
                    module_expr
                with
                | None -> ()
                | Some finding -> ast_findings := finding :: !ast_findings))
            | _ -> ());
            Ast_iterator.default_iterator.expr iter expression);
      }
    in
    let parse_errors =
      match Files.classifySourceFile path with
      | Resi ->
        let {Res_driver.parsetree; diagnostics; invalid; _} =
          Res_driver.parsing_engine.parse_interface ~for_printer:false
            ~filename:path
        in
        if not invalid then
          Ast_iterator.default_iterator.signature iterator parsetree;
        diagnostics
        |> List.map (fun diagnostic ->
               {
                 rule = "parse-error";
                 abs_path = path;
                 loc =
                   {
                     Location.loc_start =
                       Res_diagnostics.get_start_pos diagnostic;
                     loc_end = Res_diagnostics.get_end_pos diagnostic;
                     loc_ghost = false;
                   };
                 severity = SeverityError;
                 message = Res_diagnostics.explain diagnostic;
                 symbol = None;
               })
      | _ ->
        let {Res_driver.parsetree; diagnostics; invalid; _} =
          Res_driver.parsing_engine.parse_implementation ~for_printer:false
            ~filename:path
        in
        if not invalid then
          Ast_iterator.default_iterator.structure iterator parsetree;
        diagnostics
        |> List.map (fun diagnostic ->
               {
                 rule = "parse-error";
                 abs_path = path;
                 loc =
                   {
                     Location.loc_start =
                       Res_diagnostics.get_start_pos diagnostic;
                     loc_end = Res_diagnostics.get_end_pos diagnostic;
                     loc_ghost = false;
                   };
                 severity = SeverityError;
                 message = Res_diagnostics.explain diagnostic;
                 symbol = None;
               })
    in
    ( {parse_errors; local_function_bindings = !local_function_bindings},
      List.rev !ast_findings )
end

module Typed = struct
  let forbidden_source_root_reference_kind_matches left right = left = right

  let resolved_symbol_of_declared ~module_name ~kind declared =
    {
      kind;
      path = declared_symbol_path ~module_name declared;
      declaration_source_path = declared_source_path declared;
      source_root_reference_kind =
        (match kind with
        | ForbiddenReferenceValue -> Some ForbiddenSourceRootReferenceValue
        | ForbiddenReferenceType -> Some ForbiddenSourceRootReferenceType
        | ForbiddenReferenceModule -> None);
    }

  let resolve_global_symbol ~package ~module_name ~path ~tip =
    match ProcessCmt.fileForModule module_name ~package with
    | None -> None
    | Some file ->
      let env = QueryEnv.fromFile file in
      Option.bind (References.exportedForTip ~env ~path ~package ~tip)
        (fun (env, _name, stamp) ->
          match tip with
          | Tip.Value ->
            Stamps.findValue env.file.stamps stamp
            |> Option.map
                 (resolved_symbol_of_declared ~module_name:env.file.moduleName
                    ~kind:ForbiddenReferenceValue)
          | Tip.Type ->
            Stamps.findType env.file.stamps stamp
            |> Option.map
                 (resolved_symbol_of_declared ~module_name:env.file.moduleName
                    ~kind:ForbiddenReferenceType)
          | Tip.Module ->
            Stamps.findModule env.file.stamps stamp
            |> Option.map
                 (resolved_symbol_of_declared ~module_name:env.file.moduleName
                    ~kind:ForbiddenReferenceModule)
          | Tip.Field field_name ->
            Stamps.findType env.file.stamps stamp
            |> Option.map (fun declared ->
                   {
                     kind = ForbiddenReferenceType;
                     path =
                       declared_symbol_path ~module_name:env.file.moduleName
                         declared
                       @ [field_name];
                     declaration_source_path = declared_source_path declared;
                     source_root_reference_kind = None;
                   })
          | Tip.Constructor constructor_name ->
            Stamps.findType env.file.stamps stamp
            |> Option.map (fun declared ->
                   {
                     kind = ForbiddenReferenceType;
                     path =
                       declared_symbol_path ~module_name:env.file.moduleName
                         declared
                       @ [constructor_name];
                     declaration_source_path = declared_source_path declared;
                     source_root_reference_kind = None;
                   }))

  let resolve_local_symbol ~(file : File.t) ~tip stamp =
    match tip with
    | Tip.Value ->
      Stamps.findValue file.stamps stamp
      |> Option.map
           (resolved_symbol_of_declared ~module_name:file.moduleName
              ~kind:ForbiddenReferenceValue)
    | Tip.Type ->
      Stamps.findType file.stamps stamp
      |> Option.map
           (resolved_symbol_of_declared ~module_name:file.moduleName
              ~kind:ForbiddenReferenceType)
    | Tip.Module ->
      Stamps.findModule file.stamps stamp
      |> Option.map
           (resolved_symbol_of_declared ~module_name:file.moduleName
              ~kind:ForbiddenReferenceModule)
    | Tip.Field field_name ->
      Stamps.findType file.stamps stamp
      |> Option.map (fun declared ->
             {
               kind = ForbiddenReferenceType;
               path =
                 declared_symbol_path ~module_name:file.moduleName declared
                 @ [field_name];
               declaration_source_path = declared_source_path declared;
               source_root_reference_kind = None;
             })
    | Tip.Constructor constructor_name ->
      Stamps.findType file.stamps stamp
      |> Option.map (fun declared ->
             {
               kind = ForbiddenReferenceType;
               path =
                 declared_symbol_path ~module_name:file.moduleName declared
                 @ [constructor_name];
               declaration_source_path = declared_source_path declared;
               source_root_reference_kind = None;
             })

  let symbol (full : SharedTypes.full) (loc_item : locItem) =
    match loc_item.locType with
    | Typed (_, _typ, LocalReference (stamp, tip)) ->
      resolve_local_symbol ~file:full.file ~tip stamp
    | Typed (_, _typ, GlobalReference (module_name, path, tip)) ->
      resolve_global_symbol ~package:full.package ~module_name ~path ~tip
    | Typed (_, _, (Definition _ | NotFound))
    | LModule _ | TopLevelModule _ | Constant _ | TypeDefinition _ ->
      None

  let forbidden_reference_findings ~config ~path (full : SharedTypes.full) =
    let matching_rule symbol =
      let forbidden_symbol = {kind = symbol.kind; path = symbol.path} in
      config.forbidden_reference
      |> List.find_map (fun (rule : forbidden_reference_rule) ->
             if (not rule.enabled) || rule.items = [] then None
             else
               best_matching_forbidden_item rule.items forbidden_symbol
               |> Option.map (fun item -> (rule, item)))
    in
    full.extra.locItems
    |> List.filter_map (fun loc_item ->
           match symbol full loc_item with
           | None -> None
           | Some symbol -> (
             match matching_rule symbol with
             | None -> None
             | Some (rule, item) ->
               let symbol = Some (String.concat "." symbol.path) in
               Some
                 (raw_finding ~rule:"forbidden-reference" ~abs_path:path
                    ~loc:loc_item.loc ~severity:rule.severity
                    ~message:
                      (effective_forbidden_reference_item_message rule item)
                    ?symbol ())))

  let forbidden_source_root_reference_findings ~config ~path
      (full : SharedTypes.full) =
    let rule = config.forbidden_source_root_reference in
    if (not rule.enabled) || rule.roots = [] || rule.kinds = [] then []
    else
      let file_is_within_root root =
        path_is_within_root ~path ~root:root.abs_path
      in
      full.extra.locItems
      |> List.filter_map (fun loc_item ->
             match symbol full loc_item with
             | None -> None
             | Some symbol -> (
               match symbol.declaration_source_path with
               | None -> None
               | Some declaration_source_path ->
                 rule.roots
                 |> List.find_opt (fun root ->
                        (not (file_is_within_root root))
                        && (match symbol.source_root_reference_kind with
                           | None -> false
                           | Some symbol_kind ->
                             rule.kinds
                             |> List.exists (fun kind ->
                                    forbidden_source_root_reference_kind_matches
                                      kind symbol_kind))
                        && path_is_within_root ~path:declaration_source_path
                             ~root:root.abs_path)
                 |> Option.map (fun root ->
                        let symbol = Some (String.concat "." symbol.path) in
                        raw_finding ~rule:"forbidden-source-root-reference"
                          ~abs_path:path ~loc:loc_item.loc
                          ~severity:rule.severity
                          ~message:
                            (effective_forbidden_source_root_reference_message
                               rule root)
                          ?symbol ())))

  let is_function_type typ =
    match (Shared.dig typ).desc with
    | Tarrow _ -> true
    | _ -> false

  let single_use_function_findings ~config ~path ~local_function_bindings
      (full : SharedTypes.full) =
    if not config.single_use_function.enabled then []
    else
      let rule = config.single_use_function in
      let findings = ref [] in
      Stamps.iterValues
        (fun stamp (declared : Types.type_expr Declared.t) ->
          if
            (not declared.isExported)
            && is_function_type declared.item
            && StringSet.mem (loc_key declared.name.loc) local_function_bindings
          then
            let references =
              Hashtbl.find_opt full.extra.internalReferences stamp
              |> Option.value ~default:[]
            in
            let use_count = max 0 (List.length references - 1) in
            if use_count = 1 then
              let symbol = Some declared.name.txt in
              findings :=
                raw_finding ~rule:"single-use-function" ~abs_path:path
                  ~loc:declared.name.loc ~severity:rule.severity
                  ~message:(effective_single_use_function_message rule)
                  ?symbol ()
                :: !findings)
        full.file.stamps;
      List.rev !findings
end

let has_typed_artifact path =
  let uri = Uri.fromPath path in
  match Packages.getPackage ~uri with
  | None -> false
  | Some package ->
    let module_name =
      BuildSystem.namespacedName package.namespace (FindFiles.getName path)
    in
    Hashtbl.mem package.pathsForModule module_name

let source_files_in_package package =
  package.projectFiles |> FileSet.elements
  |> List.filter_map (fun module_name ->
         Hashtbl.find_opt package.pathsForModule module_name
         |> Option.map getSrc)
  |> List.concat
  |> List.filter FindFiles.isSourceFile

let collect_files target_path =
  if Lint_support.Path.is_directory target_path then
    match package_for_path target_path with
    | Some package ->
      source_files_in_package package
      |> List.filter (fun file -> Files.pathStartsWith file target_path)
      |> List.sort_uniq String.compare
    | None ->
      Files.collect target_path FindFiles.isSourceFile
      |> List.sort_uniq String.compare
  else [target_path]

let display_base target_path files =
  match files with
  | file :: _ -> (
    match package_for_path file with
    | Some package -> package.rootPath
    | None ->
      if Lint_support.Path.is_directory target_path then target_path
      else Filename.dirname target_path)
  | [] ->
    if Lint_support.Path.is_directory target_path then target_path
    else Filename.dirname target_path

let dedupe_findings findings =
  let same_signature (left : raw_finding) (right : raw_finding) =
    left.rule = right.rule
    && left.abs_path = right.abs_path
    && left.symbol = right.symbol
    && left.message = right.message
  in
  let rec loop acc = function
    | [] -> List.rev acc
    | finding :: rest ->
      let skip =
        acc
        |> List.exists (fun seen ->
               same_signature finding seen && Loc.isInside seen.loc finding.loc)
      in
      if skip then loop acc rest else loop (finding :: acc) rest
  in
  loop [] findings

let compare_raw_findings (left : raw_finding) (right : raw_finding) =
  let span ({Location.loc_start; loc_end} : Location.t) =
    ( loc_end.pos_lnum - loc_start.pos_lnum,
      loc_end.pos_cnum - loc_start.pos_cnum )
  in
  compare
    ( left.abs_path,
      left.rule,
      left.symbol,
      left.message,
      span left.loc,
      Lint_support.Range.of_loc left.loc )
    ( right.abs_path,
      right.rule,
      right.symbol,
      right.message,
      span right.loc,
      Lint_support.Range.of_loc right.loc )

let analyze_file ~config path =
  let alias_avoidance_rule =
    if config.alias_avoidance.enabled then Some config.alias_avoidance else None
  in
  let preferred_type_syntax_rule =
    if config.preferred_type_syntax.enabled && config.preferred_type_syntax.dict
    then Some config.preferred_type_syntax
    else None
  in
  let ast, ast_findings =
    Ast.summary_of_file ?alias_avoidance_rule ?preferred_type_syntax_rule path
  in
  let findings = ref ast.parse_errors in
  if ast.parse_errors = [] then (
    findings := ast_findings @ !findings;
    match
      if has_typed_artifact path then Cmt.loadFullCmtFromPath ~path else None
    with
    | None -> ()
    | Some full ->
      findings :=
        Typed.forbidden_reference_findings ~config ~path full
        @ Typed.forbidden_source_root_reference_findings ~config ~path full
        @ Typed.single_use_function_findings ~config ~path
            ~local_function_bindings:ast.local_function_bindings full
        @ !findings);
  !findings

type analyzed_target = {display_base: string; findings: raw_finding list}

let analyze_target ~config target_path =
  let config =
    {
      config with
      forbidden_reference =
        config.forbidden_reference
        |> List.map (fun (rule : forbidden_reference_rule) ->
               {
                 rule with
                 items =
                   resolve_forbidden_reference_items ~target_path rule.items;
               });
    }
  in
  let files = collect_files target_path in
  let display_base = display_base target_path files in
  let findings =
    files
    |> List.concat_map (analyze_file ~config)
    |> dedupe_findings
    |> List.sort compare_raw_findings
  in
  {display_base; findings}

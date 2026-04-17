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

let match_forbidden_path items path =
  items
  |> List.find_map (fun item ->
         if starts_with_path ~prefix:item path then Some (item, path) else None)

let path_key path = String.concat "." path

let placeholder_module_path = "place holder"

let package_for_path path =
  let uri = Uri.fromPath path in
  Packages.getPackage ~uri

let declared_symbol_path ~module_name (declared : _ SharedTypes.Declared.t) =
  module_name
  :: SharedTypes.ModulePath.toPath declared.modulePath declared.name.txt

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

let resolve_module_env ~package path =
  match path with
  | [] -> None
  | root_module :: nested_path -> (
    match ProcessCmt.fileForModule root_module ~package with
    | None -> None
    | Some file ->
      let env = QueryEnv.fromFile file in
      match nested_path with
      | [] -> Some env
      | _ ->
        ResolvePath.resolvePath ~env
          ~path:(nested_path @ [placeholder_module_path])
          ~package
        |> Option.map fst)

let resolve_exported_path ~env ~package path =
  let resolve tip find_declared =
    match References.exportedForTip ~env ~path ~package ~tip with
    | None -> None
    | Some (env, _name, stamp) ->
      find_declared env.file.stamps stamp
      |> Option.map (declared_symbol_path ~module_name:env.file.moduleName)
  in
  match path with
  | [] -> Some [env.QueryEnv.file.moduleName]
  | _ -> (
    match resolve Tip.Module Stamps.findModule with
    | Some _ as resolved -> resolved
    | None -> (
      match resolve Tip.Value Stamps.findValue with
      | Some _ as resolved -> resolved
      | None -> resolve Tip.Type Stamps.findType))

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
        let resolved = resolve_module_env ~package path in
        Hashtbl.add resolved_module_cache (path_key path) resolved;
        resolved
    in
    let resolve_item_from_module_prefix item =
      let rec loop prefix_length =
        if prefix_length <= 0 then None
        else
          let module_prefix = take_path prefix_length item in
          match resolve_module_prefix module_prefix with
          | None -> loop (prefix_length - 1)
          | Some env ->
            let remainder = drop_path prefix_length item in
            match remainder with
            | [] -> Some [env.QueryEnv.file.moduleName]
            | _ -> resolve_exported_path ~env ~package remainder
      in
      loop (List.length item)
    in
    let resolve_item item =
      match Hashtbl.find_opt resolved_cache (path_key item) with
      | Some resolved -> resolved
      | None ->
        let resolved =
          resolve_item_from_module_prefix item
          |> Option.value ~default:item
        in
        Hashtbl.add resolved_cache (path_key item) resolved;
        resolved
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

  let summary_of_file path =
    let local_function_bindings = ref StringSet.empty in
    let iterator =
      let open Ast_iterator in
      {
        Ast_iterator.default_iterator with
        expr =
          (fun iter expression ->
            (match expression.pexp_desc with
            | Pexp_let (_rec_flag, bindings, _) ->
              bindings
              |> List.iter (fun (binding : Parsetree.value_binding) ->
                     if is_function_expression binding.pvb_expr then
                       collect_binding_names binding.pvb_pat
                       |> List.iter (fun loc ->
                              local_function_bindings :=
                                StringSet.add (loc_key loc)
                                  !local_function_bindings))
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
    {parse_errors; local_function_bindings = !local_function_bindings}
end

module Typed = struct
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
            |> Option.map (fun declared ->
                   declared_symbol_path ~module_name:env.file.moduleName
                     declared)
          | Tip.Type ->
            Stamps.findType env.file.stamps stamp
            |> Option.map (fun declared ->
                   declared_symbol_path ~module_name:env.file.moduleName
                     declared)
          | Tip.Module ->
            Stamps.findModule env.file.stamps stamp
            |> Option.map (fun declared ->
                   declared_symbol_path ~module_name:env.file.moduleName
                     declared)
          | Tip.Field field_name ->
            Stamps.findType env.file.stamps stamp
            |> Option.map (fun declared ->
                   declared_symbol_path ~module_name:env.file.moduleName
                     declared
                   @ [field_name])
          | Tip.Constructor constructor_name ->
            Stamps.findType env.file.stamps stamp
            |> Option.map (fun declared ->
                   declared_symbol_path ~module_name:env.file.moduleName
                     declared
                   @ [constructor_name]))

  let resolve_local_symbol ~(file : File.t) ~tip stamp =
    match tip with
    | Tip.Value ->
      Stamps.findValue file.stamps stamp
      |> Option.map (fun declared ->
             declared_symbol_path ~module_name:file.moduleName declared)
    | Tip.Type ->
      Stamps.findType file.stamps stamp
      |> Option.map (fun declared ->
             declared_symbol_path ~module_name:file.moduleName declared)
    | Tip.Module ->
      Stamps.findModule file.stamps stamp
      |> Option.map (fun declared ->
             declared_symbol_path ~module_name:file.moduleName declared)
    | Tip.Field field_name ->
      Stamps.findType file.stamps stamp
      |> Option.map (fun declared ->
             declared_symbol_path ~module_name:file.moduleName declared
             @ [field_name])
    | Tip.Constructor constructor_name ->
      Stamps.findType file.stamps stamp
      |> Option.map (fun declared ->
             declared_symbol_path ~module_name:file.moduleName declared
             @ [constructor_name])

  let symbol_path (full : SharedTypes.full) (loc_item : locItem) =
    match loc_item.locType with
    | Typed (_, _typ, LocalReference (stamp, tip)) ->
      resolve_local_symbol ~file:full.file ~tip stamp
    | Typed (_, _typ, GlobalReference (module_name, path, tip)) ->
      resolve_global_symbol ~package:full.package ~module_name ~path ~tip
    | Typed (_, _, (Definition _ | NotFound))
    | LModule _ | TopLevelModule _ | Constant _ | TypeDefinition _ ->
      None

  let forbidden_reference_findings ~config ~path (full : SharedTypes.full) =
    if
      (not config.forbidden_reference.enabled)
      || config.forbidden_reference.items = []
    then []
    else
      full.extra.locItems
      |> List.filter_map (fun loc_item ->
             match symbol_path full loc_item with
             | None -> None
             | Some symbol_path -> (
               match
                 match_forbidden_path config.forbidden_reference.items
                   symbol_path
               with
               | None -> None
               | Some (_item, matched_path) ->
                 let symbol = Some (String.concat "." matched_path) in
                 Some
                   (raw_finding ~rule:"forbidden-reference" ~abs_path:path
                      ~loc:loc_item.loc
                      ~severity:config.forbidden_reference.severity
                      ~message:"Forbidden reference" ?symbol ())))

  let is_function_type typ =
    match (Shared.dig typ).desc with
    | Tarrow _ -> true
    | _ -> false

  let single_use_function_findings ~config ~path ~local_function_bindings
      (full : SharedTypes.full) =
    if not config.single_use_function.enabled then []
    else
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
                  ~loc:declared.name.loc
                  ~severity:config.single_use_function.severity
                  ~message:"Local function is only used once" ?symbol ()
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
  let same_signature left right =
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

let compare_raw_findings left right =
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
  let ast = Ast.summary_of_file path in
  let findings = ref ast.parse_errors in
  (if ast.parse_errors = [] then
     match
       if has_typed_artifact path then Cmt.loadFullCmtFromPath ~path else None
     with
     | None -> ()
     | Some full ->
       findings :=
         Typed.forbidden_reference_findings ~config ~path full
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
        {
          config.forbidden_reference with
          items =
            resolve_forbidden_reference_items ~target_path
              config.forbidden_reference.items;
        };
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

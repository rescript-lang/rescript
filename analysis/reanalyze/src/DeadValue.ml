(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

open DeadCommon

let string_contains ~needle haystack =
  let len_h = String.length haystack and len_n = String.length needle in
  let rec loop i =
    if len_n = 0 then true
    else if i + len_n > len_h then false
    else if String.sub haystack i len_n = needle then true
    else loop (i + 1)
  in
  loop 0

let point_location loc =
  {
    Location.loc_start = loc.Location.loc_start;
    loc_end = loc.Location.loc_start;
    loc_ghost = false;
  }

let record_value_reference collector ~addFileReference ~(locFrom : Location.t)
    ~(locTo : Location.t) ?(target_path = None) () =
  let raw_loc_from = locFrom in
  let locFrom =
    match !Current.lastBinding = Location.none with
    | true -> locFrom
    | false -> !Current.lastBinding
  in
  (match (target_path, Sys.getenv_opt "GRAPH_TRACE_UNKNOWN") with
  | None, Some ("1" | "true" | "on" | "yes") ->
      let from_pos = Common.posToString locFrom.loc_start in
      let to_pos = Common.posToString locTo.loc_start in
      let cross_file =
        not
          (String.equal locFrom.loc_start.pos_fname locTo.loc_start.pos_fname)
      in
      Printf.eprintf
        "[deadvalue] record_ref target_path=<none> cross_file=%b from=%s \
         to=%s\n"
        cross_file from_pos to_pos
  | _ -> ());
  (match (target_path, Sys.getenv_opt "GRAPH_TRACE_UNKNOWN") with
  | Some path, Some ("1" | "true" | "on" | "yes") ->
      let path_str = Common.Path.toString path in
      let should_trace =
        match Sys.getenv_opt "GRAPH_TRACE_VALUE_PATHS" with
        | Some filters when filters <> "" ->
            filters |> String.split_on_char ','
            |> List.exists (fun filter ->
                   let filter = String.trim filter in
                   filter <> "" && string_contains ~needle:filter path_str)
        | _ ->
            String.equal path_str "+CreateErrorHandler1.Error1.+notification"
            || String.equal path_str "+CreateErrorHandler2.Error2.+notification"
            || String.equal path_str "ErrorHandler.Make.notify"
            || String.equal path_str "ErrorHandler.Make.+notify"
            || String.equal path_str "+notify.Make.ErrorHandler"
            || String.equal path_str "+notify.Make.+ErrorHandler"
            || String.equal path_str "+notification.Error.ErrorHandler"
      in
      if should_trace then
        let from_pos = Common.posToString locFrom.loc_start in
        let raw_from_pos = Common.posToString raw_loc_from.loc_start in
        let to_pos = Common.posToString locTo.loc_start in
        Printf.eprintf
          "[deadvalue] trace_ref target=%s from=%s (raw=%s) to=%s addFile=%b\n"
          path_str from_pos raw_from_pos to_pos addFileReference
  | _ -> ());
  if not locFrom.loc_ghost then
    Collector.add_value_reference collector
      {
        loc_from = point_location locFrom;
        loc_to = point_location locTo;
        add_file_reference = addFileReference;
        target_path;
      }

let normalize_interface_file fname =
  if Filename.check_suffix fname ".resi" then
    Filename.chop_suffix fname ".resi" ^ ".res"
  else fname

let canonicalize_position pos =
  let fname = normalize_interface_file pos.Lexing.pos_fname in
  if String.equal fname pos.Lexing.pos_fname then pos
  else {pos with Lexing.pos_fname = fname}

module PendingDeps = struct
  module PosTbl = Hashtbl.Make (struct
    type t = Lexing.position
    let hash pos =
      Hashtbl.hash
        (pos.Lexing.pos_fname, pos.pos_lnum, pos.pos_bol, pos.pos_cnum)
    let equal = ( = )
  end)

  type entry = {
    loc_from: Location.t;
    loc_to: Location.t;
    add_file_ref: bool;
  }

  let table : entry list PosTbl.t = PosTbl.create 64

  let add entry =
    let key = entry.loc_to.Location.loc_start |> canonicalize_position in
    let existing =
      match PosTbl.find_opt table key with Some lst -> lst | None -> []
    in
    PosTbl.replace table key (entry :: existing)

  let resolve key =
    let key = canonicalize_position key in
    match PosTbl.find_opt table key with
    | None -> []
    | Some entries ->
        PosTbl.remove table key;
        entries
end

let record_value_decl collector ?(isToplevel = true)
    ?(optionalArgs = Common.OptionalArgs.empty) ?posStart ?posEnd ~loc
    ~moduleLoc ~path ~sideEffects name =
  let decl =
    Collected_types.Value_decl
      {
        name;
        path;
        loc;
        module_loc = moduleLoc;
        optional_args = optionalArgs;
        side_effects = sideEffects;
        is_toplevel = isToplevel;
        pos_start = posStart;
        pos_end = posEnd;
      }
  in
  (match Sys.getenv_opt "GRAPH_TRACE_UNKNOWN" with
  | Some ("1" | "true" | "on" | "yes") ->
      Printf.eprintf
        "[deadvalue] record_value_decl name=%s loc=%s side=%b isTop=%b\n"
        (Name.toString name)
        (Common.posToString loc.Location.loc_start)
        sideEffects isToplevel
  | _ -> ());
  Collector.add_decl collector decl;
  let common_decl_opt =
    Collected_types.to_common_decl ~current_src:!Common.currentSrc
      ~current_module:!Common.currentModule decl
  in
  (match common_decl_opt with
  | Some common_decl ->
      let resolved =
        PendingDeps.resolve (canonicalize_position common_decl.Common.pos)
      in
      (match resolved with
      | _ :: _ ->
        (match Sys.getenv_opt "GRAPH_TRACE_UNKNOWN" with
        | Some ("1" | "true" | "on" | "yes") ->
          Printf.eprintf
            "[deadvalue] resolved %d pending deps at %s\n"
            (List.length resolved) (Common.posToString common_decl.pos)
        | _ -> ())
      | [] -> ());
      let add_pending_type_reference =
        match common_decl.Common.declKind with
        | Common.DeclKind.VariantCase | RecordLabel ->
            fun entry ->
              DeadType.addTypeReference
                ~posTo:common_decl.Common.posStart
                ~posFrom:entry.PendingDeps.loc_from.Location.loc_start
        | _ -> fun _ -> ()
      in
      resolved
      |> List.iter (fun entry ->
             record_value_reference collector
               ~addFileReference:entry.PendingDeps.add_file_ref
               ~locFrom:entry.PendingDeps.loc_from
               ~locTo:entry.PendingDeps.loc_to
               ~target_path:(Some common_decl.Common.path) ();
             add_pending_type_reference entry)
  | None ->
      (match Sys.getenv_opt "GRAPH_TRACE_UNKNOWN" with
      | Some ("1" | "true" | "on" | "yes") ->
          Printf.eprintf
            "[deadvalue] record_value_decl missing Common decl at %s name=%s src=%s module=%s\n"
            (Common.posToString loc.Location.loc_start)
            (Name.toString name)
            !Common.currentSrc !Common.currentModule
      | _ -> ()))

let collectValueBinding collector super self (vb : Typedtree.value_binding) =
  let oldCurrentBindings = !Current.bindings in
  let oldLastBinding = !Current.lastBinding in
  let loc =
    match vb.vb_pat.pat_desc with
    | Tpat_any when not vb.vb_loc.loc_ghost ->
      let currentModulePath = ModulePath.getCurrent () in
      let path = currentModulePath.path @ [!Common.currentModuleName] in
      let name = Name.create "_" ~isInterface:false in
      let isToplevel = oldLastBinding = Location.none in
      let sideEffects =
        if isToplevel then true else SideEffects.checkExpr vb.vb_expr
      in
      record_value_decl collector ~isToplevel ~loc:vb.vb_loc
        ~moduleLoc:currentModulePath.loc ~path ~sideEffects name;
      vb.vb_loc
    | Tpat_var (id, {loc = {loc_start; loc_ghost} as loc})
    | Tpat_alias
        ({pat_desc = Tpat_any}, id, {loc = {loc_start; loc_ghost} as loc})
      when (not loc_ghost) && not vb.vb_loc.loc_ghost ->
      let name = Ident.name id |> Name.create ~isInterface:false in
      let optionalArgs =
        vb.vb_expr.exp_type |> DeadOptionalArgs.fromTypeExpr
        |> Common.OptionalArgs.fromList
      in
      let exists =
        match Collector.find_decl collector loc_start with
        | Some ({declKind = Value r} as decl) ->
          r.optionalArgs <- optionalArgs;
          Collector.replace_decl collector decl;
          true
        | _ -> false
      in
      let currentModulePath = ModulePath.getCurrent () in
      let path = currentModulePath.path @ [!Common.currentModuleName] in
      let isFirstClassModule =
        match vb.vb_expr.exp_type.desc with
        | Tpackage _ -> true
        | _ -> false
      in
      (if (not exists) && not isFirstClassModule then
         (* This is never toplevel currently *)
         let isToplevel = oldLastBinding = Location.none in
         let sideEffects = SideEffects.checkExpr vb.vb_expr in
         record_value_decl collector ~isToplevel ~loc
           ~moduleLoc:currentModulePath.loc ~optionalArgs ~path ~sideEffects
           name);
      (match Collector.find_decl collector loc_start with
      | None -> ()
      | Some decl ->
        (* Value bindings contain the correct location for the entire declaration: update final position.
           The previous value was taken from the signature, which only has positions for the id. *)
        let declKind =
          match decl.declKind with
          | Value vk ->
            Common.DeclKind.Value
              {vk with sideEffects = SideEffects.checkExpr vb.vb_expr}
          | dk -> dk
        in
        Collector.replace_decl collector
          {decl with declKind; posEnd = vb.vb_loc.loc_end});
      loc
    | Tpat_construct
        ( _,
          ({cstr_name = "()"; cstr_arity = 0; _} : Types.constructor_description),
          _ )
      when not vb.vb_loc.loc_ghost ->
        let currentModulePath = ModulePath.getCurrent () in
        let path = currentModulePath.path @ [!Common.currentModuleName] in
        let sideEffects = true in
        let name = Name.create "_" ~isInterface:false in
        record_value_decl collector ~isToplevel:true ~loc:vb.vb_loc
          ~moduleLoc:currentModulePath.loc ~path ~sideEffects name;
        vb.vb_loc
    | _ -> !Current.lastBinding
  in
  Current.bindings := PosSet.add loc.loc_start !Current.bindings;
  Current.lastBinding := loc;
  let r = super.Tast_mapper.value_binding self vb in
  Current.bindings := oldCurrentBindings;
  Current.lastBinding := oldLastBinding;
  r

let processOptionalArgs ~expType ~(locFrom : Location.t) ~locTo ~path args =
  if expType |> DeadOptionalArgs.hasOptionalArgs then (
    let supplied = ref [] in
    let suppliedMaybe = ref [] in
    args
    |> List.iter (fun (lbl, arg) ->
           let argIsSupplied =
             match arg with
             | Some
                 {
                   Typedtree.exp_desc =
                     Texp_construct (_, {cstr_name = "Some"}, _);
                 } ->
               Some true
             | Some
                 {
                   Typedtree.exp_desc =
                     Texp_construct (_, {cstr_name = "None"}, _);
                 } ->
               Some false
             | Some _ -> None
             | None -> Some false
           in
           match lbl with
           | Asttypes.Optional {txt = s} when not locFrom.loc_ghost ->
             if argIsSupplied <> Some false then supplied := s :: !supplied;
             if argIsSupplied = None then suppliedMaybe := s :: !suppliedMaybe
           | _ -> ());
    (!supplied, !suppliedMaybe)
    |> DeadOptionalArgs.addReferences ~locFrom ~locTo ~path)

let rec collectExpr collector super self (e : Typedtree.expression) =
  let locFrom = e.exp_loc in
  (match e.exp_desc with
  | Texp_ident (path_t, _, {Types.val_loc = {loc_ghost = false; _} as locTo})
    ->
    (* if Path.name _path = "rc" then assert false; *)
    if locFrom = locTo && path_t |> Path.name = "emptyArray" then (
      (* Work around lowercase jsx with no children producing an artifact `emptyArray`
         which is called from its own location as many things are generated on the same location. *)
      if !Common.Cli.debug then
        Log_.item "addDummyReference %s --> %s@."
          (Location.none.loc_start |> Common.posToString)
          (locTo.loc_start |> Common.posToString);
      ValueReferences.add locTo.loc_start Location.none.loc_start)
    else
      let base_path =
        path_t |> Common.Path.fromPathT |> ModulePath.resolveAlias
      in
      let target_path =
        match base_path with
        | [] -> None
        | [_name] ->
            let module_prefix =
              (ModulePath.getCurrent ()).path @ [!Common.currentModuleName]
            in
            Some (module_prefix @ base_path)
        | _ -> Some base_path
      in
      (match (Sys.getenv_opt "GRAPH_TRACE_UNKNOWN", Path.name path_t) with
      | Some ("1" | "true" | "on"), name ->
          let names =
            base_path |> List.map Name.toString |> String.concat "."
          in
          Printf.eprintf "[deadvalue] ident %s base_path=%s\n" name names
      | _ -> ());
      (match target_path with
      | None ->
          record_value_reference collector ~addFileReference:true ~locFrom ~locTo
            ()
      | Some path ->
          record_value_reference collector ~addFileReference:true ~locFrom ~locTo
            ~target_path:(Some path) ())
  | Texp_apply
      {
        funct =
          {
            exp_desc =
              Texp_ident
                (path, _, {Types.val_loc = {loc_ghost = false; _} as locTo});
            exp_type;
          };
        args;
      } ->
    args
    |> processOptionalArgs ~expType:exp_type
         ~locFrom:(locFrom : Location.t)
         ~locTo ~path
  | Texp_let
      ( (* generated for functions with optional args *)
        Nonrecursive,
        [
          {
            vb_pat = {pat_desc = Tpat_var (idArg, _)};
            vb_expr =
              {
                exp_desc =
                  Texp_ident
                    (path, _, {Types.val_loc = {loc_ghost = false; _} as locTo});
                exp_type;
              };
          };
        ],
        {
          exp_desc =
            Texp_function
              {
                case =
                  {
                    c_lhs = {pat_desc = Tpat_var (etaArg, _)};
                    c_rhs =
                      {
                        exp_desc =
                          Texp_apply
                            {
                              funct = {exp_desc = Texp_ident (idArg2, _, _)};
                              args;
                            };
                      };
                  };
              };
        } )
    when Ident.name idArg = "arg"
         && Ident.name etaArg = "eta"
         && Path.name idArg2 = "arg" ->
    args
    |> processOptionalArgs ~expType:exp_type
         ~locFrom:(locFrom : Location.t)
         ~locTo ~path
  | Texp_field
      (_, _, {lbl_loc = {Location.loc_start = posTo; loc_ghost = false}; _}) ->
    if !Config.analyzeTypes then
      DeadType.addTypeReference ~posTo ~posFrom:locFrom.loc_start
  | Texp_construct
      ( _,
        {cstr_loc = {Location.loc_start = posTo; loc_ghost} as locTo; cstr_tag},
        _ ) ->
    (match cstr_tag with
    | Cstr_extension path -> path |> DeadException.markAsUsed ~locFrom ~locTo
    | _ -> ());
    if !Config.analyzeTypes && not loc_ghost then
      DeadType.addTypeReference ~posTo ~posFrom:locFrom.loc_start
  | Texp_record {fields} ->
    fields
    |> Array.iter (fun (_, record_label_definition, _) ->
           match record_label_definition with
           | Typedtree.Overridden (_, ({exp_loc} as e)) when exp_loc.loc_ghost
             ->
             (* Punned field in OCaml projects has ghost location in expression *)
             let e = {e with exp_loc = {exp_loc with loc_ghost = false}} in
            collectExpr collector super self e |> ignore
           | _ -> ())
  | _ -> ());
  super.Tast_mapper.expr self e

(*
  type k. is a locally abstract type
  https://caml.inria.fr/pub/docs/manual-ocaml/locallyabstract.html
  it is required because in ocaml >= 4.11 Typedtree.pattern and ADT is converted
  in a GADT
  https://github.com/ocaml/ocaml/commit/312253ce822c32740349e572498575cf2a82ee96
  in short: all branches of pattern matches aren't the same type.
  With this annotation we declare a new type for each branch to allow the
  function to be typed.
  *)
let collectPattern : _ -> _ -> Typedtree.pattern -> Typedtree.pattern =
 fun super self pat ->
  let posFrom = pat.Typedtree.pat_loc.loc_start in
  (match pat.pat_desc with
  | Typedtree.Tpat_record (cases, _clodsedFlag) ->
    cases
    |> List.iter (fun (_loc, {Types.lbl_loc = {loc_start = posTo}}, _pat, _) ->
           if !Config.analyzeTypes then
             DeadType.addTypeReference ~posFrom ~posTo)
  | _ -> ());
  super.Tast_mapper.pat self pat

let rec getSignature (moduleType : Types.module_type) =
  match moduleType with
  | Mty_signature signature -> signature
  | Mty_functor (_, _mtParam, mt) -> getSignature mt
  | _ -> []

let rec processSignatureItem ~collector ~doTypes ~doValues ~moduleLoc ~path
    (si : Types.signature_item) =
  let oldModulePath = ModulePath.getCurrent () in
  (match si with
  | Sig_type (id, t, _) when doTypes ->
    if !Config.analyzeTypes then
      DeadType.addDeclaration ~typeId:id ~typeKind:t.type_kind
  | Sig_value (id, {Types.val_loc = loc; val_kind = kind; val_type})
    when doValues ->
    if not loc.Location.loc_ghost then
      let isPrimitive =
        match kind with
        | Val_prim _ -> true
        | _ -> false
      in
      if (not isPrimitive) || !Config.analyzeExternals then
        let optionalArgs =
          val_type |> DeadOptionalArgs.fromTypeExpr
          |> Common.OptionalArgs.fromList
        in
        (match Sys.getenv_opt "GRAPH_TRACE_UNKNOWN" with
        | Some ("1" | "true" | "on" | "yes") ->
            Printf.eprintf
              "[deadvalue] sig_value decl name=%s path=%s loc=%s\n"
              (Ident.name id)
              (Common.Path.toString path)
              (Common.posToString loc.loc_start)
        | _ -> ());

        (* if Ident.name id = "someValue" then
           Printf.printf "XXX %s\n" (Ident.name id); *)
        record_value_decl collector ~loc ~moduleLoc ~optionalArgs ~path
             ~sideEffects:false
          (Ident.name id |> Name.create ~isInterface:false)
  | Sig_module (id, {Types.md_type = moduleType; md_loc = moduleLoc}, _)
  | Sig_modtype (id, {Types.mtd_type = Some moduleType; mtd_loc = moduleLoc}) ->
    let module_name = Ident.name id |> Name.create in
    let module_path = module_name :: path in
    let is_module = match si with Sig_module _ -> true | _ -> false in
    let () =
      match Sys.getenv_opt "GRAPH_TRACE_UNKNOWN" with
      | Some ("1" | "true" | "on" | "yes") ->
          Printf.eprintf
            "[deadvalue] sig_%s decl path=%s loc=%s\n"
            (if is_module then "module" else "modtype")
            (Common.Path.toString module_path)
            (Common.posToString moduleLoc.loc_start)
      | _ -> ()
    in
    record_value_decl collector ~isToplevel:true ~path:module_path
      ~loc:moduleLoc ~moduleLoc:moduleLoc ~sideEffects:is_module module_name;
    ModulePath.setCurrent
      {oldModulePath with loc = moduleLoc; path = module_name :: oldModulePath.path};
    getSignature moduleType
    |> List.iter
         (processSignatureItem ~collector ~doTypes ~doValues ~moduleLoc
            ~path:((id |> Ident.name |> Name.create) :: path))
  | _ -> ());
  ModulePath.setCurrent oldModulePath

(* Traverse the AST *)
let traverseStructure ~collector ~doTypes ~doExternals =
  let super = Tast_mapper.default in
  let expr self e = e |> collectExpr collector super self in
  let pat self p = p |> collectPattern super self in
  let value_binding self vb = vb |> collectValueBinding collector super self in
  let structure_item self (structureItem : Typedtree.structure_item) =
    let oldModulePath = ModulePath.getCurrent () in
    let saved_binding_state = ref None in
    (match structureItem.str_desc with
    | Tstr_eval (_expr, _) ->
      if not structureItem.str_loc.loc_ghost then (
        let currentModulePath = ModulePath.getCurrent () in
        let path = currentModulePath.path @ [!Common.currentModuleName] in
        let name = Name.create "_" ~isInterface:false in
        record_value_decl collector ~isToplevel:true ~loc:structureItem.str_loc
          ~moduleLoc:currentModulePath.loc ~path ~sideEffects:true name;
        let oldBindings = !Current.bindings in
        let oldLastBinding = !Current.lastBinding in
        saved_binding_state := Some (oldBindings, oldLastBinding);
        Current.bindings :=
          PosSet.add structureItem.str_loc.loc_start !Current.bindings;
        Current.lastBinding := structureItem.str_loc)
    | Tstr_module {mb_expr; mb_id; mb_loc} -> (
      let hasInterface =
        match mb_expr.mod_desc with
        | Tmod_constraint _ -> true
        | _ -> false
      in
      ModulePath.setCurrent
        {
          oldModulePath with
          loc = mb_loc;
          path = (mb_id |> Ident.name |> Name.create) :: oldModulePath.path;
        };
      let module_name = Ident.name mb_id |> Name.create in
      let module_path =
        module_name :: (ModulePath.getCurrent ()).path
      in
      (match Sys.getenv_opt "GRAPH_TRACE_UNKNOWN" with
      | Some ("1" | "true" | "on" | "yes") ->
          Printf.eprintf
            "[deadvalue] struct_module decl path=%s loc=%s\n"
            (Common.Path.toString module_path)
            (Common.posToString mb_loc.loc_start)
      | _ -> ());
      record_value_decl collector ~isToplevel:true ~path:module_path
        ~loc:structureItem.str_loc ~moduleLoc:mb_loc ~sideEffects:true module_name;
      if hasInterface then
        match mb_expr.mod_type with
        | Mty_signature signature ->
          signature
          |> List.iter
               (processSignatureItem ~collector ~doTypes ~doValues:false
                  ~moduleLoc:mb_expr.mod_loc
                  ~path:
                    ((ModulePath.getCurrent ()).path
                    @ [!Common.currentModuleName]))
        | _ -> ())
    | Tstr_modtype {mtd_id; mtd_type = Some moduleType; mtd_loc} ->
      let module_name = Ident.name mtd_id |> Name.create in
      let module_path = module_name :: (ModulePath.getCurrent ()).path in
      (match Sys.getenv_opt "GRAPH_TRACE_UNKNOWN" with
      | Some ("1" | "true" | "on" | "yes") ->
          Printf.eprintf
            "[deadvalue] struct_modtype decl path=%s loc=%s\n"
            (Common.Path.toString module_path)
            (Common.posToString mtd_loc.loc_start)
      | _ -> ());
      record_value_decl collector ~isToplevel:true ~path:module_path
        ~loc:structureItem.str_loc ~moduleLoc:mtd_loc ~sideEffects:false module_name;
      ModulePath.setCurrent
        {
          oldModulePath with
          loc = mtd_loc;
          path = module_name :: oldModulePath.path;
        };
      getSignature moduleType.mty_type
      |> List.iter
           (processSignatureItem ~collector ~doTypes ~doValues:true
              ~moduleLoc:moduleType.mty_loc ~path:module_path)
    | Tstr_modtype _ -> ()
    | Tstr_primitive vd when doExternals && !Config.analyzeExternals ->
      let currentModulePath = ModulePath.getCurrent () in
      let path = currentModulePath.path @ [!Common.currentModuleName] in
      let exists =
        match Collector.find_decl collector vd.val_loc.loc_start with
        | Some {declKind = Value _} -> true
        | _ -> false
      in
      let id = vd.val_id |> Ident.name in
      Printf.printf "Primitive %s\n" id;
      if
        (not exists) && id <> "unsafe_expr"
        (* see https://github.com/BuckleScript/bucklescript/issues/4532 *)
      then
        record_value_decl collector ~path ~loc:vd.val_loc
             ~moduleLoc:currentModulePath.loc ~sideEffects:false
          (id |> Name.create ~isInterface:false)
    | Tstr_type (_recFlag, typeDeclarations) when doTypes ->
      if !Config.analyzeTypes then
        typeDeclarations
        |> List.iter (fun (typeDeclaration : Typedtree.type_declaration) ->
               DeadType.addDeclaration ~typeId:typeDeclaration.typ_id
                 ~typeKind:typeDeclaration.typ_type.type_kind)
    | Tstr_include {incl_mod; incl_type} -> (
      match incl_mod.mod_desc with
      | Tmod_ident (_path, _lid) ->
        let currentPath =
          (ModulePath.getCurrent ()).path @ [!Common.currentModuleName]
        in
        incl_type
        |> List.iter
             (processSignatureItem ~collector ~doTypes
                ~doValues:false (* TODO: also values? *)
                ~moduleLoc:incl_mod.mod_loc ~path:currentPath)
      | _ -> ())
    | Tstr_exception {ext_id = id; ext_loc = loc} ->
      let path =
        (ModulePath.getCurrent ()).path @ [!Common.currentModuleName]
      in
      let name = id |> Ident.name |> Name.create in
      name
      |> DeadException.add ~collector ~path ~loc ~strLoc:structureItem.str_loc
    | _ -> ());
    let result = super.structure_item self structureItem in
    (match !saved_binding_state with
    | Some (oldBindings, oldLastBinding) ->
        Current.bindings := oldBindings;
        Current.lastBinding := oldLastBinding
    | None -> ());
    ModulePath.setCurrent oldModulePath;
    result
  in
  {super with expr; pat; structure_item; value_binding}

(* Merge a location's references to another one's *)
let processValueDependency collector
    ( ({
         val_loc =
           {loc_start = {pos_fname = fnTo} as posTo; loc_ghost = ghost1} as
           locTo;
       } :
        Types.value_description),
      ({
         val_loc =
           {loc_start = {pos_fname = fnFrom} as posFrom; loc_ghost = ghost2} as
           locFrom;
       } :
        Types.value_description) ) =
  if (not ghost1) && (not ghost2) && posTo <> posFrom then (
    let addFileReference = fileIsImplementationOf fnTo fnFrom in
    let target_decl =
      match Collector.find_decl collector locTo.loc_start with
      | Some decl -> Some decl
      | None -> (
          match DeadCommon.find_decl_at_position locTo.loc_start with
          | Some decl -> Some decl
          | None -> DeadCommon.find_decl_by_line locTo.loc_start)
    in
    (match target_decl with
    | Some decl ->
        record_value_reference collector ~addFileReference ~locFrom ~locTo
          ~target_path:(Some decl.Common.path) ();
        (match decl.Common.declKind with
        | Common.DeclKind.VariantCase | RecordLabel ->
            DeadType.addTypeReference ~posTo:decl.posStart
              ~posFrom:locFrom.loc_start
        | _ -> ())
    | None ->
        PendingDeps.add
          {
            loc_from = locFrom;
            loc_to = locTo;
            add_file_ref = addFileReference;
          };
        (match Sys.getenv_opt "GRAPH_TRACE_UNKNOWN" with
        | Some ("1" | "true" | "on" | "yes") ->
            Printf.eprintf
              "[deadvalue] missing target decl for dependency loc=%s -> %s\n"
              (Common.posToString locFrom.loc_start)
              (Common.posToString locTo.loc_start)
        | _ -> ()));
    DeadOptionalArgs.addFunctionReference ~locFrom ~locTo)

let processStructure ~collector ~cmt_value_dependencies ~doTypes ~doExternals
    (structure : Typedtree.structure) =
  DeadType.with_collector collector (fun () ->
      DeadException.with_collector collector (fun () ->
          DeadOptionalArgs.with_collector collector (fun () ->
              let traverseStructure =
                traverseStructure ~collector ~doTypes ~doExternals
              in
              structure |> traverseStructure.structure traverseStructure
              |> ignore;
  let valueDependencies = cmt_value_dependencies |> List.rev in
              valueDependencies |> List.iter (processValueDependency collector))))

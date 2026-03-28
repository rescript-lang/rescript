open Shared_types

let add_loc_item extra loc loc_type =
  if not loc.Warnings.loc_ghost then
    extra.loc_items <- {loc; loc_type} :: extra.loc_items

let add_reference ~extra stamp loc =
  Hashtbl.replace extra.internal_references stamp
    (loc
    ::
    (if Hashtbl.mem extra.internal_references stamp then
       Hashtbl.find extra.internal_references stamp
     else []))

let extra_for_file ~(file : File.t) =
  let extra = init_extra () in
  file.stamps
  |> Stamps.iter_modules (fun stamp (d : Module.t Declared.t) ->
         add_loc_item extra d.name.loc (LModule (Definition (stamp, Module)));
         add_reference ~extra stamp d.name.loc);
  file.stamps
  |> Stamps.iter_values (fun stamp (d : Types.type_expr Declared.t) ->
         add_loc_item extra d.name.loc
           (Typed (d.name.txt, d.item, Definition (stamp, Value)));
         add_reference ~extra stamp d.name.loc);
  file.stamps
  |> Stamps.iter_types (fun stamp (d : Type.t Declared.t) ->
         add_loc_item extra d.name.loc
           (TypeDefinition (d.name.txt, d.item.Type.decl, stamp));
         add_reference ~extra stamp d.name.loc;
         match d.item.Type.kind with
         | Record labels ->
           labels
           |> List.iter (fun {stamp; fname; typ} ->
                  add_reference ~extra stamp fname.loc;
                  add_loc_item extra fname.loc
                    (Typed
                       (d.name.txt, typ, Definition (d.stamp, Field fname.txt))))
         | Variant constructors ->
           constructors
           |> List.iter (fun {Constructor.stamp; cname} ->
                  add_reference ~extra stamp cname.loc;
                  let t =
                    {
                      Types.id = 0;
                      level = 0;
                      desc =
                        Tconstr
                          ( Path.Pident
                              {Ident.stamp; name = d.name.txt; flags = 0},
                            [],
                            ref Types.Mnil );
                    }
                  in
                  add_loc_item extra cname.loc
                    (Typed
                       ( d.name.txt,
                         t,
                         Definition (d.stamp, Constructor cname.txt) )))
         | _ -> ());
  extra

let add_external_reference ~extra module_name path tip loc =
  (* TODO need to follow the path, and be able to load the files to follow module references... *)
  Hashtbl.replace extra.external_references module_name
    ((path, tip, loc)
    ::
    (if Hashtbl.mem extra.external_references module_name then
       Hashtbl.find extra.external_references module_name
     else []))

let add_file_reference ~extra module_name loc =
  let new_locs =
    match Hashtbl.find_opt extra.file_references module_name with
    | Some old_locs -> Location_set.add loc old_locs
    | None -> Location_set.singleton loc
  in
  Hashtbl.replace extra.file_references module_name new_locs

let handle_constructor txt =
  match txt with
  | Longident.Lident name -> name
  | Ldot (_left, name) -> name

let rec lid_is_complex (lid : Longident.t) =
  match lid with
  | Ldot (lid, _) -> lid_is_complex lid
  | _ -> false

let extra_for_structure_items ~(iterator : Tast_iterator.iterator)
    (items : Typedtree.structure_item list) =
  items |> List.iter (iterator.structure_item iterator)

let extra_for_signature_items ~(iterator : Tast_iterator.iterator)
    (items : Typedtree.signature_item list) =
  items |> List.iter (iterator.signature_item iterator)

let extra_for_cmt ~(iterator : Tast_iterator.iterator)
    ({cmt_annots} : Cmt_format.cmt_infos) =
  let extra_for_parts parts =
    parts
    |> Array.iter (fun part ->
           match part with
           | Cmt_format.Partial_signature str -> iterator.signature iterator str
           | Partial_signature_item str -> iterator.signature_item iterator str
           | Partial_expression expression -> iterator.expr iterator expression
           | Partial_pattern pattern -> iterator.pat iterator pattern
           | Partial_class_expr _ -> ()
           | Partial_module_type module_type ->
             iterator.module_type iterator module_type
           | Partial_structure _ | Partial_structure_item _ -> ())
  in
  match cmt_annots with
  | Implementation structure ->
    extra_for_structure_items ~iterator structure.str_items
  | Partial_implementation parts ->
    let items =
      parts |> Array.to_list
      |> Utils.filter_map (fun (p : Cmt_format.binary_part) ->
             match p with
             | Partial_structure str -> Some str.str_items
             | Partial_structure_item str -> Some [str]
             (* | Partial_expression(exp) => Some([ str]) *)
             | _ -> None)
      |> List.concat
    in
    extra_for_structure_items ~iterator items;
    extra_for_parts parts
  | Interface signature ->
    extra_for_signature_items ~iterator signature.sig_items
  | Partial_interface parts ->
    let items =
      parts |> Array.to_list
      |> Utils.filter_map (fun (p : Cmt_format.binary_part) ->
             match p with
             | Partial_signature s -> Some s.sig_items
             | Partial_signature_item str -> Some [str]
             | _ -> None)
      |> List.concat
    in
    extra_for_signature_items ~iterator items;
    extra_for_parts parts
  | _ -> extra_for_structure_items ~iterator []

let add_for_path ~env ~extra path lident loc typ tip =
  let ident_name = Longident.last lident in
  let ident_loc = Utils.end_of_location loc (String.length ident_name) in
  let loc_type =
    match Resolve_path.from_compiler_path ~env path with
    | Stamp stamp ->
      add_reference ~extra stamp ident_loc;
      LocalReference (stamp, tip)
    | NotFound -> NotFound
    | Global (module_name, path) ->
      add_external_reference ~extra module_name path tip ident_loc;
      GlobalReference (module_name, path, tip)
    | Exported (env, name) -> (
      match
        match tip with
        | Type -> Exported.find env.exported Exported.Type name
        | _ -> Exported.find env.exported Exported.Value name
      with
      | Some stamp ->
        add_reference ~extra stamp ident_loc;
        LocalReference (stamp, tip)
      | None -> NotFound)
    | GlobalMod _ -> NotFound
  in
  add_loc_item extra loc (Typed (ident_name, typ, loc_type))

let add_for_path_parent ~env ~extra path loc =
  let loc_type =
    match Resolve_path.from_compiler_path ~env path with
    | GlobalMod module_name ->
      add_file_reference ~extra module_name loc;
      TopLevelModule module_name
    | Stamp stamp ->
      add_reference ~extra stamp loc;
      LModule (LocalReference (stamp, Module))
    | NotFound -> LModule NotFound
    | Global (module_name, path) ->
      add_external_reference ~extra module_name path Module loc;
      LModule (GlobalReference (module_name, path, Module))
    | Exported (env, name) -> (
      match Exported.find env.exported Exported.Module name with
      | Some stamp ->
        add_reference ~extra stamp loc;
        LModule (LocalReference (stamp, Module))
      | None -> LModule NotFound)
  in
  add_loc_item extra loc loc_type

let get_type_at_path ~env path =
  match Resolve_path.from_compiler_path ~env path with
  | GlobalMod _ -> `Not_found
  | Global (module_name, path) -> `Global (module_name, path)
  | NotFound -> `Not_found
  | Exported (env, name) -> (
    match Exported.find env.exported Exported.Type name with
    | None -> `Not_found
    | Some stamp -> (
      let declared_type = Stamps.find_type env.file.stamps stamp in
      match declared_type with
      | Some declared_type -> `Local declared_type
      | None -> `Not_found))
  | Stamp stamp -> (
    let declared_type = Stamps.find_type env.file.stamps stamp in
    match declared_type with
    | Some declared_type -> `Local declared_type
    | None -> `Not_found)

let add_for_field ~env ~extra ~record_type ~field_type {Asttypes.txt; loc} =
  match (Shared.dig record_type).desc with
  | Tconstr (path, _args, _memo) ->
    let t = get_type_at_path ~env path in
    let name = handle_constructor txt in
    let name_loc = Utils.end_of_location loc (String.length name) in
    let loc_type =
      match t with
      | `Local {stamp; item = {kind = Record fields}} -> (
        match fields |> List.find_opt (fun f -> f.fname.txt = name) with
        | Some {stamp = astamp} ->
          add_reference ~extra astamp name_loc;
          LocalReference (stamp, Field name)
        | None -> NotFound)
      | `Global (module_name, path) ->
        add_external_reference ~extra module_name path (Field name) name_loc;
        GlobalReference (module_name, path, Field name)
      | _ -> NotFound
    in
    add_loc_item extra name_loc (Typed (name, field_type, loc_type))
  | _ -> ()

let add_for_record ~env ~extra ~record_type items =
  match (Shared.dig record_type).desc with
  | Tconstr (path, _args, _memo) ->
    let t = get_type_at_path ~env path in
    items
    |> List.iter (fun ({Asttypes.txt; loc}, _, _, _) ->
           (* let name = Longident.last(txt); *)
           let name = handle_constructor txt in
           let name_loc = Utils.end_of_location loc (String.length name) in
           let loc_type =
             match t with
             | `Local {stamp; item = {kind = Record fields}} -> (
               match fields |> List.find_opt (fun f -> f.fname.txt = name) with
               | Some {stamp = astamp} ->
                 add_reference ~extra astamp name_loc;
                 LocalReference (stamp, Field name)
               | None -> NotFound)
             | `Global (module_name, path) ->
               add_external_reference ~extra module_name path (Field name)
                 name_loc;
               GlobalReference (module_name, path, Field name)
             | _ -> NotFound
           in
           add_loc_item extra name_loc (Typed (name, record_type, loc_type)))
  | _ -> ()

let add_for_constructor ~env ~extra constructor_type {Asttypes.txt; loc}
    {Types.cstr_name} =
  match (Shared.dig constructor_type).desc with
  | Tconstr (path, _args, _memo) ->
    let name = handle_constructor txt in
    let name_loc = Utils.end_of_location loc (String.length name) in
    let t = get_type_at_path ~env path in
    let loc_type =
      match t with
      | `Local {stamp; item = {kind = Variant constructors}} -> (
        match
          constructors
          |> List.find_opt (fun c -> c.Constructor.cname.txt = cstr_name)
        with
        | Some {stamp = cstamp} ->
          add_reference ~extra cstamp name_loc;
          LocalReference (stamp, Constructor name)
        | None -> NotFound)
      | `Global (module_name, path) ->
        add_external_reference ~extra module_name path (Constructor name)
          name_loc;
        GlobalReference (module_name, path, Constructor name)
      | _ -> NotFound
    in
    add_loc_item extra name_loc (Typed (name, constructor_type, loc_type))
  | _ -> ()

let rec add_for_longident ~env ~extra top (path : Path.t) (txt : Longident.t)
    loc =
  if (not loc.Location.loc_ghost) && not (lid_is_complex txt) then (
    let id_length = String.length (String.concat "." (Longident.flatten txt)) in
    let reported_length = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
    let is_ppx = id_length <> reported_length in
    if is_ppx then
      match top with
      | Some (t, tip) -> add_for_path ~env ~extra path txt loc t tip
      | None -> add_for_path_parent ~env ~extra path loc
    else
      let l = Utils.end_of_location loc (String.length (Longident.last txt)) in
      (match top with
      | Some (t, tip) -> add_for_path ~env ~extra path txt l t tip
      | None -> add_for_path_parent ~env ~extra path l);
      match (path, txt) with
      | Pdot (pinner, _pname, _), Ldot (inner, name) ->
        add_for_longident ~env ~extra None pinner inner
          (Utils.chop_location_end loc (String.length name + 1))
      | Pident _, Lident _ -> ()
      | _ -> ())

let rec handle_module_expr ~env ~extra expr =
  match expr with
  | Typedtree.Tmod_constraint (expr, _, _, _) ->
    handle_module_expr ~env ~extra expr.mod_desc
  | Tmod_ident (path, {txt; loc}) ->
    if not (lid_is_complex txt) then
      Log.log ("Ident!! " ^ String.concat "." (Longident.flatten txt));
    add_for_longident ~env ~extra None path txt loc
  | Tmod_functor (_ident, _argName, _maybeType, result_expr) ->
    handle_module_expr ~env ~extra result_expr.mod_desc
  | Tmod_apply (obj, arg, _) ->
    handle_module_expr ~env ~extra obj.mod_desc;
    handle_module_expr ~env ~extra arg.mod_desc
  | _ -> ()

let structure_item ~env ~extra (iter : Tast_iterator.iterator) item =
  (match item.Typedtree.str_desc with
  | Tstr_include {incl_mod = expr} ->
    handle_module_expr ~env ~extra expr.mod_desc
  | Tstr_module {mb_expr} -> handle_module_expr ~env ~extra mb_expr.mod_desc
  | Tstr_open {open_path; open_txt = {txt; loc}} ->
    (* Log.log("Have an open here"); *)
    add_for_longident ~env ~extra None open_path txt loc
  | _ -> ());
  Tast_iterator.default_iterator.structure_item iter item

let signature_item ~(file : File.t) ~extra (iter : Tast_iterator.iterator) item
    =
  (match item.Typedtree.sig_desc with
  | Tsig_value {val_id; val_loc; val_name = name; val_desc; val_attributes} ->
    let stamp = Ident.binding_time val_id in
    if Stamps.find_value file.stamps stamp = None then (
      let declared =
        Process_attributes.new_declared ~name ~stamp ~extent:val_loc
          ~module_path:NotVisible ~item:val_desc.ctyp_type false val_attributes
      in
      Stamps.add_value file.stamps stamp declared;
      add_reference ~extra stamp name.loc;
      add_loc_item extra name.loc
        (Typed (name.txt, val_desc.ctyp_type, Definition (stamp, Value))))
  | _ -> ());
  Tast_iterator.default_iterator.signature_item iter item

let typ ~env ~extra (iter : Tast_iterator.iterator) (item : Typedtree.core_type)
    =
  (match item.ctyp_desc with
  | Ttyp_constr (path, {txt; loc}, _args) ->
    add_for_longident ~env ~extra (Some (item.ctyp_type, Type)) path txt loc
  | _ -> ());
  Tast_iterator.default_iterator.typ iter item

let pat ~(file : File.t) ~env ~extra (iter : Tast_iterator.iterator)
    (pattern : Typedtree.pattern) =
  (* Detect first-class module unpack in a pattern and return the module path
     if present. Used to register a synthetic module declaration *)
  let unpacked_module_path_opt () =
    let has_unpack =
      match
        pattern.pat_extra
        |> List.filter_map (function
             | Typedtree.Tpat_unpack, _, _ -> Some ()
             | _ -> None)
      with
      | _ :: _ -> true
      | [] -> false
    in
    if not has_unpack then None
    else
      match (Shared.dig pattern.pat_type).desc with
      | Tpackage (path, _, _) -> Some path
      | _ -> None
  in
  let add_for_pattern stamp name =
    if Stamps.find_value file.stamps stamp = None then (
      let declared =
        Process_attributes.new_declared ~name ~stamp ~module_path:NotVisible
          ~extent:pattern.pat_loc ~item:pattern.pat_type false
          pattern.pat_attributes
      in
      Stamps.add_value file.stamps stamp declared;
      add_reference ~extra stamp name.loc;
      add_loc_item extra name.loc
        (Typed (name.txt, pattern.pat_type, Definition (stamp, Value))))
  in
  (* Log.log("Entering pattern " ++ Utils.showLocation(pat_loc)); *)
  (match pattern.pat_desc with
  | Tpat_record (items, _, _rest) ->
    add_for_record ~env ~extra ~record_type:pattern.pat_type items
  | Tpat_construct (lident, constructor, _) ->
    add_for_constructor ~env ~extra pattern.pat_type lident constructor
  | Tpat_alias (_inner, ident, name) -> (
    let stamp = Ident.binding_time ident in
    match unpacked_module_path_opt () with
    | Some path ->
      let declared =
        Process_attributes.new_declared ~item:(Module.Ident path)
          ~extent:name.loc ~name ~stamp ~module_path:NotVisible false
          pattern.pat_attributes
      in
      Stamps.add_module file.stamps stamp declared
    | None -> add_for_pattern stamp name)
  | Tpat_var (ident, name) -> (
    (* Log.log("Pattern " ++ name.txt); *)
    let stamp = Ident.binding_time ident in
    match unpacked_module_path_opt () with
    | Some path ->
      let declared =
        Process_attributes.new_declared ~item:(Module.Ident path)
          ~extent:name.loc ~name ~stamp ~module_path:NotVisible false
          pattern.pat_attributes
      in
      Stamps.add_module file.stamps stamp declared
    | None -> add_for_pattern stamp name)
  | _ -> ());
  Tast_iterator.default_iterator.pat iter pattern

let expr ~env ~(extra : extra) (iter : Tast_iterator.iterator)
    (expression : Typedtree.expression) =
  (match expression.exp_desc with
  | Texp_ident (path, {txt; loc}, _) when not (Jsx_hacks.path_is_fragment path)
    ->
    add_for_longident ~env ~extra
      (Some (expression.exp_type, Value))
      path txt loc
  | Texp_record {fields} ->
    add_for_record ~env ~extra ~record_type:expression.exp_type
      (fields |> Array.to_list
      |> Utils.filter_map (fun (desc, item, opt) ->
             match item with
             | Typedtree.Overridden (loc, _) -> Some (loc, desc, (), opt)
             | _ -> None))
  | Texp_constant constant ->
    add_loc_item extra expression.exp_loc (Constant constant)
  (* Skip unit and list literals *)
  | Texp_construct ({txt = Lident ("()" | "::"); loc}, _, _args)
    when loc.loc_end.pos_cnum - loc.loc_start.pos_cnum <> 2 ->
    ()
  | Texp_construct (lident, constructor, _args) ->
    add_for_constructor ~env ~extra expression.exp_type lident constructor
  | Texp_field (inner, lident, _label_description) ->
    add_for_field ~env ~extra ~record_type:inner.exp_type
      ~field_type:expression.exp_type lident
  | _ -> ());
  Tast_iterator.default_iterator.expr iter expression

let get_extra ~file ~infos =
  let extra = extra_for_file ~file in
  let env = Query_env.from_file file in
  let iterator =
    {
      Tast_iterator.default_iterator with
      expr = expr ~env ~extra;
      pat = pat ~env ~extra ~file;
      signature_item = signature_item ~file ~extra;
      structure_item = structure_item ~env ~extra;
      typ = typ ~env ~extra;
    }
  in
  extra_for_cmt ~iterator infos;
  extra

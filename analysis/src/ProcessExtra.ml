open SharedTypes

let addLocItem extra loc locType =
  if not loc.Warnings.loc_ghost then
    extra.locItems <- {loc; locType} :: extra.locItems

let addReference ~extra stamp loc =
  Hashtbl.replace extra.internalReferences stamp
    (loc
    ::
    (if Hashtbl.mem extra.internalReferences stamp then
       Hashtbl.find extra.internalReferences stamp
     else []))

let extraForFile ~(file : File.t) =
  let extra = initExtra () in
  file.stamps
  |> Stamps.iterModules (fun stamp (d : Module.t Declared.t) ->
         addLocItem extra d.name.loc (LModule (Definition (stamp, Module)));
         addReference ~extra stamp d.name.loc);
  file.stamps
  |> Stamps.iterValues (fun stamp (d : Types.type_expr Declared.t) ->
         addLocItem extra d.name.loc
           (Typed (d.name.txt, d.item, Definition (stamp, Value)));
         addReference ~extra stamp d.name.loc);
  file.stamps
  |> Stamps.iterTypes (fun stamp (d : Type.t Declared.t) ->
         addLocItem extra d.name.loc
           (TypeDefinition (d.name.txt, d.item.Type.decl, stamp));
         addReference ~extra stamp d.name.loc;
         match d.item.Type.kind with
         | Record labels ->
           labels
           |> List.iter (fun {stamp; fname; typ} ->
                  addReference ~extra stamp fname.loc;
                  addLocItem extra fname.loc
                    (Typed
                       (d.name.txt, typ, Definition (d.stamp, Field fname.txt))))
         | Variant constructors ->
           constructors
           |> List.iter (fun {Constructor.stamp; cname} ->
                  addReference ~extra stamp cname.loc;
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
                  addLocItem extra cname.loc
                    (Typed
                       ( d.name.txt,
                         t,
                         Definition (d.stamp, Constructor cname.txt) )))
         | _ -> ());
  extra

let addExternalReference ~extra moduleName path tip loc =
  (* TODO need to follow the path, and be able to load the files to follow module references... *)
  Hashtbl.replace extra.externalReferences moduleName
    ((path, tip, loc)
    ::
    (if Hashtbl.mem extra.externalReferences moduleName then
       Hashtbl.find extra.externalReferences moduleName
     else []))

let addFileReference ~extra moduleName loc =
  let newLocs =
    match Hashtbl.find_opt extra.fileReferences moduleName with
    | Some oldLocs -> LocationSet.add loc oldLocs
    | None -> LocationSet.singleton loc
  in
  Hashtbl.replace extra.fileReferences moduleName newLocs

let handleConstructor txt =
  match txt with
  | Longident.Lident name -> name
  | Ldot (_left, name) -> name
  | Lapply (_, _) -> assert false

let rec lidIsComplex (lid : Longident.t) =
  match lid with
  | Lapply _ -> true
  | Ldot (lid, _) -> lidIsComplex lid
  | _ -> false

let extraForStructureItems ~(iterator : Tast_iterator.iterator)
    (items : Typedtree.structure_item list) =
  items |> List.iter (iterator.structure_item iterator)

let extraForSignatureItems ~(iterator : Tast_iterator.iterator)
    (items : Typedtree.signature_item list) =
  items |> List.iter (iterator.signature_item iterator)

let extraForCmt ~(iterator : Tast_iterator.iterator)
    ({cmt_annots} : Cmt_format.cmt_infos) =
  let extraForParts parts =
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
    extraForStructureItems ~iterator structure.str_items
  | Partial_implementation parts ->
    let items =
      parts |> Array.to_list
      |> Utils.filterMap (fun (p : Cmt_format.binary_part) ->
             match p with
             | Partial_structure str -> Some str.str_items
             | Partial_structure_item str -> Some [str]
             (* | Partial_expression(exp) => Some([ str]) *)
             | _ -> None)
      |> List.concat
    in
    extraForStructureItems ~iterator items;
    extraForParts parts
  | Interface signature -> extraForSignatureItems ~iterator signature.sig_items
  | Partial_interface parts ->
    let items =
      parts |> Array.to_list
      |> Utils.filterMap (fun (p : Cmt_format.binary_part) ->
             match p with
             | Partial_signature s -> Some s.sig_items
             | Partial_signature_item str -> Some [str]
             | _ -> None)
      |> List.concat
    in
    extraForSignatureItems ~iterator items;
    extraForParts parts
  | _ -> extraForStructureItems ~iterator []

let addForPath ~env ~extra path lident loc typ tip =
  let identName = Longident.last lident in
  let identLoc = Utils.endOfLocation loc (String.length identName) in
  let locType =
    match ResolvePath.fromCompilerPath ~env path with
    | Stamp stamp ->
      addReference ~extra stamp identLoc;
      LocalReference (stamp, tip)
    | NotFound -> NotFound
    | Global (moduleName, path) ->
      addExternalReference ~extra moduleName path tip identLoc;
      GlobalReference (moduleName, path, tip)
    | Exported (env, name) -> (
      match
        match tip with
        | Type -> Exported.find env.exported Exported.Type name
        | _ -> Exported.find env.exported Exported.Value name
      with
      | Some stamp ->
        addReference ~extra stamp identLoc;
        LocalReference (stamp, tip)
      | None -> NotFound)
    | GlobalMod _ -> NotFound
  in
  addLocItem extra loc (Typed (identName, typ, locType))

let addForPathParent ~env ~extra path loc =
  let locType =
    match ResolvePath.fromCompilerPath ~env path with
    | GlobalMod moduleName ->
      addFileReference ~extra moduleName loc;
      TopLevelModule moduleName
    | Stamp stamp ->
      addReference ~extra stamp loc;
      LModule (LocalReference (stamp, Module))
    | NotFound -> LModule NotFound
    | Global (moduleName, path) ->
      addExternalReference ~extra moduleName path Module loc;
      LModule (GlobalReference (moduleName, path, Module))
    | Exported (env, name) -> (
      match Exported.find env.exported Exported.Module name with
      | Some stamp ->
        addReference ~extra stamp loc;
        LModule (LocalReference (stamp, Module))
      | None -> LModule NotFound)
  in
  addLocItem extra loc locType

let getTypeAtPath ~env path =
  match ResolvePath.fromCompilerPath ~env path with
  | GlobalMod _ -> `Not_found
  | Global (moduleName, path) -> `Global (moduleName, path)
  | NotFound -> `Not_found
  | Exported (env, name) -> (
    match Exported.find env.exported Exported.Type name with
    | None -> `Not_found
    | Some stamp -> (
      let declaredType = Stamps.findType env.file.stamps stamp in
      match declaredType with
      | Some declaredType -> `Local declaredType
      | None -> `Not_found))
  | Stamp stamp -> (
    let declaredType = Stamps.findType env.file.stamps stamp in
    match declaredType with
    | Some declaredType -> `Local declaredType
    | None -> `Not_found)

let addForField ~env ~extra ~recordType ~fieldType {Asttypes.txt; loc} =
  match (Shared.dig recordType).desc with
  | Tconstr (path, _args, _memo) ->
    let t = getTypeAtPath ~env path in
    let name = handleConstructor txt in
    let nameLoc = Utils.endOfLocation loc (String.length name) in
    let locType =
      match t with
      | `Local {stamp; item = {kind = Record fields}} -> (
        match fields |> List.find_opt (fun f -> f.fname.txt = name) with
        | Some {stamp = astamp} ->
          addReference ~extra astamp nameLoc;
          LocalReference (stamp, Field name)
        | None -> NotFound)
      | `Global (moduleName, path) ->
        addExternalReference ~extra moduleName path (Field name) nameLoc;
        GlobalReference (moduleName, path, Field name)
      | _ -> NotFound
    in
    addLocItem extra nameLoc (Typed (name, fieldType, locType))
  | _ -> ()

let addForRecord ~env ~extra ~recordType items =
  match (Shared.dig recordType).desc with
  | Tconstr (path, _args, _memo) ->
    let t = getTypeAtPath ~env path in
    items
    |> List.iter (fun ({Asttypes.txt; loc}, _, _, _) ->
           (* let name = Longident.last(txt); *)
           let name = handleConstructor txt in
           let nameLoc = Utils.endOfLocation loc (String.length name) in
           let locType =
             match t with
             | `Local {stamp; item = {kind = Record fields}} -> (
               match fields |> List.find_opt (fun f -> f.fname.txt = name) with
               | Some {stamp = astamp} ->
                 addReference ~extra astamp nameLoc;
                 LocalReference (stamp, Field name)
               | None -> NotFound)
             | `Global (moduleName, path) ->
               addExternalReference ~extra moduleName path (Field name) nameLoc;
               GlobalReference (moduleName, path, Field name)
             | _ -> NotFound
           in
           addLocItem extra nameLoc (Typed (name, recordType, locType)))
  | _ -> ()

let addForConstructor ~env ~extra constructorType {Asttypes.txt; loc}
    {Types.cstr_name} =
  match (Shared.dig constructorType).desc with
  | Tconstr (path, _args, _memo) ->
    let name = handleConstructor txt in
    let nameLoc = Utils.endOfLocation loc (String.length name) in
    let t = getTypeAtPath ~env path in
    let locType =
      match t with
      | `Local {stamp; item = {kind = Variant constructors}} -> (
        match
          constructors
          |> List.find_opt (fun c -> c.Constructor.cname.txt = cstr_name)
        with
        | Some {stamp = cstamp} ->
          addReference ~extra cstamp nameLoc;
          LocalReference (stamp, Constructor name)
        | None -> NotFound)
      | `Global (moduleName, path) ->
        addExternalReference ~extra moduleName path (Constructor name) nameLoc;
        GlobalReference (moduleName, path, Constructor name)
      | _ -> NotFound
    in
    addLocItem extra nameLoc (Typed (name, constructorType, locType))
  | _ -> ()

let rec addForLongident ~env ~extra top (path : Path.t) (txt : Longident.t) loc
    =
  if (not loc.Location.loc_ghost) && not (lidIsComplex txt) then (
    let idLength = String.length (String.concat "." (Longident.flatten txt)) in
    let reportedLength = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum in
    let isPpx = idLength <> reportedLength in
    if isPpx then
      match top with
      | Some (t, tip) -> addForPath ~env ~extra path txt loc t tip
      | None -> addForPathParent ~env ~extra path loc
    else
      let l = Utils.endOfLocation loc (String.length (Longident.last txt)) in
      (match top with
      | Some (t, tip) -> addForPath ~env ~extra path txt l t tip
      | None -> addForPathParent ~env ~extra path l);
      match (path, txt) with
      | Pdot (pinner, _pname, _), Ldot (inner, name) ->
        addForLongident ~env ~extra None pinner inner
          (Utils.chopLocationEnd loc (String.length name + 1))
      | Pident _, Lident _ -> ()
      | _ -> ())

let rec handle_module_expr ~env ~extra expr =
  match expr with
  | Typedtree.Tmod_constraint (expr, _, _, _) ->
    handle_module_expr ~env ~extra expr.mod_desc
  | Tmod_ident (path, {txt; loc}) ->
    if not (lidIsComplex txt) then
      Log.log ("Ident!! " ^ String.concat "." (Longident.flatten txt));
    addForLongident ~env ~extra None path txt loc
  | Tmod_functor (_ident, _argName, _maybeType, resultExpr) ->
    handle_module_expr ~env ~extra resultExpr.mod_desc
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
    addForLongident ~env ~extra None open_path txt loc
  | _ -> ());
  Tast_iterator.default_iterator.structure_item iter item

let signature_item ~(file : File.t) ~extra (iter : Tast_iterator.iterator) item
    =
  (match item.Typedtree.sig_desc with
  | Tsig_value {val_id; val_loc; val_name = name; val_desc; val_attributes} ->
    let stamp = Ident.binding_time val_id in
    if Stamps.findValue file.stamps stamp = None then (
      let declared =
        ProcessAttributes.newDeclared ~name ~stamp ~extent:val_loc
          ~modulePath:NotVisible ~item:val_desc.ctyp_type false val_attributes
      in
      Stamps.addValue file.stamps stamp declared;
      addReference ~extra stamp name.loc;
      addLocItem extra name.loc
        (Typed (name.txt, val_desc.ctyp_type, Definition (stamp, Value))))
  | _ -> ());
  Tast_iterator.default_iterator.signature_item iter item

let typ ~env ~extra (iter : Tast_iterator.iterator) (item : Typedtree.core_type)
    =
  (match item.ctyp_desc with
  | Ttyp_constr (path, {txt; loc}, _args) ->
    addForLongident ~env ~extra (Some (item.ctyp_type, Type)) path txt loc
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
  let addForPattern stamp name =
    if Stamps.findValue file.stamps stamp = None then (
      let declared =
        ProcessAttributes.newDeclared ~name ~stamp ~modulePath:NotVisible
          ~extent:pattern.pat_loc ~item:pattern.pat_type false
          pattern.pat_attributes
      in
      Stamps.addValue file.stamps stamp declared;
      addReference ~extra stamp name.loc;
      addLocItem extra name.loc
        (Typed (name.txt, pattern.pat_type, Definition (stamp, Value))))
  in
  (* Log.log("Entering pattern " ++ Utils.showLocation(pat_loc)); *)
  (match pattern.pat_desc with
  | Tpat_record (items, _) ->
    addForRecord ~env ~extra ~recordType:pattern.pat_type items
  | Tpat_construct (lident, constructor, _) ->
    addForConstructor ~env ~extra pattern.pat_type lident constructor
  | Tpat_alias (_inner, ident, name) -> (
    let stamp = Ident.binding_time ident in
    match unpacked_module_path_opt () with
    | Some path ->
      let declared =
        ProcessAttributes.newDeclared ~item:(Module.Ident path) ~extent:name.loc
          ~name ~stamp ~modulePath:NotVisible false pattern.pat_attributes
      in
      Stamps.addModule file.stamps stamp declared
    | None -> addForPattern stamp name)
  | Tpat_var (ident, name) -> (
    (* Log.log("Pattern " ++ name.txt); *)
    let stamp = Ident.binding_time ident in
    match unpacked_module_path_opt () with
    | Some path ->
      let declared =
        ProcessAttributes.newDeclared ~item:(Module.Ident path) ~extent:name.loc
          ~name ~stamp ~modulePath:NotVisible false pattern.pat_attributes
      in
      Stamps.addModule file.stamps stamp declared
    | None -> addForPattern stamp name)
  | _ -> ());
  Tast_iterator.default_iterator.pat iter pattern

let expr ~env ~(extra : extra) (iter : Tast_iterator.iterator)
    (expression : Typedtree.expression) =
  (match expression.exp_desc with
  | Texp_ident (path, {txt; loc}, _) when not (JsxHacks.pathIsFragment path) ->
    addForLongident ~env ~extra (Some (expression.exp_type, Value)) path txt loc
  | Texp_record {fields} ->
    addForRecord ~env ~extra ~recordType:expression.exp_type
      (fields |> Array.to_list
      |> Utils.filterMap (fun (desc, item, opt) ->
             match item with
             | Typedtree.Overridden (loc, _) -> Some (loc, desc, (), opt)
             | _ -> None))
  | Texp_constant constant ->
    addLocItem extra expression.exp_loc (Constant constant)
  (* Skip unit and list literals *)
  | Texp_construct ({txt = Lident ("()" | "::"); loc}, _, _args)
    when loc.loc_end.pos_cnum - loc.loc_start.pos_cnum <> 2 ->
    ()
  | Texp_construct (lident, constructor, _args) ->
    addForConstructor ~env ~extra expression.exp_type lident constructor
  | Texp_field (inner, lident, _label_description) ->
    addForField ~env ~extra ~recordType:inner.exp_type
      ~fieldType:expression.exp_type lident
  | _ -> ());
  Tast_iterator.default_iterator.expr iter expression

let getExtra ~file ~infos =
  let extra = extraForFile ~file in
  let env = QueryEnv.fromFile file in
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
  extraForCmt ~iterator infos;
  extra

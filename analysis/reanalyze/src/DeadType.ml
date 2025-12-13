(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

open DeadCommon

module TypeLabels = struct
  (* map from type path (for record/variant label) to its location *)

  let table = (Hashtbl.create 256 : (DcePath.t, Location.t) Hashtbl.t)
  let add path loc = Hashtbl.replace table path loc
  let find path = Hashtbl.find_opt table path
end

let addTypeReference ~config ~refs ~posFrom ~posTo =
  if config.DceConfig.cli.debug then
    Log_.item "addTypeReference %s --> %s@." (posFrom |> Pos.toString)
      (posTo |> Pos.toString);
  References.add_type_ref refs ~posTo ~posFrom

let extendTypeDependencies ~config ~refs (loc1 : Location.t) (loc2 : Location.t)
    =
  let {Location.loc_start = posTo; loc_ghost = ghost1} = loc1 in
  let {Location.loc_start = posFrom; loc_ghost = ghost2} = loc2 in
  if (not ghost1) && (not ghost2) && posTo <> posFrom then (
    if config.DceConfig.cli.debug then
      Log_.item "extendTypeDependencies %s --> %s@." (posTo |> Pos.toString)
        (posFrom |> Pos.toString);
    addTypeReference ~config ~refs ~posFrom ~posTo)

(* Type dependencies between Foo.re and Foo.rei *)
let addTypeDependenciesAcrossFiles ~config ~refs ~file ~pathToType ~loc
    ~typeLabelName =
  let isInterface = file.FileContext.is_interface in
  if not isInterface then (
    let path_1 = pathToType |> DcePath.moduleToInterface in
    let path_2 = path_1 |> DcePath.typeToInterface in
    let path1 = typeLabelName :: path_1 in
    let path2 = typeLabelName :: path_2 in
    match TypeLabels.find path1 with
    | None -> (
      match TypeLabels.find path2 with
      | None -> ()
      | Some loc2 ->
        extendTypeDependencies ~config ~refs loc loc2;
        if not Config.reportTypesDeadOnlyInInterface then
          extendTypeDependencies ~config ~refs loc2 loc)
    | Some loc1 ->
      extendTypeDependencies ~config ~refs loc loc1;
      if not Config.reportTypesDeadOnlyInInterface then
        extendTypeDependencies ~config ~refs loc1 loc)
  else
    let path_1 = pathToType |> DcePath.moduleToImplementation in
    let path1 = typeLabelName :: path_1 in
    match TypeLabels.find path1 with
    | None -> ()
    | Some loc1 ->
      extendTypeDependencies ~config ~refs loc1 loc;
      if not Config.reportTypesDeadOnlyInInterface then
        extendTypeDependencies ~config ~refs loc loc1

(* Add type dependencies between implementation and interface in inner module *)
let addTypeDependenciesInnerModule ~config ~refs ~pathToType ~loc ~typeLabelName
    =
  let path = typeLabelName :: pathToType in
  match TypeLabels.find path with
  | Some loc2 ->
    extendTypeDependencies ~config ~refs loc loc2;
    if not Config.reportTypesDeadOnlyInInterface then
      extendTypeDependencies ~config ~refs loc2 loc
  | None -> TypeLabels.add path loc

let addDeclaration ~config ~decls ~refs ~file ~(modulePath : ModulePath.t)
    ~(typeId : Ident.t) ~(typeKind : Types.type_kind) =
  let pathToType =
    (typeId |> Ident.name |> Name.create)
    :: (modulePath.path @ [FileContext.module_name_tagged file])
  in
  let processTypeLabel ?(posAdjustment = Decl.Nothing) typeLabelName ~declKind
      ~(loc : Location.t) =
    addDeclaration_ ~config ~decls ~file ~declKind ~path:pathToType ~loc
      ~moduleLoc:modulePath.loc ~posAdjustment typeLabelName;
    addTypeDependenciesAcrossFiles ~config ~refs ~file ~pathToType ~loc
      ~typeLabelName;
    addTypeDependenciesInnerModule ~config ~refs ~pathToType ~loc ~typeLabelName;
    TypeLabels.add (typeLabelName :: pathToType) loc
  in
  match typeKind with
  | Type_record (l, _) ->
    List.iter
      (fun {Types.ld_id; ld_loc} ->
        Ident.name ld_id |> Name.create
        |> processTypeLabel ~declKind:RecordLabel ~loc:ld_loc)
      l
  | Type_variant decls ->
    List.iteri
      (fun i {Types.cd_id; cd_loc; cd_args} ->
        let _handle_inline_records =
          match cd_args with
          | Cstr_record lbls ->
            List.iter
              (fun {Types.ld_id; ld_loc} ->
                Ident.name cd_id ^ "." ^ Ident.name ld_id
                |> Name.create
                |> processTypeLabel ~declKind:RecordLabel ~loc:ld_loc)
              lbls
          | Cstr_tuple _ -> ()
        in
        let posAdjustment =
          (* In Res the variant loc can include the | and spaces after it *)
          let isRes =
            let fname = cd_loc.loc_start.pos_fname in
            Filename.check_suffix fname ".res"
            || Filename.check_suffix fname ".resi"
          in
          if isRes then if i = 0 then Decl.FirstVariant else OtherVariant
          else Nothing
        in
        Ident.name cd_id |> Name.create
        |> processTypeLabel ~declKind:VariantCase ~loc:cd_loc ~posAdjustment)
      decls
  | _ -> ()

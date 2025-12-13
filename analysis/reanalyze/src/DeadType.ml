(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

open DeadCommon

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

let addDeclaration ~config ~decls ~file ~(modulePath : ModulePath.t)
    ~(typeId : Ident.t) ~(typeKind : Types.type_kind) =
  let pathToType =
    (typeId |> Ident.name |> Name.create)
    :: (modulePath.path @ [FileContext.module_name_tagged file])
  in
  let processTypeLabel ?(posAdjustment = Decl.Nothing) typeLabelName ~declKind
      ~(loc : Location.t) =
    addDeclaration_ ~config ~decls ~file ~declKind ~path:pathToType ~loc
      ~moduleLoc:modulePath.loc ~posAdjustment typeLabelName
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

module PathMap = Map.Make (struct
  type t = DcePath.t

  let compare = Stdlib.compare
end)

let process_type_label_dependencies ~config ~decls ~refs =
  (* Use raw declaration positions, not [declGetLoc], because references are keyed
     by raw positions (decl.pos). [declGetLoc] applies [posAdjustment] (e.g. +2
     for OtherVariant), which is intended for reporting locations, not for
     reference graph keys. *)
  let decl_raw_loc (decl : Decl.t) : Location.t =
    {Location.loc_start = decl.pos; loc_end = decl.posEnd; loc_ghost = false}
  in
  (* Build an index from full label path -> list of locations *)
  let index =
    Declarations.fold
      (fun _pos decl acc ->
        match decl.Decl.declKind with
        | RecordLabel | VariantCase ->
          let loc = decl |> decl_raw_loc in
          let path = decl.path in
          let existing =
            PathMap.find_opt path acc |> Option.value ~default:[]
          in
          PathMap.add path (loc :: existing) acc
        | _ -> acc)
      decls PathMap.empty
  in
  (* Inner-module duplicates: if the same full path appears multiple times (e.g. from signature+structure),
     connect them together. *)
  index
  |> PathMap.iter (fun _key locs ->
         match locs with
         | [] | [_] -> ()
         | loc0 :: rest ->
           rest
           |> List.iter (fun loc ->
                  extendTypeDependencies ~config ~refs loc loc0;
                  if not Config.reportTypesDeadOnlyInInterface then
                    extendTypeDependencies ~config ~refs loc0 loc));

  (* Cross-file impl<->intf linking, modeled after the previous lookup logic. *)
  let hd_opt = function
    | [] -> None
    | x :: _ -> Some x
  in
  let find_one path =
    match PathMap.find_opt path index with
    | None -> None
    | Some locs -> hd_opt locs
  in

  let is_interface_of_pathToType (pathToType : DcePath.t) =
    match List.rev pathToType with
    | moduleNameTag :: _ -> (
      try (moduleNameTag |> Name.toString).[0] <> '+'
      with Invalid_argument _ -> true)
    | [] -> true
  in

  Declarations.iter
    (fun _pos decl ->
      match decl.Decl.declKind with
      | RecordLabel | VariantCase -> (
        match decl.path with
        | [] -> ()
        | typeLabelName :: pathToType -> (
          let loc = decl |> decl_raw_loc in
          let isInterface = is_interface_of_pathToType pathToType in
          if not isInterface then
            let path_1 = pathToType |> DcePath.moduleToInterface in
            let path_2 = path_1 |> DcePath.typeToInterface in
            let path1 = typeLabelName :: path_1 in
            let path2 = typeLabelName :: path_2 in
            match find_one path1 with
            | Some loc1 ->
              extendTypeDependencies ~config ~refs loc loc1;
              if not Config.reportTypesDeadOnlyInInterface then
                extendTypeDependencies ~config ~refs loc1 loc
            | None -> (
              match find_one path2 with
              | Some loc2 ->
                extendTypeDependencies ~config ~refs loc loc2;
                if not Config.reportTypesDeadOnlyInInterface then
                  extendTypeDependencies ~config ~refs loc2 loc
              | None -> ())
          else
            let path_1 = pathToType |> DcePath.moduleToImplementation in
            let path1 = typeLabelName :: path_1 in
            match find_one path1 with
            | None -> ()
            | Some loc1 ->
              extendTypeDependencies ~config ~refs loc1 loc;
              if not Config.reportTypesDeadOnlyInInterface then
                extendTypeDependencies ~config ~refs loc loc1))
      | _ -> ())
    decls

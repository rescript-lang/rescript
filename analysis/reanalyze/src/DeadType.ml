(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

open Common
open DeadCommon

module TypeLabels = struct
  (* map from type path (for record/variant label) to its location *)

  let table = (Hashtbl.create 256 : (Path.t, Location.t) Hashtbl.t)
  let add path loc = Hashtbl.replace table path loc
  let find path = Hashtbl.find_opt table path
end

let addTypeReference ~posFrom ~posTo =
  if !Common.Cli.debug then
    Log_.item "addTypeReference %s --> %s@." (posFrom |> posToString)
      (posTo |> posToString);
  TypeReferences.add posTo posFrom

module TypeDependencies = struct
  let delayedItems = ref []
  let add loc1 loc2 = delayedItems := (loc1, loc2) :: !delayedItems
  let clear () = delayedItems := []

  let processTypeDependency
      ( ({loc_start = posTo; loc_ghost = ghost1} : Location.t),
        ({loc_start = posFrom; loc_ghost = ghost2} : Location.t) ) =
    if (not ghost1) && (not ghost2) && posTo <> posFrom then
      addTypeReference ~posTo ~posFrom

  let forceDelayedItems () = List.iter processTypeDependency !delayedItems
end

let extendTypeDependencies (loc1 : Location.t) (loc2 : Location.t) =
  if loc1.loc_start <> loc2.loc_start then (
    if !Common.Cli.debug then
      Log_.item "extendTypeDependencies %s --> %s@."
        (loc1.loc_start |> posToString)
        (loc2.loc_start |> posToString);
    TypeDependencies.add loc1 loc2)

(* Type dependencies between Foo.re and Foo.rei *)
let addTypeDependenciesAcrossFiles ~pathToType ~loc ~typeLabelName =
  let isInterface = Filename.check_suffix !Common.currentSrc "i" in
  if not isInterface then (
    let path_1 = pathToType |> Path.moduleToInterface in
    let path_2 = path_1 |> Path.typeToInterface in
    let path1 = typeLabelName :: path_1 in
    let path2 = typeLabelName :: path_2 in
    match TypeLabels.find path1 with
    | None -> (
      match TypeLabels.find path2 with
      | None -> ()
      | Some loc2 ->
        extendTypeDependencies loc loc2;
        if not Config.reportTypesDeadOnlyInInterface then
          extendTypeDependencies loc2 loc)
    | Some loc1 ->
      extendTypeDependencies loc loc1;
      if not Config.reportTypesDeadOnlyInInterface then
        extendTypeDependencies loc1 loc)
  else
    let path_1 = pathToType |> Path.moduleToImplementation in
    let path1 = typeLabelName :: path_1 in
    match TypeLabels.find path1 with
    | None -> ()
    | Some loc1 ->
      extendTypeDependencies loc1 loc;
      if not Config.reportTypesDeadOnlyInInterface then
        extendTypeDependencies loc loc1

(* Add type dependencies between implementation and interface in inner module *)
let addTypeDependenciesInnerModule ~pathToType ~loc ~typeLabelName =
  let path = typeLabelName :: pathToType in
  match TypeLabels.find path with
  | Some loc2 ->
    extendTypeDependencies loc loc2;
    if not Config.reportTypesDeadOnlyInInterface then
      extendTypeDependencies loc2 loc
  | None -> TypeLabels.add path loc

let addDeclaration ~(typeId : Ident.t) ~(typeKind : Types.type_kind) =
  let currentModulePath = ModulePath.getCurrent () in
  let pathToType =
    (typeId |> Ident.name |> Name.create)
    :: (currentModulePath.path @ [!Common.currentModuleName])
  in
  let processTypeLabel ?(posAdjustment = Nothing) typeLabelName ~declKind
      ~(loc : Location.t) =
    addDeclaration_ ~declKind ~path:pathToType ~loc
      ~moduleLoc:currentModulePath.loc ~posAdjustment typeLabelName;
    addTypeDependenciesAcrossFiles ~pathToType ~loc ~typeLabelName;
    addTypeDependenciesInnerModule ~pathToType ~loc ~typeLabelName;
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
          if WriteDeadAnnotations.posLanguage cd_loc.loc_start = Res then
            if i = 0 then FirstVariant else OtherVariant
          else Nothing
        in
        Ident.name cd_id |> Name.create
        |> processTypeLabel ~declKind:VariantCase ~loc:cd_loc ~posAdjustment)
      decls
  | _ -> ()

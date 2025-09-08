open GenTypeCommon

type module_equation = {internal: bool; dep: dep}

type t = {
  mutable map: entry StringMap.t;
  mutable map_module_types: (Typedtree.signature * t) StringMap.t;
  mutable module_equation: module_equation option;
  mutable module_item: Runtime.module_item;
  name: string;
  parent: t option;
  type_equations: type_ StringMap.t;
}

and entry = Module of t | Type of string

let create_type_env ~name parent =
  let module_item = Runtime.new_module_item ~name in
  {
    map = StringMap.empty;
    map_module_types = StringMap.empty;
    module_equation = None;
    module_item;
    name;
    parent;
    type_equations = StringMap.empty;
  }

let root () = None |> create_type_env ~name:"__root__"
let to_string type_env = type_env.name

let new_module ~name type_env =
  if !Debug.type_env then
    Log_.item "TypeEnv.newModule %s %s\n" (type_env |> to_string) name;
  let new_type_env = Some type_env |> create_type_env ~name in
  type_env.map <- type_env.map |> StringMap.add name (Module new_type_env);
  new_type_env

let new_module_type ~name ~signature type_env =
  if !Debug.type_env then
    Log_.item "TypeEnv.newModuleType %s %s\n" (type_env |> to_string) name;
  let new_type_env = Some type_env |> create_type_env ~name in
  type_env.map_module_types <-
    type_env.map_module_types |> StringMap.add name (signature, new_type_env);
  new_type_env

let new_type ~name type_env =
  if !Debug.type_env then
    Log_.item "TypeEnv.newType %s %s\n" (type_env |> to_string) name;
  type_env.map <- type_env.map |> StringMap.add name (Type name)

let get_module ~name type_env =
  match type_env.map |> StringMap.find name with
  | Module type_env1 -> Some type_env1
  | Type _ -> None
  | exception Not_found -> None

let expand_alias_to_external_module ~name type_env =
  match type_env |> get_module ~name with
  | Some {module_equation = Some {internal = false; dep}} ->
    if !Debug.type_env then
      Log_.item "TypeEnv.expandAliasToExternalModule %s %s aliased to %s\n"
        (type_env |> to_string) name (dep |> dep_to_string);
    Some dep
  | _ -> None

let add_module_equation ~dep ~internal type_env =
  if !Debug.type_env then
    Log_.item "Typenv.addModuleEquation %s %s dep:%s\n" (type_env |> to_string)
      (match internal with
      | true -> "Internal"
      | false -> "External")
      (dep |> dep_to_string);
  type_env.module_equation <- Some {internal; dep}

let rec add_type_equation ~flattened ~type_ type_env =
  match flattened with
  | [name] ->
    {
      type_env with
      type_equations = type_env.type_equations |> StringMap.add name type_;
    }
  | module_name :: rest -> (
    match type_env |> get_module ~name:module_name with
    | Some type_env1 ->
      {
        type_env with
        map =
          type_env.map
          |> StringMap.add module_name
               (Module (type_env1 |> add_type_equation ~flattened:rest ~type_));
      }
    | None -> type_env)
  | [] -> type_env

let add_type_equations ~type_equations type_env =
  type_equations
  |> List.fold_left
       (fun te (long_ident, type_) ->
         te
         |> add_type_equation
              ~flattened:(long_ident |> Longident.flatten)
              ~type_)
       type_env

let apply_type_equations ~config ~path type_env =
  match path with
  | Path.Pident id -> (
    match type_env.type_equations |> StringMap.find (id |> Ident.name) with
    | type_ ->
      if !Debug.type_resolution then
        Log_.item "Typenv.applyTypeEquations %s name:%s type_:%s\n"
          (type_env |> to_string) (id |> Ident.name)
          (type_
          |> EmitType.type_to_string ~config ~type_name_is_interface:(fun _ ->
                 false));
      Some type_
    | exception Not_found -> None)
  | _ -> None

let rec lookup ~name type_env =
  match type_env.map |> StringMap.find name with
  | _ -> Some type_env
  | exception Not_found -> (
    match type_env.parent with
    | None -> None
    | Some parent -> parent |> lookup ~name)

let rec lookup_module_type ~path type_env =
  match path with
  | [module_type_name] -> (
    if !Debug.type_env then
      Log_.item "Typenv.lookupModuleType %s moduleTypeName:%s\n"
        (type_env |> to_string) module_type_name;
    match type_env.map_module_types |> StringMap.find module_type_name with
    | x -> Some x
    | exception Not_found -> (
      match type_env.parent with
      | None -> None
      | Some parent -> parent |> lookup_module_type ~path))
  | module_name :: path1 -> (
    if !Debug.type_env then
      Log_.item "Typenv.lookupModuleType %s moduleName:%s\n"
        (type_env |> to_string) module_name;
    match type_env.map |> StringMap.find module_name with
    | Module type_env1 -> type_env1 |> lookup_module_type ~path:path1
    | Type _ -> None
    | exception Not_found -> (
      match type_env.parent with
      | None -> None
      | Some parent -> parent |> lookup_module_type ~path))
  | [] -> None

let rec path_to_list path =
  match path with
  | Path.Pident id -> [id |> Ident.name]
  | Path.Pdot (p, s, _) -> s :: (p |> path_to_list)
  | Path.Papply _ -> []

let lookup_module_type_signature ~path type_env =
  if !Debug.type_env then
    Log_.item "TypeEnv.lookupModuleTypeSignature %s %s\n"
      (type_env |> to_string) (path |> Path.name);
  type_env |> lookup_module_type ~path:(path |> path_to_list |> List.rev)

let update_module_item ~module_item type_env =
  type_env.module_item <- module_item

let rec add_module_path ~type_env name =
  match type_env.parent with
  | None -> name |> ResolvedName.from_string
  | Some parent ->
    type_env.name |> add_module_path ~type_env:parent |> ResolvedName.dot name

let rec get_module_equations type_env : ResolvedName.eq list =
  let sub_equations =
    type_env.map |> StringMap.bindings
    |> List.map (fun (_, entry) ->
           match entry with
           | Module te -> te |> get_module_equations
           | Type _ -> [])
    |> List.concat
  in
  match (type_env.module_equation, type_env.parent) with
  | None, _ | _, None -> sub_equations
  | Some {dep}, Some parent ->
    [
      ( dep |> dep_to_resolved_name,
        type_env.name |> add_module_path ~type_env:parent );
    ]

let get_module_access_path ~name type_env =
  let rec access_path type_env =
    match type_env.parent with
    | None -> Runtime.Root name (* not nested *)
    | Some parent ->
      Dot
        ( (match parent.parent = None with
          | true -> Root type_env.name
          | false -> parent |> access_path),
          type_env.module_item )
  in

  type_env |> access_path

module Name_map = Map.Make (Name)

(* Keep track of the module path while traversing with Tast_mapper *)
type t = {aliases: Dce_path.t Name_map.t; loc: Location.t; path: Dce_path.t}

let initial = ({aliases = Name_map.empty; loc = Location.none; path = []} : t)

let normalize_path ~aliases path =
  match path |> List.rev with
  | name :: rest_rev when rest_rev <> [] -> (
    match aliases |> Name_map.find_opt name with
    | None -> path
    | Some path1 ->
      let new_path = List.rev (path1 @ rest_rev) in
      if !Cli.debug then
        Log_.item "Resolve Alias: %s to %s@."
          (path |> Dce_path.to_string)
          (new_path |> Dce_path.to_string);
      new_path)
  | _ -> path

let add_alias (t : t) ~name ~path : t =
  let aliases = t.aliases in
  let path_normalized = path |> normalize_path ~aliases in
  if !Cli.debug then
    Log_.item "Module Alias: %s = %s@." (name |> Name.to_string)
      (Dce_path.to_string path_normalized);
  {t with aliases = Name_map.add name path_normalized aliases}

let resolve_alias (t : t) path = path |> normalize_path ~aliases:t.aliases

let enter_module (t : t) ~(name : Name.t) ~(loc : Location.t) : t =
  {t with loc; path = name :: t.path}

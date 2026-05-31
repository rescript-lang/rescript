module NameMap = Map.Make (Name)

(* Keep track of the module path while traversing with Tast_mapper *)
type t = {aliases: DcePath.t NameMap.t; loc: Location.t; path: DcePath.t}

let initial = ({aliases = NameMap.empty; loc = Location.none; path = []} : t)

let normalize_path ~aliases path =
  match path |> List.rev with
  | name :: rest_rev when rest_rev <> [] -> (
    match aliases |> NameMap.find_opt name with
    | None -> path
    | Some path1 ->
      let new_path = List.rev (path1 @ rest_rev) in
      if !Cli.debug then
        Log_.item "Resolve Alias: %s to %s@."
          (path |> DcePath.to_string)
          (new_path |> DcePath.to_string);
      new_path)
  | _ -> path

let add_alias (t : t) ~name ~path : t =
  let aliases = t.aliases in
  let path_normalized = path |> normalize_path ~aliases in
  if !Cli.debug then
    Log_.item "Module Alias: %s = %s@." (name |> Name.to_string)
      (DcePath.to_string path_normalized);
  {t with aliases = NameMap.add name path_normalized aliases}

let resolve_alias (t : t) path = path |> normalize_path ~aliases:t.aliases

let enter_module (t : t) ~(name : Name.t) ~(loc : Location.t) : t =
  {t with loc; path = name :: t.path}

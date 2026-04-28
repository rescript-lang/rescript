module NameMap = Map.Make (Name)

(* Keep track of the module path while traversing with Tast_mapper *)
type t = {aliases: DcePath.t NameMap.t; loc: Location.t; path: DcePath.t}

let initial = ({aliases = NameMap.empty; loc = Location.none; path = []} : t)

let normalizePath ~aliases path =
  match path |> List.rev with
  | name :: restRev when restRev <> [] -> (
    match aliases |> NameMap.find_opt name with
    | None -> path
    | Some path1 ->
      let newPath = List.rev (path1 @ restRev) in
      if !Cli.debug then
        Log_.item "Resolve Alias: %s to %s@." (path |> DcePath.toString)
          (newPath |> DcePath.toString);
      newPath)
  | _ -> path

let addAlias (t : t) ~name ~path : t =
  let aliases = t.aliases in
  let pathNormalized = path |> normalizePath ~aliases in
  if !Cli.debug then
    Log_.item "Module Alias: %s = %s@." (name |> Name.toString)
      (DcePath.toString pathNormalized);
  {t with aliases = NameMap.add name pathNormalized aliases}

let resolveAlias (t : t) path = path |> normalizePath ~aliases:t.aliases

let enterModule (t : t) ~(name : Name.t) ~(loc : Location.t) : t =
  {t with loc; path = name :: t.path}

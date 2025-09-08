open Common
module NameMap = Map.Make (Name)

(* Keep track of the module path while traversing with Tast_mapper *)
type t = {aliases: Path.t NameMap.t; loc: Location.t; path: Path.t}

let initial = ({aliases = NameMap.empty; loc = Location.none; path = []} : t)
let current = (ref initial : t ref)
let init () = current := initial

let normalizePath ~aliases path =
  match path |> List.rev with
  | name :: restRev when restRev <> [] -> (
    match aliases |> NameMap.find_opt name with
    | None -> path
    | Some path1 ->
      let newPath = List.rev (path1 @ restRev) in
      if !Common.Cli.debug then
        Log_.item "Resolve Alias: %s to %s@."
          (path |> Common.Path.toString)
          (newPath |> Common.Path.toString);
      newPath)
  | _ -> path

let addAlias ~name ~path =
  let aliases = !current.aliases in
  let pathNormalized = path |> normalizePath ~aliases in
  if !Common.Cli.debug then
    Log_.item "Module Alias: %s = %s@." (name |> Name.toString)
      (Path.toString pathNormalized);
  current := {!current with aliases = NameMap.add name pathNormalized aliases}

let resolveAlias path = path |> normalizePath ~aliases:!current.aliases
let getCurrent () = !current
let setCurrent p = current := p

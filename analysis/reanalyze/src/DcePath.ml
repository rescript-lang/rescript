(** Path representation for dead code analysis.
    A path is a list of names, e.g. [MyModule; myFunction] *)

type t = Name.t list

let to_name (path : t) =
  path |> List.rev_map Name.to_string |> String.concat "." |> Name.create

let to_string path = path |> to_name |> Name.to_string

let without_head path =
  match
    path |> List.rev_map (fun n -> n |> Name.to_interface |> Name.to_string)
  with
  | _ :: tl -> tl |> String.concat "."
  | [] -> ""

let on_ok_path ~when_contains_apply ~f path =
  match path |> Path.flatten with
  | `Ok (id, mods) -> f (Ident.name id :: mods |> String.concat ".")
  | `Contains_apply -> when_contains_apply

let from_path_t path =
  match path |> Path.flatten with
  | `Ok (id, mods) -> Ident.name id :: mods |> List.rev_map Name.create
  | `Contains_apply -> []

let module_to_implementation path =
  match path |> List.rev with
  | module_name :: rest ->
    (module_name |> Name.to_implementation) :: rest |> List.rev
  | [] -> path

let module_to_interface path =
  match path |> List.rev with
  | module_name :: rest -> (module_name |> Name.to_interface) :: rest |> List.rev
  | [] -> path

let to_module_name ~is_type path =
  match path with
  | _ :: tl when not is_type -> tl |> to_name
  | _ :: _ :: tl when is_type -> tl |> to_name
  | _ -> "" |> Name.create

let type_to_interface path =
  match path with
  | type_name :: rest -> (type_name |> Name.to_interface) :: rest
  | [] -> path

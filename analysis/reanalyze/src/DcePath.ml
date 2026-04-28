(** Path representation for dead code analysis.
    A path is a list of names, e.g. [MyModule; myFunction] *)

type t = Name.t list

let toName (path : t) =
  path |> List.rev_map Name.toString |> String.concat "." |> Name.create

let toString path = path |> toName |> Name.toString

let withoutHead path =
  match
    path |> List.rev_map (fun n -> n |> Name.toInterface |> Name.toString)
  with
  | _ :: tl -> tl |> String.concat "."
  | [] -> ""

let onOkPath ~whenContainsApply ~f path =
  match path |> Path.flatten with
  | `Ok (id, mods) -> f (Ident.name id :: mods |> String.concat ".")
  | `Contains_apply -> whenContainsApply

let fromPathT path =
  match path |> Path.flatten with
  | `Ok (id, mods) -> Ident.name id :: mods |> List.rev_map Name.create
  | `Contains_apply -> []

let moduleToImplementation path =
  match path |> List.rev with
  | moduleName :: rest ->
    (moduleName |> Name.toImplementation) :: rest |> List.rev
  | [] -> path

let moduleToInterface path =
  match path |> List.rev with
  | moduleName :: rest -> (moduleName |> Name.toInterface) :: rest |> List.rev
  | [] -> path

let toModuleName ~isType path =
  match path with
  | _ :: tl when not isType -> tl |> toName
  | _ :: _ :: tl when isType -> tl |> toName
  | _ -> "" |> Name.create

let typeToInterface path =
  match path with
  | typeName :: rest -> (typeName |> Name.toInterface) :: rest
  | [] -> path

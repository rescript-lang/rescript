let namespacedName namespace name =
  match namespace with
  | None -> name
  | Some namespace -> name ^ "-" ^ namespace

let ( /+ ) = Filename.concat

let getRuntimeDir rootPath =
  let result =
    ModuleResolution.resolveNodeModulePath ~startPath:rootPath
      "@rescript/runtime"
  in
  match result with
  | Some path -> Some path
  | None ->
    let message = "@rescript/runtime could not be found" in
    Log.log message;
    None

let getLibBs root = Files.ifExists (root /+ "lib" /+ "bs")

let getStdlib base =
  match getRuntimeDir base with
  | None -> None
  | Some runtimeDir -> Some (runtimeDir /+ "lib" /+ "ocaml")

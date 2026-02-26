let namespacedName namespace name =
  match namespace with
  | None -> name
  | Some namespace -> name ^ "-" ^ namespace

let ( /+ ) = Filename.concat

(*
Editor tooling can more accurately resolve the runtime path and will try and pass it via an environment variable.
Example path: "test-stdlib/node_modules/.pnpm/@rescript+runtime@12.0.0-rc.4/node_modules/@rescript/runtime"
*)

let getRuntimeDir rootPath =
  match !Cfg.isDocGenFromCompiler with
  | false -> (
    (* First check RESCRIPT_RUNTIME environment variable, like bsc does *)
    match Sys.getenv_opt "RESCRIPT_RUNTIME" with
    | Some envPath ->
      if Debug.verbose () then
        Printf.printf "[getRuntimeDir] Using RESCRIPT_RUNTIME=%s\n" envPath;
      Some envPath
    | None -> (
      let result =
        ModuleResolution.resolveNodeModulePath ~startPath:rootPath
          "@rescript/runtime"
      in
      match result with
      | Some path ->
        if Debug.verbose () then
          Printf.printf "[getRuntimeDir] Resolved via node_modules: %s\n" path;
        Some path
      | None ->
        let message = "@rescript/runtime could not be found" in
        Log.log message;
        if Debug.verbose () then
          Printf.printf
            "[getRuntimeDir] Failed to resolve @rescript/runtime from \
             rootPath=%s\n"
            rootPath;
        None))
  | true -> Some rootPath

let getLibBs path =
  let bs = path /+ "lib" /+ "bs" in
  let lsp = path /+ "lib" /+ "lsp" in
  let sourcedirs dir = dir /+ ".sourcedirs.json" in
  match (Sys.file_exists (sourcedirs bs), Sys.file_exists (sourcedirs lsp)) with
  | true, true ->
    let mtime f = (Unix.stat f).st_mtime in
    if mtime (sourcedirs lsp) > mtime (sourcedirs bs) then Some lsp else Some bs
  | true, false -> Some bs
  | false, true -> Some lsp
  | false, false -> Files.ifExists bs

let getStdlib base =
  match getRuntimeDir base with
  | None -> None
  | Some runtimeDir -> Some (runtimeDir /+ "lib" /+ "ocaml")

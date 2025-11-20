let namespacedName namespace name =
  match namespace with
  | None -> name
  | Some namespace -> name ^ "-" ^ namespace

let ( /+ ) = Filename.concat

(*
Here is the real problem:

For pnpm it needs to figure out 

"/Users/nojaf/Projects/test-stdlib/node_modules/.pnpm/@rescript+runtime@12.0.0-rc.4/node_modules/@rescript/runtime"
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

let getLibBs path = Files.ifExists (path /+ "lib" /+ "bs")

let getStdlib base =
  match getRuntimeDir base with
  | None -> None
  | Some runtimeDir -> Some (runtimeDir /+ "lib" /+ "ocaml")

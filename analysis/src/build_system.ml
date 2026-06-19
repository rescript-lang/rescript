let namespaced_name namespace name =
  match namespace with
  | None -> name
  | Some namespace -> name ^ "-" ^ namespace

let ( /+ ) = Filename.concat

(*
Editor tooling can more accurately resolve the runtime path and will try and pass it via an environment variable.
Example path: "test-stdlib/node_modules/.pnpm/@rescript+runtime@12.0.0-rc.4/node_modules/@rescript/runtime"

RESCRIPT_RUNTIME is still useful as a fallback for compiler development, but
using it first makes LSP navigation jump into the server checkout instead of the
project's node_modules.
*)

let get_runtime_dir root_path =
  match !Cfg.is_doc_gen_from_compiler with
  | false -> (
    (* First check RESCRIPT_RUNTIME environment variable, like bsc does *)
    match Sys.getenv_opt "RESCRIPT_RUNTIME" with
    | Some env_path ->
      if Debug.verbose () then
        Printf.printf "[getRuntimeDir] Using RESCRIPT_RUNTIME=%s\n" env_path;
      Some env_path
    | None -> (
      let result =
        Module_resolution.resolve_node_module_path ~start_path:root_path
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
            root_path;
        None))
  | true -> Some root_path

let get_lib_bs path = Files.if_exists (path /+ "lib" /+ "bs")

let get_stdlib base =
  match get_runtime_dir base with
  | None -> None
  | Some runtime_dir -> Some (runtime_dir /+ "lib" /+ "ocaml")

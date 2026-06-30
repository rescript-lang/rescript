let namespaced_name namespace name =
  match namespace with
  | None -> name
  | Some namespace -> name ^ "-" ^ namespace

let ( /+ ) = Filename.concat

let get_runtime_dir root_path =
  match !Cfg.is_doc_gen_from_compiler with
  | false -> (
    let result =
      Module_resolution.resolve_node_module_path ~start_path:root_path
        "@rescript/runtime"
    in
    match result with
    | Some path -> Some path
    | None ->
      let message = "@rescript/runtime could not be found" in
      Log.log message;
      None)
  | true -> Some root_path

let get_lib_bs path = Files.if_exists (path /+ "lib" /+ "bs")

let get_stdlib base =
  match get_runtime_dir base with
  | None -> None
  | Some runtime_dir -> Some (runtime_dir /+ "lib" /+ "ocaml")

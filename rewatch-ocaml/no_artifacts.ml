let enabled = Sys.getenv_opt "RESCRIPT_NO_ARTIFACTS" = Some "1"

let extensions = [".ast"; ".iast"; ".cmi"; ".cmj"; ".cmt"; ".cmti"; ".mlmap"]

(* Keyed by Build_file_store.key. Changed only on the main domain while no
   request runs, and read by the requests' domains. *)
let artifact_dirs : (string, unit) Hashtbl.t = Hashtbl.create 16

let register_artifact_dir directory =
  Hashtbl.replace artifact_dirs (Build_file_store.key directory) ()

(* Only the artifact directories of the packages being built: the runtime's
   shipped interfaces are read from disk. *)
let is_artifact key =
  List.exists (Filename.check_suffix key) extensions
  && Hashtbl.mem artifact_dirs (Filename.dirname key)

let install () = if enabled then Build_file_store.set_memory_only is_artifact

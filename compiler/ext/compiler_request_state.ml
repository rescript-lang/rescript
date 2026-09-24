type t = {
  active: bool;
  cwd: string;
  mutable input_name: string;
  mutable command_runner: string -> int;
  mutable cmt_args: string array;
  mutable lambda_raise_count: int;
  mutable lambda_negative_raise_count: int;
  mutable builtin_ppx_local_module_counter: int;
  mutable substitution_saved_id: int;
  mutable type_node_id: int;
  mutable type_node_reset_id: int option;
  mutable type_node_last_snapshot: int;
  mutable runtime_path_override: string option;
  mutable project_root: string option;
  mutable load_path: string list;
}

let create ~active cwd =
  {
    active;
    cwd;
    input_name = "_none_";
    command_runner = Sys.command;
    cmt_args = Sys.argv;
    lambda_raise_count = 0;
    lambda_negative_raise_count = 0;
    builtin_ppx_local_module_counter = 0;
    substitution_saved_id = -1;
    type_node_id = -1;
    type_node_reset_id = None;
    type_node_last_snapshot = 0;
    runtime_path_override = None;
    project_root = None;
    load_path = [];
  }

let key = Domain.DLS.new_key (fun () -> create ~active:false (Sys.getcwd ()))
let current () = Domain.DLS.get key
let cwd () =
  let state = current () in
  if state.active then state.cwd else Sys.getcwd ()

let resolve_path path =
  let state = current () in
  if state.active && Filename.is_relative path then
    Filename.concat state.cwd path
  else path

let with_fresh ?cwd:requested_cwd action =
  let previous = current () in
  let cwd =
    match requested_cwd with
    | None -> cwd ()
    | Some path when Filename.is_relative path -> Filename.concat (cwd ()) path
    | Some path -> path
  in
  Domain.DLS.set key (create ~active:true cwd);
  Fun.protect action ~finally:(fun () -> Domain.DLS.set key previous)

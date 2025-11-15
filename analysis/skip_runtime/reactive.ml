external init_memory : int -> string -> unit = "init_memory"
external protect_memory_ro : unit -> unit = "protect_memory_ro"
external protect_memory_rw : unit -> unit = "protect_memory_rw"
external skip_setup_local : unit -> unit = "oskip_setup_local"
external skip_exists_input_dir : string -> int = "oskip_exists_input_dir"
external skip_create_input_dir : string -> string array -> unit
  = "oskip_create_input_dir"
external skip_write : string -> string -> unit = "oskip_write"
external skip_unsafe_get_array : string -> string -> 'a array
  = "oskip_unsafe_get_array"
external skip_get_array : string -> string -> 'a array = "oskip_get_array"
external skip_prepare_map : string -> string -> int = "oskip_prepare_map"
external skip_process_element :
  (string -> (string * 'a array) array) -> int
  = "oskip_process_element"
external skip_map : string -> string -> string -> string -> unit = "oskip_map"
external skip_exit : unit -> unit = "oskip_exit"
external skip_union : string -> string -> string -> unit = "oskip_union"
external skip_set_file : string -> string -> unit = "oskip_set_file"
external skip_remove_file : string -> string -> unit = "oskip_remove_file"
external skip_get_files : string -> string array = "oskip_get_files"

exception Memory_write_violation

let has_exited = ref false
let toplevel = ref true
let has_been_initialized = ref false

let init file_name dataSize =
  has_been_initialized := true;
  Callback.register_exception "memory_write_violation" Memory_write_violation;
  init_memory dataSize file_name

let fork_n_processes2 _n closure =
  if not !has_been_initialized then
    failwith "Reactive has not been initialized";
  if !has_exited then failwith "Reactive has exited";
  toplevel := false;
  protect_memory_ro ();
  skip_setup_local ();
  Fun.protect
    ~finally:(fun () -> toplevel := true)
    (fun () ->
      while skip_process_element closure <> 0 do
        ()
      done)

let fork_n_processes n closure =
  if not !has_been_initialized then
    failwith "Reactive has not been initialized";
  if not !toplevel then failwith "Calling map inside a map";
  if !has_exited then failwith "Reactive has exited";
  toplevel := false;
  let rec fork_loop i acc =
    if i >= n then acc
    else
      let pid = Unix.fork () in
      if pid = 0 then (
        protect_memory_ro ();
        skip_setup_local ();
        while skip_process_element closure <> 0 do
          if Unix.getppid () = 1 then (
            Printf.eprintf "Parent died, child exiting\n%!";
            exit 1
          )
        done;
        exit 0)
      else fork_loop (i + 1) (pid :: acc)
  in
  let child_pids = fork_loop 0 [] in
  List.iter
    (fun _ ->
      match snd (Unix.wait ()) with
      | Unix.WEXITED code when code = 0 -> ()
      | Unix.WEXITED code ->
          Printf.eprintf "Error in child: exited with code %d\n" code;
          exit 2
      | Unix.WSIGNALED signal ->
          Printf.eprintf "Error in child: killed by signal %d\n" signal;
          exit 2
      | Unix.WSTOPPED signal ->
          Printf.eprintf "Error in child: stopped by signal %d\n" signal;
          exit 2)
    child_pids;
  toplevel := true

let () =
  if false then (
    protect_memory_rw ();
    skip_write "" "";
    let dummy_closure : string -> (string * string array) array =
      fun _ -> [||]
    in
    fork_n_processes2 0 dummy_closure
  )

type 'a t = string
type filename = string
type key = string
type tracker = int
type 'a marshalled = string

let read_file filename _tracker =
  if !toplevel then
    if !has_exited then ()
    else failwith "Cannot read a file at toplevel before exit";
  let ic = open_in_bin filename in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

exception Can_only_call_map_at_toplevel

let make_collection_name =
  let count = ref 0 in
  fun name ->
    incr count;
    "/col" ^ string_of_int !count ^ "/" ^ name ^ "/"

let map collection f =
  if !has_exited then failwith "Reactive has exited";
  if not !has_been_initialized then
    failwith "Reactive has not been initialized";
  if not !toplevel then raise Can_only_call_map_at_toplevel;
  let prepare_collection = make_collection_name "prepare" in
  let nbr_procs = skip_prepare_map collection prepare_collection in
  if nbr_procs <> 0 then
    fork_n_processes nbr_procs (fun x ->
        let values = skip_unsafe_get_array collection x in
        f x values);
  let map_collection = make_collection_name "map" in
  let dedup_collection = make_collection_name "dedup" in
  skip_map collection prepare_collection map_collection dedup_collection;
  dedup_collection

let marshalled_map collection f =
  map collection (fun key values ->
      let result = f key values in
      Array.map
        (fun (key, values) ->
          let values =
            Array.map
              (fun x -> Marshal.to_string x [Marshal.Closures])
              values
          in
          (key, values))
        result)

let unmarshal x = Marshal.from_string x 0

let diff_sorted_arrays_iter (a : string array) (b : string array) f =
  let len_a = Array.length a in
  let len_b = Array.length b in
  let i = ref 0 in
  let j = ref 0 in
  while !i < len_a && !j < len_b do
    match String.compare a.(!i) b.(!j) with
    | 0 ->
        incr i;
        incr j
    | c when c < 0 ->
        f a.(!i);
        incr i
    | _ -> incr j
  done;
  while !i < len_a do
    f a.(!i);
    incr i
  done

let check_sorted (arr : string array) =
  let len = Array.length arr in
  for i = 0 to len - 2 do
    if String.compare arr.(i) arr.(i + 1) > 0 then
      failwith "Expected sorted array"
  done

let input_files file_names =
  if !has_exited then failwith "Reactive has exited";
  if not !has_been_initialized then
    failwith "Reactive has not been initialized";
  let input_collection = make_collection_name "input" in
  if skip_exists_input_dir input_collection = 0 then
    skip_create_input_dir input_collection file_names
  else (
    let all = skip_get_files input_collection in
    check_sorted all;
    Array.sort String.compare file_names;
    diff_sorted_arrays_iter all file_names (fun file ->
        skip_remove_file input_collection file);
    Array.iter
      (fun file_name -> skip_set_file input_collection file_name)
      file_names);
  input_collection

exception Toplevel_get_array

let get_array collection key =
  if not !has_been_initialized then
    failwith "Reactive has not been initialized";
  if !has_exited then skip_unsafe_get_array collection key
  else if !toplevel then raise Toplevel_get_array
  else skip_get_array collection key

let union col1 col2 =
  if !has_exited then failwith "Reactive has exited";
  if not !has_been_initialized then
    failwith "Reactive has not been initialized";
  let name = col1 ^ "union" ^ col2 in
  skip_union col1 col2 name;
  name

let exit () =
  if not !has_been_initialized then
    failwith "Reactive has not been initialized";
  if !has_exited then (
    Printf.fprintf stderr "Error: double call to Reactive.exit";
    exit 2);
  has_exited := true;
  skip_exit ();
  protect_memory_ro ()


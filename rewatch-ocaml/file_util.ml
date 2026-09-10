let path_of_parts root parts = List.fold_left Filename.concat root parts

let ensure_dir path =
  (* OCaml's standard library has no recursive directory-creation primitive.
     Another process may win a mkdir race, but EEXIST is success only when the
     resulting path is a directory. Filename.dirname may return an unavailable
     Windows volume root unchanged, so stop at that fixed point and let mkdir
     surface the native filesystem error. *)
  let is_directory path =
    try (Unix.stat path).Unix.st_kind = Unix.S_DIR
    with Sys_error _ | Unix.Unix_error _ -> false
  in
  let mkdir path =
    try Unix.mkdir path 0o755
    with Unix.Unix_error (Unix.EEXIST, _, _) as error ->
      if not (is_directory path) then raise error
  in
  let rec loop path =
    if path = "" || path = "." || is_directory path then ()
    else
      let parent = Filename.dirname path in
      if parent = path then mkdir path
      else (
        loop parent;
        mkdir path)
  in
  loop path

let read_file path =
  let channel = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr channel) (fun () ->
    really_input_string channel (in_channel_length channel))

(* Callers that already created the destination directory may skip that work,
   avoiding repeated metadata probes when publishing many files. *)
let copy_existing_file ?(ensure_parent = true) source destination =
  if ensure_parent then ensure_dir (Filename.dirname destination);
  let input = open_in_bin source in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () ->
      let output = open_out_bin destination in
      Fun.protect
        ~finally:(fun () -> close_out_noerr output)
        (fun () ->
          really_input_string input (in_channel_length input)
          |> output_string output))

let copy_optional_existing_file ?(ensure_parent = true) source destination =
  try copy_existing_file ~ensure_parent source destination
  with (Sys_error _ | Unix.Unix_error _) as error ->
    let source_is_missing =
      try
        ignore (Unix.stat source);
        false
      with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> true
    in
    if source_is_missing then
      try Sys.remove destination with Sys_error _ -> ()
    else raise error

let copy_file source destination =
  if Sys.file_exists source then copy_existing_file source destination

let stat_opt path =
  try Some (Unix.stat path)
  with Sys_error _ | Unix.Unix_error _ -> None

let files_equal first second =
  match stat_opt first with
  | None -> false
  | Some first_stat -> (
    match stat_opt second with
    | None -> false
    | Some second_stat ->
      first_stat.Unix.st_size = second_stat.Unix.st_size
      && let first_channel = open_in_bin first in
         Fun.protect
           ~finally:(fun () -> close_in_noerr first_channel)
           (fun () ->
             let second_channel = open_in_bin second in
             Fun.protect
               ~finally:(fun () -> close_in_noerr second_channel)
               (fun () ->
                 let buffer_size = 65_536 in
                 let first_buffer = Bytes.create buffer_size in
                 let second_buffer = Bytes.create buffer_size in
                 let rec loop () =
                   let first_count =
                     input first_channel first_buffer 0 buffer_size
                   in
                   let second_count =
                     input second_channel second_buffer 0 buffer_size
                   in
                   first_count = second_count
                   && (first_count = 0
                      || (Bytes.sub first_buffer 0 first_count
                          = Bytes.sub second_buffer 0 second_count
                         && loop ()))
                 in
                 loop ())))

let copy_file_if_changed ?(ensure_parent = true) source destination =
  if not (files_equal source destination) then
    if ensure_parent then copy_file source destination
    else copy_existing_file ~ensure_parent:false source destination

let modification_time path =
  stat_opt path |> Option.map (fun metadata -> metadata.Unix.st_mtime)

let remove_file path =
  if Sys.file_exists path then (try Sys.remove path with Sys_error _ -> ())

let rec remove_tree path =
  try
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
    | _ -> Sys.remove path
  with Sys_error _ | Unix.Unix_error _ -> ()

let rec files_under directory =
  try
    match (Unix.lstat directory).Unix.st_kind with
    | Unix.S_DIR ->
      Sys.readdir directory |> Array.to_list
      |> List.concat_map (fun name ->
           files_under (Filename.concat directory name))
    (* Following links during recursion could leave the requested tree or enter
       a cycle. Follow one only to omit dangling links from the result. *)
    | Unix.S_LNK when not (Sys.file_exists directory) -> []
    | _ -> [directory]
  with Sys_error _ | Unix.Unix_error _ -> []

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
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let with_output_channel channel write =
  try
    let result = write channel in
    close_out channel;
    result
  with exn ->
    close_out_noerr channel;
    raise exn

let write_file path contents =
  let channel = open_out_bin path in
  with_output_channel channel (fun channel -> output_string channel contents)

let append_file path contents =
  let channel =
    open_out_gen [Open_wronly; Open_append; Open_binary] 0o644 path
  in
  with_output_channel channel (fun channel -> output_string channel contents)

let restore_after_exception restore_signals exn =
  let exn =
    try
      restore_signals ();
      exn
    with signal_exn -> signal_exn
  in
  raise exn

let write_file_atomic ?(ensure_parent = true) ?perm path contents =
  if ensure_parent then ensure_dir (Filename.dirname path);
  (* A temporary file must have a cleanup owner before a watch signal can
     interrupt the command. Publishing is also signal-deferred so the final
     path always names either the previous complete file or the replacement. *)
  let restore_creation_signals = Platform.defer_termination_signals () in
  let temporary = ref None in
  let remove_temporary path =
    try Sys.remove path with Sys_error _ | Unix.Unix_error _ -> ()
  in
  let perm =
    match perm with
    | Some _ as perm -> perm
    | None -> (
      try Some (Unix.stat path).Unix.st_perm
      with Sys_error _ | Unix.Unix_error _ -> None)
  in
  try
    let candidate =
      Filename.temp_file ~temp_dir:(Filename.dirname path) ".rewatch-write-"
        ".tmp"
    in
    temporary := Some candidate;
    Fun.protect
      ~finally:(fun () -> Option.iter remove_temporary !temporary)
      (fun () ->
        restore_creation_signals ();
        Option.iter (Unix.chmod candidate) perm;
        write_file candidate contents;
        let restore_publish_signals = Platform.defer_termination_signals () in
        try
          Sys.rename candidate path;
          temporary := None;
          restore_publish_signals ()
        with exn -> restore_after_exception restore_publish_signals exn)
  with exn ->
    Option.iter remove_temporary !temporary;
    restore_after_exception restore_creation_signals exn

(* Callers that already created the destination directory may skip that work,
   avoiding repeated metadata probes when publishing many files. *)
let copy_existing_file ?(ensure_parent = true) source destination =
  if ensure_parent then ensure_dir (Filename.dirname destination);
  let input_channel = open_in_bin source in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input_channel)
    (fun () ->
      let output_channel = open_out_bin destination in
      with_output_channel output_channel (fun output_channel ->
          let buffer = Bytes.create 65_536 in
          let rec copy () =
            let count = input input_channel buffer 0 (Bytes.length buffer) in
            if count > 0 then (
              output output_channel buffer 0 count;
              copy ())
          in
          copy ()))

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
  try Some (Unix.stat path) with Sys_error _ | Unix.Unix_error _ -> None

let files_equal first second =
  match stat_opt first with
  | None -> false
  | Some first_stat -> (
    match stat_opt second with
    | None -> false
    | Some second_stat ->
      first_stat.Unix.st_size = second_stat.Unix.st_size
      &&
      let first_channel = open_in_bin first in
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
              let rec read_chunk channel buffer offset =
                if offset = Bytes.length buffer then offset
                else
                  match
                    input channel buffer offset (Bytes.length buffer - offset)
                  with
                  | 0 -> offset
                  | count -> read_chunk channel buffer (offset + count)
              in
              let equal_prefix length =
                let index = ref 0 in
                while
                  !index < length
                  && Bytes.get first_buffer !index
                     = Bytes.get second_buffer !index
                do
                  incr index
                done;
                !index = length
              in
              let rec loop () =
                let first_count = read_chunk first_channel first_buffer 0 in
                let second_count = read_chunk second_channel second_buffer 0 in
                first_count = second_count
                && (first_count = 0 || (equal_prefix first_count && loop ()))
              in
              loop ())))

let copy_file_if_different ?(ensure_parent = true) source destination =
  let changed = not (files_equal source destination) in
  if changed then
    if ensure_parent then copy_file source destination
    else copy_existing_file ~ensure_parent:false source destination;
  changed

let copy_file_if_changed ?ensure_parent source destination =
  ignore (copy_file_if_different ?ensure_parent source destination)

let modification_time path =
  stat_opt path |> Option.map (fun metadata -> metadata.Unix.st_mtime)

let remove_file path =
  try Unix.unlink path
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ()

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
    Sys.readdir path
    |> Array.iter (fun name -> remove_tree (Filename.concat path name));
    Unix.rmdir path
  | _ -> Unix.unlink path
  | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ()

let remove_file_best_effort path =
  try remove_file path with Sys_error _ | Unix.Unix_error _ -> ()

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

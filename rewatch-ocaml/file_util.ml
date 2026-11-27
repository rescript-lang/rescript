let path_of_parts root parts = List.fold_left Filename.concat root parts

let is_directory path =
  match Unix.stat path with
  | metadata -> metadata.Unix.st_kind = Unix.S_DIR
  | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> false

let ensure_dir path =
  (* OCaml's standard library has no recursive directory-creation primitive.
     Another process may win a mkdir race, but EEXIST is success only when the
     resulting path is a directory. Filename.dirname may return an unavailable
     Windows volume root unchanged, so stop at that fixed point and let mkdir
     surface the native filesystem error. *)
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

let open_for_reading path =
  (* Windows denies deleting or renaming a file while any handle lacks
     FILE_SHARE_DELETE, and Unix.openfile only adds it on request. Without it
     the watcher's periodic watch.lock read can make the shutdown protocol's
     rm fail, and hashing a source can block an editor's atomic rename. *)
  Unix.openfile path [Unix.O_RDONLY; Unix.O_CLOEXEC; Unix.O_SHARE_DELETE] 0

let read_file path =
  let descriptor = open_for_reading path in
  let channel = Unix.in_channel_of_descr descriptor in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let expected_size = (Unix.fstat descriptor).Unix.st_size in
      let contents = Buffer.create (max 16 expected_size) in
      (* A small initial chunk avoids allocating 64 KiB for tiny files such as
         the watch lock. The loop still handles streams and files that grow
         while they are being read. *)
      let chunk = Bytes.create (max 1 (min 65536 (max 4096 expected_size))) in
      let rec read () =
        match input channel chunk 0 (Bytes.length chunk) with
        | 0 -> Buffer.contents contents
        | count ->
          Buffer.add_subbytes contents chunk 0 count;
          read ()
      in
      read ())

let is_regular_file path =
  match Unix.stat path with
  | metadata -> metadata.Unix.st_kind = Unix.S_REG
  | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> false

let digest_file path =
  let descriptor = open_for_reading path in
  let channel = Unix.in_channel_of_descr descriptor in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> Digest.channel channel (-1))

let with_output_channel channel write =
  try
    let result = write channel in
    close_out channel;
    result
  with exn ->
    close_out_noerr channel;
    raise exn

let write_file path contents =
  Build_file_store.forget path;
  let channel = open_out_bin path in
  with_output_channel channel (fun channel -> output_string channel contents)

let append_file path contents =
  let channel =
    open_out_gen [Open_wronly; Open_append; Open_binary] 0o644 path
  in
  with_output_channel channel (fun channel -> output_string channel contents)

let directory_entries path =
  let directory = Unix.opendir path in
  Fun.protect
    ~finally:(fun () -> Unix.closedir directory)
    (fun () ->
      let rec read entries =
        match Unix.readdir directory with
        | "." | ".." -> read entries
        | name -> read (name :: entries)
        | exception End_of_file -> List.rev entries
      in
      read [])

let atomic_write_prefix = ".rewatch-write-"

let temporary_names = Domain.DLS.new_key Random.State.make_self_init

(* Creates the temporary file under a random name, as Filename.temp_file
   does, but keeps its descriptor open for writing instead of closing it. *)
let open_temporary_file directory =
  let rec attempt count =
    let name =
      Printf.sprintf "%s%06x.tmp" atomic_write_prefix
        (Random.State.bits (Domain.DLS.get temporary_names) land 0xFFFFFF)
    in
    let path = Filename.concat directory name in
    match
      Unix.openfile path
        [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC]
        0o644
    with
    | descriptor -> (path, descriptor)
    | exception Unix.Unix_error (Unix.EEXIST, _, _) when count < 1000 ->
      attempt (count + 1)
  in
  attempt 0

(* A build writes thousands of artifacts, and for most of them the
   file-system calls cost more than writing the contents. So the temporary
   file is created once and written and given its permissions through that
   one descriptor. *)
let write_file_atomic ?(ensure_parent = true) path contents =
  let directory = Filename.dirname path in
  if ensure_parent then ensure_dir directory;
  let candidate, descriptor = open_temporary_file directory in
  let open_descriptor = ref (Some descriptor) in
  try
    (* The umask may clear bits of the mode openfile was given. *)
    Platform.set_file_permissions descriptor 0o644;
    ignore (Unix.write_substring descriptor contents 0 (String.length contents));
    open_descriptor := None;
    Unix.close descriptor;
    Build_file_store.forget path;
    Sys.rename candidate path
  with exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    Option.iter
      (fun descriptor ->
        try Unix.close descriptor with Unix.Unix_error _ -> ())
      !open_descriptor;
    (try Sys.remove candidate with Sys_error _ -> ());
    Printexc.raise_with_backtrace exn backtrace

let stat_opt path =
  try Some (Unix.stat path)
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> None

let exists path = Option.is_some (stat_opt path)

let modification_time path =
  stat_opt path |> Option.map (fun metadata -> metadata.Unix.st_mtime)

let remove_file path =
  Build_file_store.forget path;
  try Unix.unlink path
  with Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ()

let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
    directory_entries path
    |> List.iter (fun name -> remove_tree (Filename.concat path name));
    Unix.rmdir path
  | _ ->
    Build_file_store.forget path;
    Unix.unlink path
  | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> ()

let remove_file_best_effort path =
  try remove_file path with Sys_error _ | Unix.Unix_error _ -> ()

let rec files_under directory =
  match (Unix.lstat directory).Unix.st_kind with
  | Unix.S_DIR ->
    directory_entries directory
    |> List.concat_map (fun name ->
        files_under (Filename.concat directory name))
  (* Following links during recursion could leave the requested tree or enter
     a cycle. Follow one only to omit dangling links from the result. *)
  | Unix.S_LNK -> (
    match Unix.stat directory with
    | _ -> [directory]
    | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> [])
  | _ -> [directory]
  | exception Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> []

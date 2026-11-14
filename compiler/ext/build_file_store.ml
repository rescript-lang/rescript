(* Every compile of a project reads many of the same interfaces again (the
   runtime's above all), and the build system then copies each output it just
   wrote. A file system call costs tens of microseconds, and some systems
   serialize them across threads, so in-process builds spent much of their
   time in [open].

   Within a build, a file is read only after the request that writes it has
   finished, because the build system orders requests by their dependencies.
   A request it starts before then gets the interfaces not yet written from
   the build system instead (see [Compiler_request_state.redirect_cmi]): the
   same strings that are then written through the store, held or not. So an
   entry changes only when it is written through the store, and the store is
   emptied before each build. *)

(* What the disk holds for a path, as far as the build has read it. *)
type on_disk = Read of string | Missing | Unknown

type entry =
  | Contents of string
  | Exists  (** On disk, contents not read yet. *)
  | Absent
  | Held of {contents: string; on_disk: on_disk}
      (** Written by a request, not on disk until {!publish_held}. *)

(* [enabled] and [generation] are read without the lock: only [with_build]
   changes them, before a build's requests start and after they finish. *)
type state = {
  mutex: Mutex.t;
  mutable entries: (string, entry) Hashtbl.t;
  mutable enabled: bool;
  mutable generation: int;
}

let state =
  {
    mutex = Mutex.create ();
    entries = Hashtbl.create 1;
    enabled = false;
    generation = 0;
  }

let locked f =
  Mutex.lock state.mutex;
  match f () with
  | result ->
    Mutex.unlock state.mutex;
    result
  | exception exn ->
    Mutex.unlock state.mutex;
    raise exn

let default_disk_reader path =
  match open_in_bin path with
  | channel ->
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> Some (really_input_string channel (in_channel_length channel)))
  | exception (Sys_error _ as error) ->
    if Sys.file_exists path then raise error else None

let disk_reader = Atomic.make default_disk_reader
let set_disk_reader reader = Atomic.set disk_reader reader

let held_writes = Atomic.make (fun (_ : string) -> false)
let set_held_writes held = Atomic.set held_writes held

let enabled () = state.enabled
let generation () = state.generation

let empty () =
  state.entries <- Hashtbl.create 4096;
  state.generation <- state.generation + 1

let with_build f =
  locked (fun () ->
      empty ();
      state.enabled <- true);
  Fun.protect f ~finally:(fun () ->
      locked (fun () ->
          state.enabled <- false;
          empty ()))

(* Whether the segment of [path] from [start] to [index] is [.] or [..]. *)
let is_dot_segment path start index =
  match index - start with
  | 1 -> String.unsafe_get path start = '.'
  | 2 ->
    String.unsafe_get path start = '.'
    && String.unsafe_get path (start + 1) = '.'
  | _ -> false

(* Whether [path], from the segment that starts at [start], has no [.], [..]
   or empty segment, nor a separator other than [/]: then [key] gives back
   [path] itself. Most paths are such, and checking takes no allocation. *)
let rec is_normal_from path start index =
  if index = String.length path then
    index > start && not (is_dot_segment path start index)
  else
    match String.unsafe_get path index with
    | '/' ->
      (index > start || start = 0)
      && (not (is_dot_segment path start index))
      && is_normal_from path (index + 1) (index + 1)
    | '\\' when Sys.win32 -> false
    | _ -> is_normal_from path start (index + 1)

(* Requests spell paths relative to their own working directory, such as
   [lib/rescript/./A.cmi] or [lib/rescript/../../src/A.res], and the build
   system as [lib/rescript/A.cmi]. Both must find the same entry. Build
   directories are never symbolic links, so removing [..] lexically is sound
   for the paths the store sees. *)
let key path =
  if is_normal_from path 0 0 then path
  else
    let is_separator c = c = '/' || (Sys.win32 && c = '\\') in
    let length = String.length path in
    let rec segments start index acc =
      if index = length then
        List.rev (String.sub path start (index - start) :: acc)
      else if is_separator path.[index] then
        segments (index + 1) (index + 1)
          (String.sub path start (index - start) :: acc)
      else segments start (index + 1) acc
    in
    match segments 0 0 [] with
    | [] -> path
    | first :: rest ->
      let rec normalize acc = function
        | [] -> List.rev acc
        | ("" | ".") :: rest -> normalize acc rest
        | ".." :: rest -> (
          match acc with
          | previous :: acc when previous <> ".." -> normalize acc rest
          | _ -> normalize (".." :: acc) rest)
        | segment :: rest -> normalize (segment :: acc) rest
      in
      String.concat "/" (first :: normalize [] rest)

let find key = locked (fun () -> Hashtbl.find_opt state.entries key)

let record key entry =
  locked (fun () ->
      if state.enabled then Hashtbl.replace state.entries key entry)

let read ?(keep = true) path =
  if not state.enabled then (Atomic.get disk_reader) path
  else
    let key = key path in
    match find key with
    | Some (Contents contents | Held {contents; _}) -> Some contents
    | Some Absent -> None
    | Some Exists | None ->
      let contents = (Atomic.get disk_reader) path in
      record key
        (match contents with
        | Some contents -> if keep then Contents contents else Exists
        | None -> Absent);
      contents

let exists path =
  if not state.enabled then Sys.file_exists path
  else
    let key = key path in
    match find key with
    | Some (Contents _ | Exists | Held _) -> true
    | Some Absent -> false
    | None ->
      let exists = Sys.file_exists path in
      record key (if exists then Exists else Absent);
      exists

let write_to_disk path contents =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () ->
      output_string channel contents;
      close_out channel)

let write ?(keep = true) path contents =
  if state.enabled && (Atomic.get held_writes) path then
    let key = key path in
    locked (fun () ->
        (* Remember what the disk holds, so that publishing compares with
           what this build read instead of reading it again. *)
        let on_disk =
          match Hashtbl.find_opt state.entries key with
          | Some (Contents contents) -> Read contents
          | Some Absent -> Missing
          | Some (Held {on_disk; _}) -> on_disk
          | Some Exists | None -> Unknown
        in
        Hashtbl.replace state.entries key (Held {contents; on_disk}))
  else
    match write_to_disk path contents with
    | () ->
      if state.enabled then
        record (key path) (if keep then Contents contents else Exists)
    | exception exn ->
      (* A failed write can leave anything on disk. *)
      if state.enabled then
        locked (fun () -> Hashtbl.remove state.entries (key path));
      raise exn

let evict path =
  if state.enabled then
    let key = key path in
    locked (fun () ->
        match Hashtbl.find_opt state.entries key with
        | Some (Contents _) -> Hashtbl.replace state.entries key Exists
        (* Held contents exist nowhere else. *)
        | Some (Exists | Absent | Held _) | None -> ())

let forget path =
  if state.enabled then
    locked (fun () -> Hashtbl.remove state.entries (key path))

let cached path =
  if not state.enabled then None
  else
    match find (key path) with
    | Some (Contents contents | Held {contents; _}) -> Some contents
    | Some (Exists | Absent) | None -> None

let publish_held ~if_changed ~write path =
  let key = key path in
  match find key with
  | Some (Held {contents; on_disk}) ->
    let changed =
      (not if_changed)
      ||
      let on_disk =
        match on_disk with
        | Read on_disk -> Some on_disk
        | Missing -> None
        | Unknown -> (Atomic.get disk_reader) path
      in
      on_disk <> Some contents
    in
    if changed then write path contents;
    record key (Contents contents);
    changed
  | Some (Contents _ | Exists | Absent) | None -> raise Not_found

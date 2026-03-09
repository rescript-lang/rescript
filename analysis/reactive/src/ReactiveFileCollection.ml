(** Reactive File Collection

    Creates a reactive collection from files with automatic change detection. *)

type file_id = {mtime: float; size: int; ino: int}
(** File identity for change detection *)

let get_file_id path : file_id =
  let st = Unix.stat path in
  {mtime = st.Unix.st_mtime; size = st.Unix.st_size; ino = st.Unix.st_ino}

let file_changed ~old_id ~new_id =
  old_id.mtime <> new_id.mtime
  || old_id.size <> new_id.size || old_id.ino <> new_id.ino

type ('raw, 'v) internal = {
  cache: (string, file_id * 'v) Hashtbl.t;
  read_file: string -> 'raw;
  process: string -> 'raw -> 'v; (* path -> raw -> value *)
}
(** Internal state for file collection *)

type ('raw, 'v) t = {
  internal: ('raw, 'v) internal;
  collection: (string, 'v) Reactive.t;
  emit: (string, 'v Maybe.t) StableWave.t -> unit;
  scratch_wave: (string, 'v Maybe.t) StableWave.t;
}
(** A file collection is just a Reactive.t with some extra operations *)

(** Create a new reactive file collection *)
let create ~read_file ~process : ('raw, 'v) t =
  let internal = {cache = Hashtbl.create 256; read_file; process} in
  let collection, emit = Reactive.Source.create ~name:"file_collection" () in
  let scratch_wave = StableWave.create () in
  {internal; collection; emit; scratch_wave}

(** Get the collection interface for composition *)
let to_collection t : (string, 'v) Reactive.t = t.collection

(** Emit a single set entry *)
let emit_set t path value =
  StableWave.clear t.scratch_wave;
  StableWave.push t.scratch_wave
    (Stable.unsafe_of_value path)
    (Stable.unsafe_of_value (Maybe.some value));
  t.emit t.scratch_wave

(** Process a file if changed. Emits delta to subscribers. *)
let process_if_changed t path =
  let new_id = get_file_id path in
  match Hashtbl.find_opt t.internal.cache path with
  | Some (old_id, _) when not (file_changed ~old_id ~new_id) ->
    false (* unchanged *)
  | _ ->
    let raw = t.internal.read_file path in
    let value = t.internal.process path raw in
    Hashtbl.replace t.internal.cache path (new_id, value);
    emit_set t path value;
    true (* changed *)

(** Process multiple files (emits individual deltas) *)
let process_files t paths =
  List.iter (fun path -> ignore (process_if_changed t path)) paths

(** Process multiple files and emit as a single batch.
    More efficient than process_files when processing many files at once. *)
let process_files_batch t paths =
  StableWave.clear t.scratch_wave;
  let count = ref 0 in
  (* Accumulate changes in an OCaml list to keep values reachable from GC roots.
     We must not store minor-heap values directly into C-allocated scratch_wave
     because the GC cannot update pointers in C memory when it moves objects. *)
  let changes = ref [] in
  List.iter
    (fun path ->
      let new_id = get_file_id path in
      match Hashtbl.find_opt t.internal.cache path with
      | Some (old_id, _) when not (file_changed ~old_id ~new_id) -> ()
      | _ ->
        let raw = t.internal.read_file path in
        let value = t.internal.process path raw in
        Hashtbl.replace t.internal.cache path (new_id, value);
        changes := (path, value) :: !changes;
        incr count)
    paths;
  if !count > 0 then (
    (* Promote all values to the major heap. After this, addresses are stable
       (minor GC never moves major-heap objects, and we don't compact). *)
    Gc.full_major ();
    List.iter
      (fun (path, value) ->
        StableWave.push t.scratch_wave (Stable.of_value path)
          (Stable.of_value (Maybe.some value)))
      !changes;
    t.emit t.scratch_wave);
  !count

(** Remove a file *)
let remove t path =
  Hashtbl.remove t.internal.cache path;
  StableWave.clear t.scratch_wave;
  StableWave.push t.scratch_wave (Stable.unsafe_of_value path) Maybe.none_stable;
  t.emit t.scratch_wave

(** Remove multiple files as a batch *)
let remove_batch t paths =
  StableWave.clear t.scratch_wave;
  let count = ref 0 in
  List.iter
    (fun path ->
      if Hashtbl.mem t.internal.cache path then (
        Hashtbl.remove t.internal.cache path;
        StableWave.push t.scratch_wave
          (Stable.unsafe_of_value path)
          Maybe.none_stable;
        incr count))
    paths;
  if !count > 0 then t.emit t.scratch_wave;
  !count

(** Clear all cached data *)
let clear t = Hashtbl.clear t.internal.cache

(** Invalidate a path *)
let invalidate t path = Hashtbl.remove t.internal.cache path

let get t path =
  match Hashtbl.find_opt t.internal.cache path with
  | Some (_, v) -> Some v
  | None -> None

let mem t path = Hashtbl.mem t.internal.cache path
let length t = Reactive.length t.collection
let iter f t =
  t.collection.iter (fun k v ->
      f (Stable.to_linear_value k) (Stable.to_linear_value v))

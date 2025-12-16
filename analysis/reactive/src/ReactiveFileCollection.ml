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
  process: 'raw -> 'v;
  mutable subscribers: ((string, 'v) Reactive.delta -> unit) list;
}
(** Internal state for file collection *)

type ('raw, 'v) t = {
  internal: ('raw, 'v) internal;
  collection: (string, 'v) Reactive.t;
}
(** A file collection is just a Reactive.t with some extra operations *)

let emit t delta = List.iter (fun h -> h delta) t.internal.subscribers

(** Create a new reactive file collection *)
let create ~read_file ~process : ('raw, 'v) t =
  let internal =
    {cache = Hashtbl.create 256; read_file; process; subscribers = []}
  in
  let collection =
    {
      Reactive.subscribe =
        (fun handler -> internal.subscribers <- handler :: internal.subscribers);
      iter =
        (fun f -> Hashtbl.iter (fun path (_, v) -> f path v) internal.cache);
      get =
        (fun path ->
          match Hashtbl.find_opt internal.cache path with
          | Some (_, v) -> Some v
          | None -> None);
      length = (fun () -> Hashtbl.length internal.cache);
    }
  in
  {internal; collection}

(** Get the collection interface for composition *)
let to_collection t : (string, 'v) Reactive.t = t.collection

(** Process a file if changed. Emits delta to subscribers. *)
let process_if_changed t path =
  let new_id = get_file_id path in
  match Hashtbl.find_opt t.internal.cache path with
  | Some (old_id, _) when not (file_changed ~old_id ~new_id) ->
    false (* unchanged *)
  | _ ->
    let raw = t.internal.read_file path in
    let value = t.internal.process raw in
    Hashtbl.replace t.internal.cache path (new_id, value);
    emit t (Reactive.Set (path, value));
    true (* changed *)

(** Process multiple files *)
let process_files t paths =
  List.iter (fun path -> ignore (process_if_changed t path)) paths

(** Remove a file *)
let remove t path =
  Hashtbl.remove t.internal.cache path;
  emit t (Reactive.Remove path)

(** Clear all cached data *)
let clear t = Hashtbl.clear t.internal.cache

(** Invalidate a path *)
let invalidate t path = Hashtbl.remove t.internal.cache path

let get t path = t.collection.get path
let mem t path = Hashtbl.mem t.internal.cache path
let length t = t.collection.length ()
let iter f t = t.collection.iter f

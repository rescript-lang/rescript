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
  emit: (string, 'v) Reactive.delta -> unit;
}
(** A file collection is just a Reactive.t with some extra operations *)

(** Create a new reactive file collection *)
let create ~read_file ~process : ('raw, 'v) t =
  let internal = {cache = Hashtbl.create 256; read_file; process} in
  let collection, emit = Reactive.source ~name:"file_collection" () in
  {internal; collection; emit}

(** Get the collection interface for composition *)
let to_collection t : (string, 'v) Reactive.t = t.collection

(** Emit a delta *)
let emit t delta = t.emit delta

(** Process a file without emitting. Returns batch entry if changed. *)
let process_file_silent t path =
  let new_id = get_file_id path in
  match Hashtbl.find_opt t.internal.cache path with
  | Some (old_id, _) when not (file_changed ~old_id ~new_id) ->
    None (* unchanged *)
  | _ ->
    let raw = t.internal.read_file path in
    let value = t.internal.process path raw in
    Hashtbl.replace t.internal.cache path (new_id, value);
    Some (Reactive.set path value)

(** Process multiple files and emit as a single batch. *)
let process_files_batch t paths =
  let entries =
    paths |> List.filter_map (fun path -> process_file_silent t path)
  in
  if entries <> [] then emit t (Reactive.Batch entries);
  List.length entries

(** Remove multiple files as a batch *)
let remove_batch t paths =
  let entries =
    paths
    |> List.filter_map (fun path ->
        if Hashtbl.mem t.internal.cache path then (
          Hashtbl.remove t.internal.cache path;
          Some (path, None))
        else None)
  in
  if entries <> [] then emit t (Reactive.Batch entries);
  List.length entries

let mem t path = Hashtbl.mem t.internal.cache path
let iter f t = Reactive.iter f t.collection

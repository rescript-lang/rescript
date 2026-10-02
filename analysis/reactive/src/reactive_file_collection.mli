(** Reactive File Collection

    Creates a reactive collection from files with automatic change detection.

    {2 Example}

    {[
      (* Create file collection *)
      let files = Reactive_file_collection.create
        ~read_file:Cmt_format.read_cmt
        ~process:(fun path cmt -> extract_data path cmt)

      (* Compose with flat_map *)
      let decls = Reactive.flat_map ~name:"decls" (Reactive_file_collection.to_collection files)
        ~f:(fun _path data -> data.decls)
        ()

      (* Process files - decls updates automatically *)
      ignore (Reactive_file_collection.process_files_batch files [file_a; file_b]);

      (* Read results *)
      Reactive.iter (fun pos decl -> ...) decls
    ]} *)

type ('raw, 'v) t
(** A file collection. ['raw] is the raw file type, ['v] is the processed value. *)

(** {1 Creation} *)

val create :
  read_file:(string -> 'raw) -> process:(string -> 'raw -> 'v) -> ('raw, 'v) t
(** Create a new file collection.
    [process path raw] receives the file path and raw content to produce the value. *)

(** {1 Composition} *)

val to_collection : ('raw, 'v) t -> (string, 'v) Reactive.t
(** Get the reactive collection interface for use with [Reactive.flat_map]. *)

(** {1 Processing} *)

val process_files_batch : ('raw, 'v) t -> string list -> int
(** Process files, emitting a single [Batch] delta with all changes.
    Returns the number of files that changed. Downstream combinators
    process all changes together. *)

val remove_batch : ('raw, 'v) t -> string list -> int
(** Remove multiple files as a batch. Returns the number of files removed. *)

(** {1 Access} *)

val mem : ('raw, 'v) t -> string -> bool
val iter : (string -> 'v -> unit) -> ('raw, 'v) t -> unit

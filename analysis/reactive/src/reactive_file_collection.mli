(** Reactive File Collection

    Creates a reactive collection from files with automatic change detection.

    {2 Example}

    {[
      (* Create file collection *)
      let files = ReactiveFileCollection.create
        ~read_file:Cmt_format.read_cmt
        ~process:(fun path cmt -> extract_data path cmt)

      (* Compose with flatMap *)
      let decls = Reactive.flatMap ~name:"decls" (ReactiveFileCollection.to_collection files)
        ~f:(fun _path data -> data.decls)
        ()

      (* Process files - decls updates automatically *)
      ReactiveFileCollection.process_files files [file_a; file_b];

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
(** Get the reactive collection interface for use with [Reactive.flatMap]. *)

(** {1 Processing} *)

val process_files : ('raw, 'v) t -> string list -> unit
(** Process files, emitting individual deltas for each changed file. *)

val process_files_batch : ('raw, 'v) t -> string list -> int
(** Process files, emitting a single [Batch] delta with all changes.
    Returns the number of files that changed.
    More efficient than [process_files] when processing many files at once,
    as downstream combinators can process all changes together. *)

val process_if_changed : ('raw, 'v) t -> string -> bool
(** Process a file if changed. Returns true if file was processed. *)

val remove : ('raw, 'v) t -> string -> unit
(** Remove a file from the collection. *)

val remove_batch : ('raw, 'v) t -> string list -> int
(** Remove multiple files as a batch. Returns the number of files removed.
    More efficient than calling [remove] multiple times. *)

(** {1 Cache Management} *)

val invalidate : ('raw, 'v) t -> string -> unit
val clear : ('raw, 'v) t -> unit

(** {1 Access} *)

val get : ('raw, 'v) t -> string -> 'v option
val mem : ('raw, 'v) t -> string -> bool
val length : ('raw, 'v) t -> int
val iter : (string -> 'v -> unit) -> ('raw, 'v) t -> unit

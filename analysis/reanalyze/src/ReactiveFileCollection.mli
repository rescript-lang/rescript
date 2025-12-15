(** Reactive File Collection

    A collection that maps file paths to processed values, with efficient
    delta-based updates. Designed for use with file watchers.

    {2 Usage Example}

    {[
      (* Create collection with processing function *)
      let coll = ReactiveFileCollection.create
        ~process:(fun (data : Cmt_format.cmt_infos) -> 
          extract_types data
        )

      (* Initial load *)
      List.iter (ReactiveFileCollection.add coll) (glob "*.cmt")

      (* On file watcher events *)
      match event with
      | Created path -> ReactiveFileCollection.add coll path
      | Deleted path -> ReactiveFileCollection.remove coll path
      | Modified path -> ReactiveFileCollection.update coll path

      (* Access the collection *)
      ReactiveFileCollection.iter (fun path value -> ...) coll
    ]}

    {2 Thread Safety}

    Not thread-safe. Use external synchronization if accessed from
    multiple threads/domains. *)

type 'v t
(** The type of a reactive file collection with values of type ['v]. *)

(** Events for batch updates. *)
type event =
  | Added of string  (** File was created *)
  | Removed of string  (** File was deleted *)
  | Modified of string  (** File was modified *)

(** {1 Creation} *)

val create : process:(Cmt_format.cmt_infos -> 'v) -> 'v t
(** [create ~process] creates an empty collection.
    
    [process] is called to transform CMT file contents into values. *)

(** {1 Delta Operations} *)

val add : 'v t -> string -> unit
(** [add t path] adds a file to the collection.
    Loads the file and processes immediately. *)

val remove : 'v t -> string -> unit
(** [remove t path] removes a file from the collection.
    No-op if path is not in collection. *)

val update : 'v t -> string -> unit
(** [update t path] reloads a modified file.
    Equivalent to remove + add, but more efficient. *)

val set : 'v t -> string -> 'v -> unit
(** [set t path value] sets the value for [path] directly.
    Used when you have already processed the file externally. *)

val apply : 'v t -> event list -> unit
(** [apply t events] applies multiple events.
    More efficient than individual operations for batches. *)

(** {1 Access} *)

val get : 'v t -> string -> 'v option
(** [get t path] returns the value for [path], or [None] if not present. *)

val find : 'v t -> string -> 'v
(** [find t path] returns the value for [path].
    @raise Not_found if path is not in collection *)

val mem : 'v t -> string -> bool
(** [mem t path] returns [true] if [path] is in the collection. *)

val length : 'v t -> int
(** [length t] returns the number of files in the collection. *)

val is_empty : 'v t -> bool
(** [is_empty t] returns [true] if the collection is empty. *)

(** {1 Iteration} *)

val iter : (string -> 'v -> unit) -> 'v t -> unit
(** [iter f t] applies [f] to each (path, value) pair. *)

val fold : (string -> 'v -> 'acc -> 'acc) -> 'v t -> 'acc -> 'acc
(** [fold f t init] folds [f] over all (path, value) pairs. *)

val to_list : 'v t -> (string * 'v) list
(** [to_list t] returns all (path, value) pairs as a list. *)

val paths : 'v t -> string list
(** [paths t] returns all paths in the collection. *)

val values : 'v t -> 'v list
(** [values t] returns all values in the collection. *)

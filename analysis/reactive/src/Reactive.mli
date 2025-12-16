(** Reactive collections for incremental computation.

    Provides composable reactive collections with delta-based updates.

    {2 Example: Composing collections}

    {[
      (* Create a file collection *)
      let files = ReactiveFileCollection.create ~read_file ~process in

      (* Derive a declarations collection *)
      let decls = Reactive.flatMap files
        ~f:(fun _path data -> data.decls)
        ()

      (* Derive a references collection with merging *)
      let refs = Reactive.flatMap decls
        ~f:(fun _pos decl -> decl.refs)
        ~merge:PosSet.union
        ()

      (* Process files - all downstream collections update automatically *)
      files |> Reactive.iter (fun path _ -> 
        ReactiveFileCollection.process_if_changed files_internal path)

      (* Read from any collection *)
      Reactive.iter (fun k v -> ...) refs
    ]} *)

(** {1 Deltas} *)

type ('k, 'v) delta = Set of 'k * 'v | Remove of 'k

val apply_delta : ('k, 'v) Hashtbl.t -> ('k, 'v) delta -> unit
val apply_deltas : ('k, 'v) Hashtbl.t -> ('k, 'v) delta list -> unit

(** {1 Reactive Collection} *)

type ('k, 'v) t = {
  subscribe: (('k, 'v) delta -> unit) -> unit;
  iter: ('k -> 'v -> unit) -> unit;
  get: 'k -> 'v option;
  length: unit -> int;
}
(** A reactive collection that can emit deltas and be read.
    All collections share this interface, enabling composition. *)

(** {1 Collection operations} *)

val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
(** Iterate over entries. *)

val get : ('k, 'v) t -> 'k -> 'v option
(** Get a value by key. *)

val length : ('k, 'v) t -> int
(** Number of entries. *)

(** {1 Composition} *)

val flatMap :
  ('k1, 'v1) t ->
  f:('k1 -> 'v1 -> ('k2 * 'v2) list) ->
  ?merge:('v2 -> 'v2 -> 'v2) ->
  unit ->
  ('k2, 'v2) t
(** [flatMap source ~f ()] creates a derived collection.
    
    Each entry [(k1, v1)] in [source] produces entries [(k2, v2), ...] via [f k1 v1].
    When [source] changes, the derived collection updates automatically.
    
    Optional [merge] combines values when multiple sources produce the same key.
    Defaults to last-write-wins.
    
    Derived collections can be further composed with [flatMap]. *)

(** {1 Lookup} *)

val lookup : ('k, 'v) t -> key:'k -> ('k, 'v) t
(** [lookup source ~key] creates a reactive subscription to a single key.
    
    Returns a collection containing at most one entry (the value at [key]).
    When [source]'s value at [key] changes, the lookup collection updates.
    
    Useful for reactive point queries. *)

(** {1 Join} *)

val join :
  ('k1, 'v1) t ->
  ('k2, 'v2) t ->
  key_of:('k1 -> 'v1 -> 'k2) ->
  f:('k1 -> 'v1 -> 'v2 option -> ('k3 * 'v3) list) ->
  ?merge:('v3 -> 'v3 -> 'v3) ->
  unit ->
  ('k3, 'v3) t
(** [join left right ~key_of ~f ()] joins two collections.
    
    For each entry [(k1, v1)] in [left]:
    - Computes lookup key [k2 = key_of k1 v1]
    - Looks up [k2] in [right] to get [v2_opt]
    - Produces entries via [f k1 v1 v2_opt]
    
    When either [left] or [right] changes, affected entries are recomputed.
    This is the reactive equivalent of a hash join.
    
    {2 Example: Exception refs lookup}
    
    {[
      (* exception_refs: (path, loc_from) *)
      (* decl_by_path: (path, decl list) *)
      let resolved = Reactive.join exception_refs decl_by_path
        ~key_of:(fun path _loc -> path)
        ~f:(fun path loc decls_opt ->
          match decls_opt with
          | Some decls -> decls |> List.map (fun d -> (d.pos, loc))
          | None -> [])
        ()
    ]} *)

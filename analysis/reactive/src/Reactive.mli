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

(** {1 Union} *)

val union :
  ('k, 'v) t -> ('k, 'v) t -> ?merge:('v -> 'v -> 'v) -> unit -> ('k, 'v) t
(** [union left right ?merge ()] combines two collections.
    
    Returns a collection containing all entries from both [left] and [right].
    When the same key exists in both collections:
    - If [merge] is provided, values are combined with [merge left_val right_val]
    - Otherwise, the value from [right] takes precedence
    
    When either collection changes, the union updates automatically.
    
    {2 Example: Combining reference sets}
    {[
      let value_refs = ...
      let type_refs = ...
      let all_refs = Reactive.union value_refs type_refs ~merge:PosSet.union ()
    ]} *)

(** {1 Fixpoint} *)

val fixpoint :
  init:('k, unit) t -> edges:('k, 'k list) t -> unit -> ('k, unit) t
(** [fixpoint ~init ~edges ()] computes transitive closure.
    
    Starting from keys in [init], follows edges to discover all reachable keys.
    
    - [init]: reactive collection of starting keys
    - [edges]: reactive collection mapping each key to its successor keys
    - Returns: reactive collection of all reachable keys
    
    When [init] or [edges] changes, the fixpoint recomputes.
    
    {b Note}: Current implementation recomputes full fixpoint on any change.
    Future versions will update incrementally.
    
    {2 Example: Reachability}
    {[
      let roots = ...  (* keys that are initially reachable *)
      let graph = ...  (* key -> successor keys *)
      let reachable = Reactive.fixpoint ~init:roots ~edges:graph ()
    ]} *)

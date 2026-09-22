(** Graph indexing is shared by cycle analysis and subprocess scheduling, but
    their input contracts differ. The validation policy makes replacement or
    rejection of duplicate and unknown nodes an explicit caller decision. *)
type validation =
  | Replace_duplicates_and_ignore_unknown
  | Reject_invalid of {
      duplicate_node: string -> exn;
      unknown_dependency: node:string -> dependency:string -> exn;
    }

type 'a index

val create_index :
  'a list ->
  name:('a -> string) ->
  deps:('a -> string list) ->
  validation:validation ->
  'a index

val node_count : 'a index -> int
val find_node : 'a index -> string -> 'a
val dependencies : 'a index -> string -> string list
val dependents : 'a index -> string -> string list
val dependency_count : 'a index -> string -> int
val shortest_cycle_in_index : 'a index -> string list option

val cycle_blocked_nodes :
  'a list -> name:('a -> string) -> deps:('a -> string list) -> 'a list

val shortest_cycle :
  'a list ->
  name:('a -> string) ->
  deps:('a -> string list) ->
  string list option

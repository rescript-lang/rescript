exception Cycle of string list

val blocked_dependents : (string * string list) list -> string list -> string list

val shortest_cycle :
  'a list ->
  name:('a -> string) ->
  deps:('a -> string list) ->
  string list option

val topological_sort :
  'a list -> name:('a -> string) -> deps:('a -> string list) -> 'a list

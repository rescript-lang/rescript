val cycle_blocked_nodes :
  'a list -> name:('a -> string) -> deps:('a -> string list) -> 'a list

val shortest_cycle :
  'a list ->
  name:('a -> string) ->
  deps:('a -> string list) ->
  string list option

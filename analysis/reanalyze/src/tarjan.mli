type 'a component = {members: 'a list}

val compute : successors:('a -> 'a list) -> 'a list -> 'a component list


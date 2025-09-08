let module(Set) = z 
let module(Set : Set.S with type elt = s) = y
let module(Set : Set.S with type elt = s and type elt2 = t) = x 
let module(Set : Set.S with type elt = s and type elt2 = t and type elementWithSuperLongName = thisIsALongTypeOverHere) = x 

/* Pin (object mutability cleanup): mutable-to-mutable coercion is invariant
   in the field type — with unequal types it is rejected (today the setter
   member demands contravariance while the getter demands covariance). Must
   stay an error under the new model (Mutable A <: Mutable B iff A = B).
   See docs/object_representation_cleanup.md. */
type wide = {"a": int, "b": int}
type narrow = {"a": int}
let p = (v: {@set "x": wide}) => (v :> {@set "x": narrow})

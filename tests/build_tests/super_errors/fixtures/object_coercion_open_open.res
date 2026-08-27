/* Pin (object mutability cleanup): when BOTH rows are open, object fields
   are invariant — this covariant coercion is rejected. Principled, not an
   artifact: an open result is a promotable result, and a covariantly
   weakened field must never remain promotable (a later write at the narrow
   type would reach readers at the wide type). Must stay an error under the
   new model. See docs/object_representation_cleanup.md. */
type wide = {"a": int, "b": int}
type narrow = {"a": int}
let p = (o: {.."x": wide}) => (o :> {.."x": narrow})

/* A coercion cannot acquire write access at a type different from the
   field's type: promotion changes mutability without changing the field
   type, and mutable fields are invariant. Under the previous phantom-member
   encoding this compiled, leaving getter `wide` and setter `narrow` on one
   property. */
type wide = {"a": int, "b": int}
type narrow = {"a": int}
let p = (o: {.."x": wide}) => (o :> {@set "x": narrow})

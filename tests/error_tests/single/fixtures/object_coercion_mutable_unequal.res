/* Mutable fields are invariant in the field type. A mutable field of A is a
   subtype of a mutable field of B only when A and B are equivalent: reads
   require covariance, while writes require contravariance. This coercion
   therefore fails because wide and narrow are not equivalent. */

type wide = {"a": int, "b": int}
type narrow = {"a": int}
let p = (v: {@set "x": wide}) => (v :> {@set "x": narrow})

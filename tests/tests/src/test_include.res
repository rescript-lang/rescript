open Belt

module N = Belt.List
let v = N.length

module type OrderedType = {
  type t
  let compare: (t, t) => int
}

module Make = (U: OrderedType) => {
  let v = U.compare
}

module U = Make(Test_order)

/* Missing optimization
  alias to a number could also be removed, especially 0
*/

module N0 = N

module N1 = N0

module N2 = N1
module N3 = N2
module N4 = N3
module N5 = N4
module N6 = N5

include N6

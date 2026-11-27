module type OrderedType = {
  type t
  let compare: (t, t) => int
}

module Make = (U: OrderedType) => {
  include U
}

include (
  {
    module V = Test_order
    module U = V

    let v = module(U: OrderedType)

    let pack = h => {
      module U = unpack(h: OrderedType)
      module V = Make(U)
      module(V: OrderedType)
    }

    let g = pack(v)
    let gg = pack(module(Test_order))
    module N = Make(U)
    module NN = Make(Test_order)
  }: {}
)

module type OrderedType = {
  type t
  let compare: (t, t) => int
}

module Make = (U: OrderedType) => {
  let v = U.compare
}

include (
  {
    module M = (
      S: {
        let add: (int, int) => int
      },
    ) => {
      let u = S.add(1, 2)
    }
    module H = M({
      let add = (x, y) => x + y
    })

    open Belt

    include List
    module N = List
    let v = N.length

    module Make = (U: OrderedType) => {
      let v = U.compare
    }

    module U = Make(Test_order)
  }: {}
)

// ^int

module StrSet = CreateInterfaceFunctor.MakeSet({
  type t = string
  let compare = (a: string, b) => a < b ? -1 : a > b ? 1 : 0
})

include CreateInterfaceFunctor.MakeSet({
  type t = float
  let compare = (a: float, b) => a < b ? -1 : a > b ? 1 : 0
})

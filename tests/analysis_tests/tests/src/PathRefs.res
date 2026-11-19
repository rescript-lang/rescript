module type Ord = {
  type t
}
module IntOrd = {
  type t = int
}
module MakeSet = (O: Ord) => {
  type sh = Dot | Box(O.t)
  let add = (x: O.t) => [x]
  //  ^ref
}
module MakeTwo = (A: Ord, B: Ord) => {
  let pair = 1
  //  ^ref
}
module First = MakeTwo(IntOrd)

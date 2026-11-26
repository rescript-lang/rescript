module M = Belt.Set

module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a: int, b: int) => compare(a, b)
})

let s = M.fromArray([1], ~id=module(IntCmp))
let n: string = M.size(s)

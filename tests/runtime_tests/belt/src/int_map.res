module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = (a, b) => Pervasives.compare(a, b)
})

let m = Belt.Map.make(~id=module(IntCmp))
let m = m->Belt.Map.set(0, "test")

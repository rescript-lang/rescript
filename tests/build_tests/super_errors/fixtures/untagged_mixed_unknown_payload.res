type t = | @unboxed A(list<int>) | B({x: int}) | C
let f = x =>
  switch x {
  | A(_) => "a"
  | B(_) => "b"
  | C => "c"
  }

type t = | @unboxed A({x: int}) | B({y: int}) | C
let f = x =>
  switch x {
  | A(_) => "a"
  | B(_) => "b"
  | C => "c"
  }
let r = f(A({x: 1}))

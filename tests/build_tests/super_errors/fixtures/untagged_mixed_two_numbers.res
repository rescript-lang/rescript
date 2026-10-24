type t = | @unboxed A(int) | @unboxed B(int) | C
let f = x =>
  switch x {
  | A(_) => "a"
  | B(_) => "b"
  | C => "c"
  }

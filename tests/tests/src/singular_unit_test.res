type t = A

let f0 = (x: t) =>
  switch x {
  | A as y => y
  }

let f1 = (x: t) =>
  switch x {
  | A => 2
  }

let f3 = x =>
  switch x {
  | Some(A as y) => y
  | None => A
  }

let v0 = ()

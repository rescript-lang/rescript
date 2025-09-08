let f = x =>
  if true {
    x
  } else {
    x - 1
  }

let ff = x =>
  switch Some(x) {
  | Some(_) => x
  | None => 0
  }

type t =
  | A(int)
  | B(int)
  | C(int)

let fff = x =>
  switch A(x) {
  | A(x) => x
  | B(_) => 1
  | C(_) => 2
  }

let h = x =>
  switch x {
  | #A => 0
  | #B => 1
  | #C => 2
  }
let hh = () =>
  switch "x" {
  | "y" => 1
  | "z" => 2
  | "x" => 3
  | _ => 4
  }

let g = h(#A)

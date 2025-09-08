type t = A(int) | B(string)
let eq_A = (x: t, y) =>
  switch x {
  | A(x) =>
    switch y {
    | A(x1) => x == x1
    | _ => false
    }
  | _ => false
  }

module Test = () => {
  let () = Js.log("no inline")
  let u = A(3)
  module Block = {}
  let y = 32
  let b = eq_A(A(3), u)
}

module Test2 = () => {
  let () = Js.log("no inline")

  module Block = {}
  let y = 32
  let b = eq_A(A(3), A(3))
}

let x = 3

let f = (i, y) => {
  let x = A(i)
  eq_A(x, y)
}

module Test3 = () => {
  let f = (x, y) => x == y
  module Caml_obj = {}
}
module Test4 = () => {
  module Caml_obj = {}
  let f = (x, y) => x == y
}

module Test5 = () => {
  let f = x => Some(x)
  module Caml_option = {}
}

module Test6 = () => {
  module Caml_option = {}
  let f = x => Some(x)
}

module Test7 = () => {
  module Caml_option = {}
}

module Test8 = () => {
  module Curry = {}
  let f = x => x(1)
}

module Test9 = () => {
  let f = x => x(1)
  module Curry = {}
}

module Test10 = () => {
  module Curry = {}
}

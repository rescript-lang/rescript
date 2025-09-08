let suites = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)
let b = (loc, x) => Mt.bool_suites(~test_id, ~suites, loc, x)

module Block = {}
type t =
  | A0(int)
  | A(int, int)

let v0 = A(0, 1)

module N = {
  module Block = {}
  let v1 = A(0, 1)
}

module Caml_obj = {}

module V = {
  module List = {}
}
let f = (a, b) => a == b

let h = Belt.List.length

eq(__LOC__, h(list{1, 2}), 2)
b(__LOC__, f(v0, A(0, 1)))
eq(__LOC__, v0, N.v1)

Mt.from_pair_suites(__MODULE__, suites.contents)

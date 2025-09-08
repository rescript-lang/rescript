/* [@@@config {flags = [|"-dsource"|]}] */
let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

type rec t = {
  a: option<int>,
  b: y,
}
and y = {
  xx: int,
  yy: int,
}

let v = {a: Some(3), b: {xx: 2, yy: 3}}

let u = {...v, a: Some(2)}

let h = list{1, 2, 3, 4}

exception A(int)

type u = ..

type u += B(int, int)

let v0 = A(3)
let v1 = B(3, 2)
let v2 = #C(2)
let v3 = #C(2, 3)

module N = {
  let a = 0
  let b = 1
  external f: int => int = "%identity"
}

module N0: {
  let a: int
  let b: int
  let f: int => int
} = {
  let a = 0
  let b = 1
  external f: int => int = "%identity"
}

Js.log2("hei", v)

let (a, c) = ((1, 2, 2, 4, 3), [1, 2, 3, 4, 5])
Js.log2(a, c)

%%private(let i = 3)
%%private(let a = (``, `a`))

eq(__LOC__, a, ("", "a"))

Mt.from_pair_suites(__FILE__, suites.contents)

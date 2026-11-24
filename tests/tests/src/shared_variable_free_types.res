/* Uses of let-bound values whose types contain no type variable share those
 types instead of copying them (#9265). These uses go through open objects,
 settable fields, polymorphic variants, recursion, GADTs and weak types. */

let o = {"x": 1, "y": "a"}
let getX = (p: {.."x": int}) => p["x"]
let a = getX(o) + getX(o)

@val external settable: {@set "x": int} = "settableObj"
let s1 = settable
let s2 = settable
let write = () => s1["x"] = s2["x"] + 1

let v: [#a | #b] = #a

let toInt = (x: [< #a | #b | #c]) =>
  switch x {
  | #a => 1
  | #b => 2
  | #c => 3
  }

let b = toInt(v) + toInt(v)

let rec even = n => n == 0 ? true : odd(n - 1)
and odd = n => n == 0 ? false : even(n - 1)

type rec expr<_> =
  | Int(int): expr<int>
  | Bool(bool): expr<bool>

let eval = (type a, e: expr<a>): a =>
  switch e {
  | Int(n) =>
    let k = n + 1
    let k2 = k
    k2
  | Bool(b) =>
    let c = !b
    c
  }

let r = ref(None)
let () = r := Some(o)

let local = () => {
  let l = {"z": [1, 2]}
  let l1 = l
  let l2 = l
  Array.length(l1["z"]) + Array.length(l2["z"])
}

let pair = (o, v)
let (o2, v2) = pair
let n = o2["x"] + toInt(v2)

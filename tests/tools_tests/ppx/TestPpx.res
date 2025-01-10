Console.log("ppx test")

type t = [#A | #B]

let a: t = #A
let b: t = #B

module M = {
  let v = 10
}

open M

let vv = v

module OptionalFields = {
  type opt = {x?: int, y: float}

  let r = {y: 1.0}
}

module Arity = {
  let one = x => x
  let two = (x, y) => x + y
  let n = two(one(1), 5)
}

@module("react")
external useState: (unit => 'state) => string = "useState"

let _ = useState(() => 0)

let fpromise = async (promise, _x) => await promise
module Uncurried = {
  type f1 = int => string
  type f2 = (int, int) => string
}

let async_succ = async x => x + 1
let async_foo = async (x, y) => {
  let a: promise<int> = async_succ(x)
  let b: promise<int> = async_succ(y)
  (await a) + (await b)
}

let add = (x, y) => x + y
let partial_add = add(3, ...)

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
  let a = async_succ(x)
  let b = async_succ(y)
  (await a) + (await b)
}

let add = (x, y) => x + y
let partial_add = add(3, ...)

module Pipe = {
  let plus = (x, y) => x + y
  let z = 1->plus(2)
}

let concat = "a" ++ "b"

let neq = 3 != 3
let neq2 = 3 !== 3

let eq = 3 == 3
let eq2 = 3 === 3

let test = async () => 12
let f = async () => (await test()) + 1

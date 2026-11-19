open Mocha
open Test_utils

// Labelled arguments passed before arguments declared ahead of them are still
// evaluated in source order.

type t
@send external send: (t, int, ~extra: int, int) => array<int> = "send"
@obj external makeObj: (~x: int, ~y: int) => {"x": int, "y": int} = ""

let target: t = %raw(`{send: (...xs) => xs}`)

let f = (x: int, ~extra: int, y: int) => [x, extra, y]
let withOpt = (x: int, ~extra: option<int>=?, y: int) => [x, extra->Option.getOr(0), y]

describe(__MODULE__, () => {
  let log = []
  let mark = (name, v) => {
    log->Array.push(name)
    v
  }
  let take = () => {
    let names = log->Array.join("")
    log->Array.splice(~start=0, ~remove=log->Array.length, ~insert=[])
    names
  }

  test("a function", () => {
    let r = f(~extra=mark("b", 2), mark("a", 1), mark("c", 3))
    eq(__LOC__, "bac", take())
    eq(__LOC__, [1, 2, 3], r)
  })

  test("a @send external", () => {
    let r = send(target, ~extra=mark("b", 2), mark("a", 1), mark("c", 3))
    eq(__LOC__, "bac", take())
    eq(__LOC__, [1, 2, 3], r)
  })

  test("an @obj external", () => {
    let o = makeObj(~y=mark("y", 2), ~x=mark("x", 1))
    eq(__LOC__, "yx", take())
    eq(__LOC__, (1, 2), (o["x"], o["y"]))
  })

  test("a callee that is a call", () => {
    let getF = () => mark("f", f)
    let r = getF()(~extra=mark("b", 2), mark("a", 1), mark("c", 3))
    eq(__LOC__, "fbac", take())
    eq(__LOC__, [1, 2, 3], r)
  })

  test("a read after an effect that changes it", () => {
    let n = ref(0)
    let bump = () => {
      n := n.contents + 1
      10
    }
    eq(__LOC__, [1, 10, 5], f(~extra=bump(), n.contents, 5))
  })

  test("an optional argument", () => {
    let r = withOpt(~extra=?mark("b", Some(2)), mark("a", 1), mark("c", 3))
    eq(__LOC__, "bac", take())
    eq(__LOC__, [1, 2, 3], r)
  })

  test("a partial application", () => {
    let g = f(~extra=mark("b", 2), mark("a", 1), ...)
    eq(__LOC__, "ba", take())
    eq(__LOC__, [1, 2, 3], g(3))
  })

  test("arguments already in order", () => {
    let r = f(mark("a", 1), ~extra=mark("b", 2), mark("c", 3))
    eq(__LOC__, "abc", take())
    eq(__LOC__, [1, 2, 3], r)
  })
})

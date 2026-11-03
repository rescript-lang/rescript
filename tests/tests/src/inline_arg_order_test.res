// The beta reducer used to stack inlined-call argument bindings in reverse
// parameter order, so the last argument was evaluated first. The checked-in
// JS pins evaluation to source order: effA runs before effB.
let recorded: array<string> = []

let equalish = (a: array<int>, b: array<int>) => a == b
let copy = (a: array<int>) => Array.copy(a)
let helper = (x, y) => equalish(x, copy(y))

let rec effA = n => {
  recorded->Array.push("a")
  if n > 0 {
    effA(n - 1)
  } else {
    [n]
  }
}

let rec effB = n => {
  recorded->Array.push("b")
  if n > 0 {
    effB(n - 1)
  } else {
    [n]
  }
}

let _ = {
  helper(effA(0), effB(0))
}

// A function whose body is a single primitive or call used to be inlined by
// putting each argument where the body uses its parameter. The arguments then
// ran in that order, and a read of mutable state could move past the effects
// of another argument or past an await.
open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

let order: array<string> = []
let resetOrder = () => order->Array.splice(~start=0, ~remove=Array.length(order), ~insert=[])

let logA = () => {
  order->Array.push("A")
  1
}

let logB = () => {
  order->Array.push("B")
  2
}

let logTrue = () => {
  order->Array.push("true")
  true
}

let r = ref(0)

let bump = () => {
  r := r.contents + 10
  1
}

let minus = (a: int, b: int) => b - a
let sub = (a: int, b: int) => a - b
let flip = (a, b) => sub(b, a)
let first = (a: int, _b: int) => Int.toString(a)
let addBump = (b: int) => bump() + b
let both = (a: bool, b: bool) => a && b
let either = (a: bool, b: bool) => a || b
let apply2 = (f, a: bool, b: bool) => f(a, b)

let minusAfterAwait = async (p: promise<int>) => minus(await p, r.contents)
let flipAfterAwait = async (p: promise<int>) => flip(await p, r.contents)

// The bindings make the condition compile to statements, which must stay in
// the async function rather than go into a closure called for each iteration
let countWhileAwait = async (p: promise<int>) => {
  let n = ref(0)
  while minus(await p, r.contents) < 5 && n.contents < 3 {
    n := n.contents + 1
  }
  n.contents
}

describe(__MODULE__, () => {
  test("arguments that are let-bound before the body", () => {
    eq(__LOC__, ["a", "b"], recorded)
  })

  test("arguments with side effects", () => {
    resetOrder()
    eq(__LOC__, 1, minus(logA(), logB()))
    eq(__LOC__, ["A", "B"], order)
  })

  test("a body that is a call", () => {
    resetOrder()
    eq(__LOC__, 1, flip(logA(), logB()))
    eq(__LOC__, ["A", "B"], order)
  })

  test("an unused argument", () => {
    resetOrder()
    eq(__LOC__, "1", first(logA(), logB()))
    eq(__LOC__, ["A", "B"], order)
  })

  test("a read after an argument that writes", () => {
    r := 0
    eq(__LOC__, 9, minus(bump(), r.contents))
  })

  test("a read before an argument that writes", () => {
    r := 0
    eq(__LOC__, 1, minus(r.contents, bump()))
  })

  test("a read before a body that writes", () => {
    r := 0
    eq(__LOC__, 1, addBump(r.contents))
  })

  test("a local ref that becomes a JS variable", () => {
    let s = ref(0)
    let bumpS = () => {
      s := s.contents + 10
      1
    }
    eq(__LOC__, 1, minus(s.contents, bumpS()))
    eq(__LOC__, 19, minus(bumpS(), s.contents))
  })

  test("the right operand of && and ||", () => {
    resetOrder()
    eq(__LOC__, false, both(false, logTrue()))
    eq(__LOC__, true, either(true, logTrue()))
    eq(__LOC__, false, apply2((x, y) => x && y, false, logTrue()))
    eq(__LOC__, ["true", "true", "true"], order)
  })

  testAsync("a read after an await", async () => {
    r := 0
    let pending = minusAfterAwait(Promise.resolve(1))
    r := 10
    eq(__LOC__, 9, await pending)
  })

  testAsync("a read after an await, in a body that is a call", async () => {
    r := 0
    let pending = flipAfterAwait(Promise.resolve(1))
    r := 10
    eq(__LOC__, 9, await pending)
  })

  testAsync("a read after an await, in a while condition", async () => {
    r := 0
    let pending = countWhileAwait(Promise.resolve(1))
    r := 10
    eq(__LOC__, 0, await pending)
  })
})

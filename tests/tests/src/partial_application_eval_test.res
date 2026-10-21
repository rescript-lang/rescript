open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

let add = (a, b, c) => a + b + c
let first = (x, _y) => x

%%raw(`
let partialReads = 0
Object.defineProperty(globalThis, "partialApplicationValue", {
  get() {
    partialReads += 1
    return partialReads
  },
  configurable: true,
})
`)
@val external partialApplicationValue: int = "partialApplicationValue"

describe(__MODULE__, () => {
  test("arguments are evaluated once", () => {
    let calls = ref(0)
    let next = () => {
      calls := calls.contents + 1
      calls.contents
    }
    let h = add(next(), ...)
    eq(__LOC__, 1, calls.contents)
    eq(__LOC__, 4, h(1, 2))
    eq(__LOC__, 4, h(1, 2))
    eq(__LOC__, 1, calls.contents)
  })

  test("the callee is evaluated once", () => {
    let calls = ref(0)
    let getAdd = () => {
      calls := calls.contents + 1
      add
    }
    let h = getAdd()(1, ...)
    ignore(h(2, 3))
    ignore(h(2, 3))
    eq(__LOC__, 1, calls.contents)
  })

  test("a closure argument is created once", () => {
    let h = first(x => x + 1, ...)
    ok(__LOC__, h(0) === h(0))
  })

  test("a constant tuple argument is created once", () => {
    let h = first((1, 2), ...)
    ok(__LOC__, h(0) === h(0))
  })

  test("a nested None argument is created once", () => {
    let h = first(Some(None), ...)
    ok(__LOC__, h(0) === h(0))
  })

  test("an external value is read once", () => {
    let h = add(partialApplicationValue, ...)
    eq(__LOC__, 3, h(1, 1))
    eq(__LOC__, 3, h(1, 1))
  })

  test("a partial application of values stays polymorphic", () => {
    let one = first(1, ...)
    eq(__LOC__, 2, one("ignored") + one(0))
  })

  test("callee and arguments are evaluated left to right", () => {
    let log = []
    let getAdd = () => {
      log->Array.push("callee")
      add
    }
    let arg = (name, value) => {
      log->Array.push(name)
      value
    }
    let h = getAdd()(arg("a", 1), arg("b", 2), ...)
    eq(__LOC__, ["callee", "a", "b"], log)
    eq(__LOC__, 6, h(3))
  })

  testAsync("an awaited argument stays in the async function", async () => {
    let h = add(await Promise.resolve(1), ...)
    eq(__LOC__, 4, h(1, 2))
    eq(__LOC__, 6, h(2, 3))
  })
})

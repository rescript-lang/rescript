open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

// `let x = e; f(x)` runs `e` before it evaluates `f`. JavaScript evaluates the
// callee of `f(e)` before the argument, so the generated JS keeps the binding
// unless `e` gives the same value after `f`, or `f` the same function after
// `e`: `fold` and `foldRead` below still compile to one call.

type counter = {mutable n: int}

type handler = {mutable call: int => int}

type snapshot = {mutable unused: int, saved: int}

let c = {n: 0}

let next = () => {
  c.n = c.n + 1
  (x: int) => x
}

let h = {call: x => x + 1}

let swap = () => {
  h.call = x => x + 100
  1
}

let events: array<string> = []

let log = (s: string) => {
  events->Array.push(s)
  s
}

let getHandler = async () => {
  events->Array.push("getHandler")
  (s: string) => s
}

%%raw(`globalThis.letBeforeCalleeHook = x => x + 1`)

@scope("globalThis") @val external hook: int => int = "letBeforeCalleeHook"

let install: unit => int = %raw(`() => {
  globalThis.letBeforeCalleeHook = x => x + 100
  return 1
}`)

type box = {v: int}

%%raw(`globalThis.LetBeforeCalleeBox = class {
  constructor(x) {
    this.v = x + 1
  }
}`)

@scope("globalThis") @new external makeBox: int => box = "LetBeforeCalleeBox"

let installBox: unit => int = %raw(`() => {
  globalThis.LetBeforeCalleeBox = class {
    constructor(x) {
      this.v = x + 100
    }
  }
  return 1
}`)

let readBeforeCall = () => {
  let before = c.n
  next()(before)
}

let effectBeforeFieldRead = () => {
  let x = swap()
  h.call(x)
}

let effectBeforeAwait = async () => {
  let x = log("argument")
  (await getHandler())(x)
}

let callNext = (x: int) => next()(x)

let inlined = () => callNext(c.n)

let recordField = () => {
  let s = {unused: 0, saved: c.n}
  next()(s.saved)
}

let assignedVariable = replace => {
  let f = ref((x: int) => x + 1)
  let x = if replace {
    f := x => x + 100
    1
  } else {
    2
  }
  f.contents(x)
}

let effectBeforeExternal = () => {
  let x = install()
  hook(x)
}

let effectBeforeNew = () => {
  let x = installBox()
  makeBox(x)
}

let fold = (cb: int => int) => {
  let x = swap()
  cb(x)
}

let foldRead = () => {
  let x = c.n
  h.call(x)
}

describe(__MODULE__, () => {
  test("a let that reads what the callee changes", () => {
    c.n = 0
    eq(__LOC__, readBeforeCall(), 0)
    eq(__LOC__, c.n, 1)
  })

  test("a let that changes the field the callee is read from", () => {
    h.call = x => x + 1
    eq(__LOC__, effectBeforeFieldRead(), 101)
  })

  testAsync("a let before an awaited callee", async () => {
    events->Array.splice(~start=0, ~remove=Array.length(events), ~insert=[])
    let p = effectBeforeAwait()
    eq(__LOC__, events, ["argument", "getHandler"])
    eq(__LOC__, await p, "argument")
  })

  test("an argument of an inlined function", () => {
    c.n = 0
    eq(__LOC__, inlined(), 0)
  })

  test("a field of a local record", () => {
    c.n = 0
    eq(__LOC__, recordField(), 0)
  })

  test("a let that assigns the ref the callee is read from", () => {
    eq(__LOC__, assignedVariable(true), 101)
    eq(__LOC__, assignedVariable(false), 3)
  })

  test("a let that replaces an external function", () => eq(__LOC__, effectBeforeExternal(), 101))

  test("a let that replaces an external class", () => eq(__LOC__, effectBeforeNew().v, 101))

  test("a callee that stays the same", () => {
    h.call = x => x + 1
    eq(__LOC__, fold(x => x * 2), 2)
    c.n = 5
    eq(__LOC__, foldRead(), 105)
  })
})

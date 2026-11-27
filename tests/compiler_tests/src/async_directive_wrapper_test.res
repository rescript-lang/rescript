open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

// A function that only passes its parameters on to a call stays a function of
// its own when it is async or has a directive: the callee cannot replace it,
// and the callee's call cannot replace a call to it.

let isPromise: 'a => bool = %raw(`x => x instanceof Promise`)
let source: 'a => string = %raw(`f => f.toString()`)

// Recursive, so the optimizer does not inline it
let rec countdown = n => n <= 0 ? 0 : countdown(n - 1)

@val external parseInt: string => int = "parseInt"
@send external push: (array<int>, int) => unit = "push"

// Printed as a wrapper around a ReScript call
let asyncWrapper = async n => countdown(n)
let asyncRuntimeWrapper = async (a: array<int>, b: array<int>) => compare(a, b)
let serverAction = @directive("'use server'") async n => countdown(n)
let cachedFn = @directive("'use cache'") n => countdown(n)

let makeAsyncWrapper = () => async n => countdown(n)
let makeServerAction = () => @directive("'use server'") async n => countdown(n)
let makeCachedFn = () => @directive("'use cache'") n => countdown(n)

// Applied where they are defined
let config: promise<int> = (async s => parseInt(s))("42")
let sum: promise<int> = (async (x, y) => x + y)(1, 2)
let kind: promise<Type.t> = (async s => Type.typeof(parseInt(s)))("42")
let pushed = []
let pushResult: promise<unit> = (async (arr, x) => push(arr, x))(pushed, 1)
let piped: promise<int> = "42"->(async s => parseInt(s))
let serverConfig: promise<int> = (@directive("'use server'") async s => parseInt(s))("42")
let cachedConfig: int = (@directive("'use cache'") s => parseInt(s))("42")
// Reducing the async function away would leave `await` in a plain function
let awaitThrough = (p: promise<int>) => (async q => await q)(p)

// A promise, not the value it resolves to
let resolvesTo = async (loc, p: promise<'a>, expected: 'a) => {
  ok(loc, isPromise(p))
  eq(loc, expected, await p)
}

describe(__MODULE__, () => {
  testAsync("async wrapper around a ReScript call returns a promise", async () => {
    let p = asyncWrapper(3)
    ok(__LOC__, isPromise(p))
    eq(__LOC__, 0, await p)
  })

  testAsync("async wrapper around a runtime call returns a promise", async () => {
    let p = asyncRuntimeWrapper([1], [2])
    ok(__LOC__, isPromise(p))
    eq(__LOC__, -1, await p)
  })

  testAsync("async wrapper with a directive keeps both", async () => {
    let p = serverAction(3)
    ok(__LOC__, isPromise(p))
    eq(__LOC__, 0, await p)
    ok(__LOC__, source(serverAction)->String.includes("'use server'"))
  })

  test("wrapper with a directive keeps it", () => {
    eq(__LOC__, 0, cachedFn(3))
    ok(__LOC__, source(cachedFn)->String.includes("'use cache'"))
  })

  testAsync("returned async wrappers stay async", async () => {
    let wrapper = makeAsyncWrapper()
    let p = wrapper(3)
    ok(__LOC__, isPromise(p))
    eq(__LOC__, 0, await p)

    let action = makeServerAction()
    let p = action(3)
    ok(__LOC__, isPromise(p))
    eq(__LOC__, 0, await p)
    ok(__LOC__, source(action)->String.includes("'use server'"))
  })

  test("returned wrapper with a directive keeps it", () => {
    let cached = makeCachedFn()
    eq(__LOC__, 0, cached(3))
    ok(__LOC__, source(cached)->String.includes("'use cache'"))
  })

  testAsync("async function applied to an external call", () => resolvesTo(__LOC__, config, 42))

  testAsync("async function applied to an arithmetic primitive", () => resolvesTo(__LOC__, sum, 3))

  testAsync("async function applied to a wrapped primitive", () =>
    resolvesTo(__LOC__, kind, #number)
  )

  testAsync("async function applied to a unit call", async () => {
    await resolvesTo(__LOC__, pushResult, ())
    eq(__LOC__, [1], pushed)
  })

  testAsync("value piped into an async function", () => resolvesTo(__LOC__, piped, 42))

  testAsync("async function with a directive applied where it is defined", () =>
    resolvesTo(__LOC__, serverConfig, 42)
  )

  // The checked-in output pins the directive, which leaves no trace at runtime
  test("function with a directive applied where it is defined", () => {
    eq(__LOC__, 42, cachedConfig)
  })

  testAsync("async function awaiting its parameter, applied in a plain function", () =>
    resolvesTo(__LOC__, awaitThrough(Promise.resolve(1)), 1)
  )
})

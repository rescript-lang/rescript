open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

module type Source = {
  let value: promise<int>
}

// A functor body is not async, but an async function in it is.
module MakeReader = (Source: Source) => {
  let read = async () => (await Source.value) + 1

  let readLength = async () => {
    module L = await List
    L.length(list{await Source.value})
  }

  module Nested = {
    let read = async () => await Source.value
  }
}

module Reader = MakeReader({
  let value = Promise.resolve(41)
})

// A functor inside an async function does not change its context.
let readInAsyncFunction = async value => {
  module LocalReader = MakeReader({
    let value = value
  })
  (await LocalReader.read()) + (await value)
}

// After a functor in an async function, the context is the function's again.
let readAfterLocalFunctor = async value => {
  module MakeLocalReader = (Source: Source) => {
    let read = async () => (await Source.value) * 2
  }
  module LocalReader = MakeLocalReader({
    let value = value
  })
  (await LocalReader.read()) + (await value)
}

// The top level of a module outside functors, including the argument of a
// functor application, is still an async context.
let topLevel = await Promise.resolve(1)

module ReaderOfAwaited = MakeReader({
  let value = Promise.resolve(await Promise.resolve(2))
})

describe(__MODULE__, () => {
  testAsync("async function in a functor body", async () => {
    eq(__LOC__, 42, await Reader.read())
    eq(__LOC__, 41, await Reader.Nested.read())
  })

  testAsync("dynamic import in an async function in a functor body", async () => {
    eq(__LOC__, 1, await Reader.readLength())
  })

  testAsync("functor applied in an async function", async () => {
    eq(__LOC__, 5, await readInAsyncFunction(Promise.resolve(2)))
  })

  testAsync("await after a functor in an async function", async () => {
    eq(__LOC__, 6, await readAfterLocalFunctor(Promise.resolve(2)))
  })

  testAsync("await at the top level", async () => {
    eq(__LOC__, 1, topLevel)
    eq(__LOC__, 3, await ReaderOfAwaited.read())
  })
})

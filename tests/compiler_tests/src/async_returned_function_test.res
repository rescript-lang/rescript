open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

// Each arrow is a function of its own: the inner arrows below must compile to
// JS functions that are async exactly when they are async in the source.
let addAsync = async x => async y => (await y) + x

let addPlain = async x => y => y + x

let addThree = async x => y => async z => (await y) + await z + x

describe(__MODULE__, () => {
  testAsync("async function returning an async function", async () => {
    let add = await addAsync(1)
    eq(__LOC__, 3, await add(Promise.resolve(2)))
  })

  testAsync("async function returning a plain function", async () => {
    let add = await addPlain(1)
    eq(__LOC__, 3, add(2))
  })

  testAsync("plain function between two async functions", async () => {
    let add = await addThree(1)
    eq(__LOC__, 6, await add(Promise.resolve(2))(Promise.resolve(3)))
  })
})

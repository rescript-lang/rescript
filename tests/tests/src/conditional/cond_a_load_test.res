open Mocha

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

let loadCondANone = async () => {
  module M = await Cond_a_none
  M.A.u
}

let raisesAssertFailure = async load =>
  switch await load() {
  | _ => false
  | exception Assert_failure(_) => true
  }

describe(__MODULE__, () => {
  testAsync("cond_a loads as a module whose body throws Assert_failure", async () => {
    let ok = await raisesAssertFailure(() => import(Cond_a.u))
    if !ok {
      failwith("cond_a did not throw Assert_failure on load")
    }
  })
  testAsync("cond_a_none loads as a module whose body throws Assert_failure", async () => {
    let ok = await raisesAssertFailure(loadCondANone)
    if !ok {
      failwith("cond_a_none did not throw Assert_failure on load")
    }
  })
})

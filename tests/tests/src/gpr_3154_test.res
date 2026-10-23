open Mocha
open Test_utils

let get = (dict, key) =>
  if Dict.has(dict, key) {
    Some(Dict.getUnsafe(dict, key))
  } else {
    None
  }

describe(__MODULE__, () => {
  test("Dict None value handling", () => {
    let d = Dict.make()
    Dict.set(d, "foo", None)
    switch get(d, "foo") {
    | Some(None) => ok(__LOC__, true)
    | _ => ok(__LOC__, false)
    }
  })

  test("Dict get with None", () => {
    let d0 = Dict.make()
    Dict.set(d0, "foo", None)
    eq(__LOC__, get(d0, "foo"), Some(None))
  })
})

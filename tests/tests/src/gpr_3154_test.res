open Mocha
open Test_utils

module J = Js.Dict

describe(__MODULE__, () => {
  test("Js.Dict None value handling", () => {
    let d = Js.Dict.empty()
    J.set(d, "foo", None)
    switch J.get(d, "foo") {
    | Some(None) => ok(__LOC__, true)
    | _ => ok(__LOC__, false)
    }
  })

  test("Js.Dict get with None", () => {
    let d0 = Js.Dict.empty()
    J.set(d0, "foo", None)
    eq(__LOC__, J.get(d0, "foo"), Some(None))
  })
})

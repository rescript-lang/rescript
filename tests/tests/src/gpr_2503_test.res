open Mocha
open Test_utils

/* TODO: */

@obj external make: (~foo: [#a | #b]=?, unit) => _ = ""

let makeWrapper = (~foo=?, ()) => Js.log(make(~foo?, ()))

@obj external make2: (~foo: [#a | #b], unit) => _ = ""

let makeWrapper2 = (foo, ()) => Js.log(make2(~foo, ()))

let _ = makeWrapper2(#a, ())

@obj external make3: (~foo: [#a | #b]=?, unit) => _ = ""

let makeWrapper3 = (~foo=?, ()) => {
  Js.log(2)
  make(~foo?, ())
}

let makeWrapper4 = (foo, ()) => {
  Js.log(2)
  make(
    ~foo=?if foo > 100 {
      None
    } else if foo > 10 {
      Some(#b)
    } else {
      Some(#a)
    },
    (),
  )
}

describe(__MODULE__, () => {
  test("gpr_2503 polymorphic variant optional parameter test", () => {
    ok(__LOC__, Js.eqUndefined(#a, makeWrapper3(~foo=#a, ())["foo"]))
    ok(__LOC__, Js.undefined == makeWrapper3()["foo"])
    ok(__LOC__, Js.eqUndefined(#a, makeWrapper4(1, ())["foo"]))
    ok(__LOC__, Js.eqUndefined(#b, makeWrapper4(11, ())["foo"]))
    ok(__LOC__, Js.undefined == makeWrapper4(111, ())["foo"])
  })
})

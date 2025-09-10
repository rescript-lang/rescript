open Mocha
open Test_utils

type element
type dom
@send @return(nullable) external getElementById: (dom, string) => option<element> = "getElementById"

let test_return_nullable = dom => {
  let elem = dom->getElementById("haha")
  switch elem {
  | None => 1
  | Some(ui) =>
    Js.log(ui)
    2
  }
}

let f = (x, y) => {
  Js.log("no inline")
  Js.Nullable.return(x + y)
}

describe(__MODULE__, () => {
  test("Js.Nullable operations", () => {
    eq(__LOC__, Js.isNullable(Js.Nullable.return(3)), false)
    eq(__LOC__, Js.isNullable(f(1, 2)), false)
    eq(__LOC__, Js.isNullable(%raw("null")), true)

    let null2 = Js.Nullable.return(3)
    let null = null2
    eq(__LOC__, Js.isNullable(null), false)
  })
})

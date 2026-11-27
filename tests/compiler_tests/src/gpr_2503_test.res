open Mocha
open Test_utils

/* TODO: */

@get external foo: {..} => option<[#a | #b]> = "foo"

@obj external make: (~foo: [#a | #b]=?, unit) => {..} = ""

let makeWrapper = (~foo=?, ()) => Console.log(make(~foo?, ()))

@obj external make2: (~foo: [#a | #b], unit) => {..} = ""

let makeWrapper2 = (foo, ()) => Console.log(make2(~foo, ()))

let _ = makeWrapper2(#a, ())

@obj external make3: (~foo: [#a | #b]=?, unit) => {..} = ""

let makeWrapper3 = (~foo=?, ()) => {
  Console.log(2)
  make(~foo?, ())
}

let makeWrapper4 = (foo, ()) => {
  Console.log(2)
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
    ok(__LOC__, makeWrapper3(~foo=#a, ())->foo == Some(#a))
    ok(__LOC__, makeWrapper3()->foo == None)
    ok(__LOC__, makeWrapper4(1, ())->foo == Some(#a))
    ok(__LOC__, makeWrapper4(11, ())->foo == Some(#b))
    ok(__LOC__, makeWrapper4(111, ())->foo == None)
  })
})

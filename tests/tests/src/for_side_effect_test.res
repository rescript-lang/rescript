let tst = () =>
  for i in {
    Js.log("hi")
    0
  } to {
    Js.log("hello")
    3
  } {
    ()
  }

let test2 = () => {
  let v = ref(0)
  for i in {
    v := 3
    0
  } to {
    v := 10
    1
  } {
    ()
  }
  v.contents
}

open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("for_order", () => {
    eq(__LOC__, 10, test2())
  })
})

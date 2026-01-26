open Mocha
open Test_utils

@send external map: (Js_array2.t<'a>, 'a => 'b) => Js_array2.t<'b> = "map"

describe(__MODULE__, () => {
  test("ffi array test", () => {
    eq(__LOC__, map([1, 2, 3, 4], x => x + 1), [2, 3, 4, 5])
  })
})

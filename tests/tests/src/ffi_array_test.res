open Mocha
open Test_utils

@send external map: (array<'a>, 'a => 'b) => array<'b> = "map"

describe(__MODULE__, () => {
  test("ffi array test", () => {
    eq(__LOC__, map([1, 2, 3, 4], x => x + 1), [2, 3, 4, 5])
  })
})

open Mocha
open Test_utils

let f = (x, y) => Pervasives.compare(x + y, y + x)

let f2 = (x, y) => Pervasives.compare(x + y, y)

let f3 = (x, y) => Pervasives.compare((x: int), y)

let f4 = (x, y) => min((x: int), y)

let f5_min = (x, y) => min(x, y)
let f5_max = (x, y) => max(x, y)

describe(__MODULE__, () => {
  test("min/max operations with options", () => {
    eq(__LOC__, f5_min(None, Some(3)), None)
    eq(__LOC__, f5_min(Some(3), None), None)
    eq(__LOC__, f5_max(Some(3), None), Some(3))
    eq(__LOC__, f5_max(None, Some(3)), Some(3))
    ok(__LOC__, Some(5) >= None)
    ok(__LOC__, None <= Some(5))
    ok(__LOC__, !(None == Some(5)))
    ok(__LOC__, None != Some(5))
  })
})

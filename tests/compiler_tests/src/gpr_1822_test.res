open Mocha
open Test_utils

type shape =
  | Circle(int)
  | Rectangle(int, int)

describe(__MODULE__, () => {
  test("gpr_1822_test", () => {
    let myShape = Circle(10)
    let area = switch myShape {
    | Circle(r) => Int.toFloat(r * r) *. 3.14
    | Rectangle(w, h) => Int.toFloat(w * h)
    }

    eq(__LOC__, area, 314.)
  })
})

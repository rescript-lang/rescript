open Mocha
open Test_utils

type shape =
  | Circle(int)
  | Rectangle(int, int)

describe(__MODULE__, () => {
  test("gpr_1822_test", () => {
    let myShape = Circle(10)
    let area = switch myShape {
    | Circle(r) => float_of_int(r * r) *. 3.14
    | Rectangle(w, h) => float_of_int(w * h)
    }

    eq(__LOC__, area, 314.)
  })
})

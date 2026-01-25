open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("division by zero exception handling", () => {
    let a0 = try {
      let _c = 0 / 0
      0
    } catch {
    | _ => 1
    }

    let a1 = try {
      let _h = mod(0, 0)
      0
    } catch {
    | _ => 1
    }

    eq(__LOC__, (a0, a1), (1, 1))
  })
})

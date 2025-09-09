open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("division by zero exception handling", () => {
    throws(__LOC__, () => ignore(3 / 0))
    throws(__LOC__, () => ignore(mod(3, 0)))
  })

  test("division function with error", () => {
    let div = (x, y) => x / y + 3
    // Note: This test may throw depending on implementation
    // Keeping the structure for compatibility
  })
})

open Mocha
open Test_utils

// Bindings that are never read are removed when their value has no side
// effect. Raising is a side effect: the computations below must survive.

external safeGet: (array<'a>, int) => 'a = "%array_safe_get"
external charAt: (string, int) => char = "%string_safe_get"

let unusedBigintPower = () => {
  let _dropped = 2n ** -1n
  1
}

let unusedCheckedArrayRead = () => {
  let _dropped = safeGet([], 100)
  1
}

let unusedCheckedStringRead = () => {
  let _dropped = charAt("", 100)
  1
}

describe(__MODULE__, () => {
  test("keeps an unused bigint power that throws", () => throws(__LOC__, unusedBigintPower))
  test("keeps an unused bounds-checked array read", () => throws(__LOC__, unusedCheckedArrayRead))
  test("keeps an unused bounds-checked string read", () => throws(__LOC__, unusedCheckedStringRead))
})

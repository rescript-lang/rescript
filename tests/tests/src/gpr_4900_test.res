open Mocha
open Test_utils

type show = No | After(int) | Yes

describe(__MODULE__, () => {
  test("gpr_4900_test", () => {
    let showToJs = x =>
      switch x {
      | Yes | After(_) => true
      | No => false
      }

    eq(__LOC__, showToJs(Yes), true)
    eq(__LOC__, showToJs(No), false)
    eq(__LOC__, showToJs(After(3)), true)
  })
})

open Mocha
open Test_utils

type t =
  | A
  | B
  | C(int)

describe(__MODULE__, () => {
  test("gpr_4924_test", () => {
    let u = b =>
      switch b {
      | A => 0
      | B | C(_) => 1
      }

    let u1 = b =>
      switch b {
      | A => true
      | B | C(_) => false
      }

    let u2 = b =>
      switch b {
      | A => false
      | B | C(_) => true
      }

    eq(__LOC__, u2(A), false)
    eq(__LOC__, u2(B), true)
    eq(__LOC__, u2(C(2)), true)
    let u3 = b =>
      switch b {
      | A => 3
      | B | C(_) => 4
      }

    let u4 = b =>
      switch b {
      | A => 3
      | _ => 4
      }

    let u5 = b =>
      switch b {
      | A => false
      | _ => true
      }

    let u6 = b =>
      switch b {
      | A => true
      | _ => false
      }
  })
})

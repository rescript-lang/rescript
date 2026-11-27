open Mocha
open Test_utils

// Constant shifts must fold to what JS computes: JS uses only the low
// 5 bits of the shift count (#9410).

let shl = (x, n) => Int.shiftLeft(x, n)
let shr = (x, n) => Int.shiftRight(x, n)
let shru = (x, n) => Int.shiftRightUnsigned(x, n)

describe(__MODULE__, () => {
  test("shiftLeft by 32 or more", () => {
    eq(__LOC__, Int.shiftLeft(3, 32), 3)
    eq(__LOC__, Int.shiftLeft(3, 33), 6)
    eq(__LOC__, Int.shiftLeft(3, -31), 6)
    eq(__LOC__, 3 << 32, 3)
    eq(__LOC__, shl(3, 33), 6)
  })

  test("shiftRight by 32 or more", () => {
    eq(__LOC__, Int.shiftRight(-8, 32), -8)
    eq(__LOC__, Int.shiftRight(-8, 33), -4)
    eq(__LOC__, -8 >> 32, -8)
    eq(__LOC__, shr(-8, 33), -4)
  })

  test("shiftRightUnsigned by 32 or more", () => {
    eq(__LOC__, Int.shiftRightUnsigned(-8, 32), -8)
    eq(__LOC__, Int.shiftRightUnsigned(-8, -31), 2147483644)
    eq(__LOC__, -8 >>> 32, -8)
    eq(__LOC__, shru(-8, 32), -8)
  })

  test("shiftRightUnsigned of a variable by 32 stays signed", () => {
    let x = Array.length([1, 2, 3]) - 11
    eq(__LOC__, Int.shiftRightUnsigned(x, 32), -8)
    eq(__LOC__, Int.shiftRightUnsigned(x, -31), 2147483644)
  })
})

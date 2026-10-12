// Functions whose bodies grow in size around the inlining thresholds
// (Lam_analysis.small_inline_size, the limit for constant arguments and
// exit_inline_size): the generated JS shows which calls get inlined.
open Mocha
open Test_utils

let g1 = (x, y) => x * y
let g2 = (x, y) => x * y + x
let g3 = (x, y) => x * y + x + x
let g4 = (x, y) => x * y + x + x + x
let g5 = (x, y) => x * y + x + x + x + x
let g6 = (x, y) => x * y + x + x + x + x + x
let g7 = (x, y) => x * y + x + x + x + x + x + x
let g8 = (x, y) => x * y + x + x + x + x + x + x + x
let g9 = (x, y) => x * y + x + x + x + x + x + x + x + x
let g10 = (x, y) => x * y + x + x + x + x + x + x + x + x + x
let g11 = (x, y) => x * y + x + x + x + x + x + x + x + x + x + x
let g12 = (x, y) => x * y + x + x + x + x + x + x + x + x + x + x + x

let pick = (b, x) =>
  if b {
    x + 1
  } else {
    x * 2
  }

let withVariables = (a, b) => [
  g1(a, b),
  g2(a, b),
  g3(a, b),
  g4(a, b),
  g5(a, b),
  g6(a, b),
  g7(a, b),
  g8(a, b),
  g9(a, b),
  g10(a, b),
  g11(a, b),
  g12(a, b),
]

let withConstants = () => [
  g1(2, 3),
  g2(2, 3),
  g3(2, 3),
  g4(2, 3),
  g5(2, 3),
  g6(2, 3),
  g7(2, 3),
  g8(2, 3),
  g9(2, 3),
  g10(2, 3),
  g11(2, 3),
  g12(2, 3),
]

let picked = a => (pick(true, a), pick(false, a))

describe(__LOC__, () => {
  test("calls around the inlining thresholds", () => {
    eq(__LOC__, withVariables(2, 3), [6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28])
    eq(__LOC__, withConstants(), [6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28])
    eq(__LOC__, picked(5), (6, 10))
  })
})

open Mocha
open Test_utils

// Negating a float comparison must not flip its operator: for NaN,
// !(x < y) is true while x >= y is false.
let notLt = (x: float, y: float) => !(x < y)
let notLe = (x: float, y: float) => !(x <= y)
let notGt = (x: float, y: float) => !(x > y)
let notGe = (x: float, y: float) => !(x >= y)

let log = ref(list{})

let ifNotLt = (x: float) => {
  if !(x < 0.0) {
    log := list{"then", ...log.contents}
  }
  log.contents
}

let condNotLt = (x: float) => !(x < 0.0) ? "then" : "else"

describe(__MODULE__, () => {
  test("negated comparisons with NaN", () => {
    let nan = Float.Constants.nan
    eq(__LOC__, true, notLt(nan, 0.0))
    eq(__LOC__, true, notLe(nan, 0.0))
    eq(__LOC__, true, notGt(nan, 0.0))
    eq(__LOC__, true, notGe(nan, 0.0))
    eq(__LOC__, true, notLt(0.0, nan))
  })
  test("negated comparisons with numbers", () => {
    eq(__LOC__, false, notLt(-1.0, 0.0))
    eq(__LOC__, true, notLt(1.0, 0.0))
    eq(__LOC__, false, notGe(1.0, 0.0))
  })
  test("negated comparison as a condition", () => {
    eq(__LOC__, list{"then"}, ifNotLt(Float.Constants.nan))
    eq(__LOC__, "then", condNotLt(Float.Constants.nan))
    eq(__LOC__, "else", condNotLt(-1.0))
  })
})

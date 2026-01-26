open Js.Math
open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("_E", () => {
    approxEq(__LOC__, 0.001, 2.718, _E)
  })
  test("_LN2", () => {
    approxEq(__LOC__, 0.001, 0.693, _LN2)
  })
  test("_LN10", () => {
    approxEq(__LOC__, 0.001, 2.303, _LN10)
  })
  test("_LOG2E", () => {
    approxEq(__LOC__, 0.001, 1.443, _LOG2E)
  })
  test("_LOG10E", () => {
    approxEq(__LOC__, 0.001, 0.434, _LOG10E)
  })
  test("_PI", () => {
    approxEq(__LOC__, 0.001, 3.14159, _PI)
  })
  test("_SQRT1_2", () => {
    approxEq(__LOC__, 0.001, 0.707, _SQRT1_2)
  })
  test("_SQRT2", () => {
    approxEq(__LOC__, 0.001, 1.414, _SQRT2)
  })
  test("abs_int", () => {
    eq(__LOC__, 4, abs_int(-4))
  })
  test("abs_float", () => {
    eq(__LOC__, 1.2, abs_float(-1.2))
  })
  test("acos", () => {
    approxEq(__LOC__, 0.001, 1.159, acos(0.4))
  })
  test("acosh", () => {
    approxEq(__LOC__, 0.001, 0.622, acosh(1.2))
  })
  test("asin", () => {
    approxEq(__LOC__, 0.001, 0.411, asin(0.4))
  })
  test("asinh", () => {
    approxEq(__LOC__, 0.001, 0.390, asinh(0.4))
  })
  test("atan", () => {
    approxEq(__LOC__, 0.001, 0.380, atan(0.4))
  })
  test("atanh", () => {
    approxEq(__LOC__, 0.001, 0.423, atanh(0.4))
  })
  test("atan2", () => {
    approxEq(__LOC__, 0.001, 0.588, atan2(~x=0.6, ~y=0.4, ()))
  })
  test("cbrt", () => {
    eq(__LOC__, 2., cbrt(8.))
  })
  test("unsafe_ceil_int", () => {
    eq(__LOC__, 4, unsafe_ceil_int(3.2))
  })
  test("ceil_int", () => {
    eq(__LOC__, 4, ceil_int(3.2))
  })
  test("ceil_float", () => {
    eq(__LOC__, 4., ceil_float(3.2))
  })
  test("cos", () => {
    approxEq(__LOC__, 0.001, 0.921, cos(0.4))
  })
  test("cosh", () => {
    approxEq(__LOC__, 0.001, 1.081, cosh(0.4))
  })
  test("exp", () => {
    approxEq(__LOC__, 0.001, 1.491, exp(0.4))
  })
  test("expm1", () => {
    approxEq(__LOC__, 0.001, 0.491, expm1(0.4))
  })
  test("unsafe_floor_int", () => {
    eq(__LOC__, 3, unsafe_floor_int(3.2))
  })
  test("floor_int", () => {
    eq(__LOC__, 3, floor_int(3.2))
  })
  test("floor_float", () => {
    eq(__LOC__, 3., floor_float(3.2))
  })
  test("fround", () => {
    approxEq(__LOC__, 0.001, 3.2, fround(3.2))
  })
  test("hypot", () => {
    approxEq(__LOC__, 0.001, 0.721, hypot(0.4, 0.6))
  })
  test("hypotMany", () => {
    approxEq(__LOC__, 0.001, 1.077, hypotMany([0.4, 0.6, 0.8]))
  })
  test("imul", () => {
    eq(__LOC__, 8, imul(4, 2))
  })
  test("log", () => {
    approxEq(__LOC__, 0.001, -0.916, log(0.4))
  })
  test("log1p", () => {
    approxEq(__LOC__, 0.001, 0.336, log1p(0.4))
  })
  test("log10", () => {
    approxEq(__LOC__, 0.001, -0.397, log10(0.4))
  })
  test("log2", () => {
    approxEq(__LOC__, 0.001, -1.321, log2(0.4))
  })
  test("max_int", () => {
    eq(__LOC__, 4, max_int(2, 4))
  })
  test("maxMany_int", () => {
    eq(__LOC__, 4, maxMany_int([2, 4, 3]))
  })
  test("max_float", () => {
    eq(__LOC__, 4.2, max_float(2.7, 4.2))
  })
  test("maxMany_float", () => {
    eq(__LOC__, 4.2, maxMany_float([2.7, 4.2, 3.9]))
  })
  test("min_int", () => {
    eq(__LOC__, 2, min_int(2, 4))
  })
  test("minMany_int", () => {
    eq(__LOC__, 2, minMany_int([2, 4, 3]))
  })
  test("min_float", () => {
    eq(__LOC__, 2.7, min_float(2.7, 4.2))
  })
  test("minMany_float", () => {
    eq(__LOC__, 2.7, minMany_float([2.7, 4.2, 3.9]))
  })
  test("random", () => {
    let a = random()
    eq(__LOC__, true, a >= 0. && a < 1.)
  })
  test("random_int", () => {
    let a = random_int(1, 3)
    eq(__LOC__, true, a >= 1 && a < 3)
  })
  test("unsafe_round", () => {
    eq(__LOC__, 3, unsafe_round(3.2))
  })
  test("round", () => {
    eq(__LOC__, 3., round(3.2))
  })
  test("sign_int", () => {
    eq(__LOC__, -1, sign_int(-4))
  })
  test("sign_float", () => {
    eq(__LOC__, -1., sign_float(-4.2))
  })
  test("sign_float -0", () => {
    eq(__LOC__, -0., sign_float(-0.))
  })
  test("sin", () => {
    approxEq(__LOC__, 0.001, 0.389, sin(0.4))
  })
  test("sinh", () => {
    approxEq(__LOC__, 0.001, 0.410, sinh(0.4))
  })
  test("sqrt", () => {
    approxEq(__LOC__, 0.001, 0.632, sqrt(0.4))
  })
  test("tan", () => {
    approxEq(__LOC__, 0.001, 0.422, tan(0.4))
  })
  test("tanh", () => {
    approxEq(__LOC__, 0.001, 0.379, tanh(0.4))
  })
  test("unsafe_trunc", () => {
    eq(__LOC__, 4, unsafe_trunc(4.2156))
  })
  test("trunc", () => {
    eq(__LOC__, 4., trunc(4.2156))
  })
})

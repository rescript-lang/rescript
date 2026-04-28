open Mocha
open Test_utils
open Js.Float

describe(__MODULE__, () => {
  test("_NaN <> _NaN", () => eq(__LOC__, false, _NaN == _NaN))
  test("isNaN - _NaN", () => eq(__LOC__, true, isNaN(_NaN)))
  test("isNaN - 0.", () => eq(__LOC__, false, isNaN(0.)))
  test("isFinite - infinity", () => eq(__LOC__, false, isFinite(infinity)))
  test("isFinite - neg_infinity", () => eq(__LOC__, false, isFinite(neg_infinity)))
  test("isFinite - _NaN", () => eq(__LOC__, false, isFinite(_NaN)))
  test("isFinite - 0.", () => eq(__LOC__, true, isFinite(0.)))
  test("toExponential", () => eq(__LOC__, "1.23456e+2", toExponential(123.456)))
  test("toExponential - large number", () => eq(__LOC__, "1.2e+21", toExponential(1.2e21)))
  test("toExponentialWithPrecision - digits:2", () =>
    eq(__LOC__, "1.23e+2", toExponentialWithPrecision(123.456, ~digits=2))
  )
  test("toExponentialWithPrecision - digits:4", () =>
    eq(__LOC__, "1.2346e+2", toExponentialWithPrecision(123.456, ~digits=4))
  )
  test("toExponentialWithPrecision - digits:20", () =>
    eq(__LOC__, "0.00000000000000000000e+0", toExponentialWithPrecision(0., ~digits=20))
  )
  test("toExponentialWithPrecision - digits:101", () => {
    throws(__LOC__, () => toExponentialWithPrecision(0., ~digits=101))
  })
  test("toExponentialWithPrecision - digits:-1", () => {
    throws(__LOC__, () => toExponentialWithPrecision(0., ~digits=-1))
  })
  test("toFixed", () => eq(__LOC__, "123", toFixed(123.456)))
  test("toFixed - large number", () => eq(__LOC__, "1.2e+21", toFixed(1.2e21)))
  test("toFixedWithPrecision - digits:2", () =>
    eq(__LOC__, "123.46", toFixedWithPrecision(123.456, ~digits=2))
  )
  test("toFixedWithPrecision - digits:4", () =>
    eq(__LOC__, "123.4560", toFixedWithPrecision(123.456, ~digits=4))
  )
  test("toFixedWithPrecision - digits:20", () =>
    eq(__LOC__, "0.00000000000000000000", toFixedWithPrecision(0., ~digits=20))
  )
  test("toFixedWithPrecision - digits:101", () => {
    throws(__LOC__, () => toFixedWithPrecision(0., ~digits=101))
  })
  test("toFixedWithPrecision - digits:-1", () => {
    throws(__LOC__, () => toFixedWithPrecision(0., ~digits=-1))
  })
  test("toPrecision", () => eq(__LOC__, "123.456", toPrecision(123.456)))
  test("toPrecision - large number", () => eq(__LOC__, "1.2e+21", toPrecision(1.2e21)))
  test("toPrecisionWithPrecision - digits:2", () =>
    eq(__LOC__, "1.2e+2", toPrecisionWithPrecision(123.456, ~digits=2))
  )
  test("toPrecisionWithPrecision - digits:4", () =>
    eq(__LOC__, "123.5", toPrecisionWithPrecision(123.456, ~digits=4))
  )
  test("toPrecisionWithPrecision - digits:20", () =>
    eq(__LOC__, "0.0000000000000000000", toPrecisionWithPrecision(0., ~digits=20))
  )
  test("toPrecisionWithPrecision - digits:101", () => {
    throws(__LOC__, () => toPrecisionWithPrecision(0., ~digits=101))
  })
  test("toPrecisionWithPrecision - digits:-1", () => {
    throws(__LOC__, () => toPrecisionWithPrecision(0., ~digits=-1))
  })
  test("toString", () => eq(__LOC__, "1.23", toString(1.23)))
  test("toString - large number", () => eq(__LOC__, "1.2e+21", toString(1.2e21)))
  test("toStringWithRadix - radix:2", () =>
    eq(
      __LOC__,
      "1111011.0111010010111100011010100111111011111001110111",
      toStringWithRadix(123.456, ~radix=2),
    )
  )
  test("toStringWithRadix - radix:16", () =>
    eq(__LOC__, "7b.74bc6a7ef9dc", toStringWithRadix(123.456, ~radix=16))
  )
  test("toStringWithRadix - radix:36", () => eq(__LOC__, "3f", toStringWithRadix(123., ~radix=36)))
  test("toStringWithRadix - radix:37", () => {
    throws(__LOC__, () => toStringWithRadix(0., ~radix=37))
  })
  test("toStringWithRadix - radix:1", () => {
    throws(__LOC__, () => toStringWithRadix(0., ~radix=1))
  })
  test("toStringWithRadix - radix:-1", () => {
    throws(__LOC__, () => toStringWithRadix(0., ~radix=-1))
  })
  test("fromString - 123", () => eq(__LOC__, 123., fromString("123")))
  test("fromString - 12.3", () => eq(__LOC__, 12.3, fromString("12.3")))
  test("fromString - empty string", () => eq(__LOC__, 0., fromString("")))
  test("fromString - 0x11", () => eq(__LOC__, 17., fromString("0x11")))
  test("fromString - 0b11", () => eq(__LOC__, 3., fromString("0b11")))
  test("fromString - 0o11", () => eq(__LOC__, 9., fromString("0o11")))
  test("fromString - invalid string", () => eq(__LOC__, true, isNaN(fromString("foo"))))
})

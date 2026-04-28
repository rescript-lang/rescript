open Js_int
open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("toExponential", () => {
    eq(__LOC__, "1.23456e+5", toExponential(123456))
  })

  test("toExponentialWithPrecision - digits:2", () => {
    eq(__LOC__, "1.23e+5", toExponentialWithPrecision(123456, ~digits=2))
  })

  test("toExponentialWithPrecision - digits:4", () => {
    eq(__LOC__, "1.2346e+5", toExponentialWithPrecision(123456, ~digits=4))
  })

  test("toExponentialWithPrecision - digits:20", () => {
    eq(__LOC__, "0.00000000000000000000e+0", toExponentialWithPrecision(0, ~digits=20))
  })

  test("toExponentialWithPrecision - digits:101 throws", () => {
    throws(__LOC__, () => ignore(toExponentialWithPrecision(0, ~digits=101)))
  })

  test("toExponentialWithPrecision - digits:-1 throws", () => {
    throws(__LOC__, () => ignore(toExponentialWithPrecision(0, ~digits=-1)))
  })

  test("toPrecision", () => {
    eq(__LOC__, "123456", toPrecision(123456))
  })

  test("toPrecisionWithPrecision - digits:2", () => {
    eq(__LOC__, "1.2e+5", toPrecisionWithPrecision(123456, ~digits=2))
  })

  test("toPrecisionWithPrecision - digits:4", () => {
    eq(__LOC__, "1.235e+5", toPrecisionWithPrecision(123456, ~digits=4))
  })

  test("toPrecisionWithPrecision - digits:20", () => {
    eq(__LOC__, "0.0000000000000000000", toPrecisionWithPrecision(0, ~digits=20))
  })

  test("toPrecisionWithPrecision - digits:101 throws", () => {
    throws(__LOC__, () => ignore(toPrecisionWithPrecision(0, ~digits=101)))
  })

  test("toPrecisionWithPrecision - digits:-1 throws", () => {
    throws(__LOC__, () => ignore(toPrecisionWithPrecision(0, ~digits=-1)))
  })

  test("toString", () => {
    eq(__LOC__, "123", toString(123))
  })

  test("toStringWithRadix - radix:2", () => {
    eq(__LOC__, "11110001001000000", toStringWithRadix(123456, ~radix=2))
  })

  test("toStringWithRadix - radix:16", () => {
    eq(__LOC__, "1e240", toStringWithRadix(123456, ~radix=16))
  })

  test("toStringWithRadix - radix:36", () => {
    eq(__LOC__, "2n9c", toStringWithRadix(123456, ~radix=36))
  })

  test("toStringWithRadix - radix:37 throws", () => {
    throws(__LOC__, () => ignore(toStringWithRadix(0, ~radix=37)))
  })

  test("toStringWithRadix - radix:1 throws", () => {
    throws(__LOC__, () => ignore(toStringWithRadix(0, ~radix=1)))
  })

  test("toStringWithRadix - radix:-1 throws", () => {
    throws(__LOC__, () => ignore(toStringWithRadix(0, ~radix=-1)))
  })
})

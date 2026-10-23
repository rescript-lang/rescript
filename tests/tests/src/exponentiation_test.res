open Mocha
open Test_utils

external jsPow: (float, float) => float = "Math.pow"

let intPow: (int, int) => int = %raw(`(a, b) => Math.pow(a, b) | 0`)
let four: int = %raw(`4`)

let floatPowDiv = (base: float, numerator: float, denominator: float) =>
  base ** (numerator /. denominator)
let floatPowMul = (base: float, left: float, right: float) => base ** (left *. right)
let floatPowMod = (base: float, value: float, modulus: float) => base ** (value % modulus)
let bigintPowMul = (base: bigint, left: bigint, right: bigint) => base ** (left * right)

describe(__MODULE__, () => {
  test("exponentiation operations", () => {
    eq(__LOC__, 2. ** 3. ** 2., jsPow(2., jsPow(3., 2.)))
    eq(__LOC__, 2. ** -3. ** 2., jsPow(2., jsPow(-3., 2.)))
    eq(__LOC__, (2. ** 3.) ** 2., jsPow(jsPow(2., 3.), 2.))
    eq(__LOC__, -2. ** 2., jsPow(-2., 2.))

    eq(__LOC__, 2 ** 3 ** 2, intPow(2, intPow(3, 2)))
    eq(__LOC__, 2 ** -3 ** 2, intPow(2, intPow(-3, 2)))
    eq(__LOC__, (2 ** 3) ** 2, intPow(intPow(2, 3), 2))
    eq(__LOC__, -2 ** 31, intPow(-2, 31))
    eq(__LOC__, 2 ** 32, intPow(2, 32))
    eq(__LOC__, 2147483647 ** 2, intPow(2147483647, 2))
    eq(__LOC__, -2147483648 ** 2, intPow(-2147483648, 2))

    eq(__LOC__, 4 ** 4, four ** four)

    eq(__LOC__, floatPowDiv(2., 0., 10000.), 1.)
    eq(__LOC__, floatPowMul(2., 3., 4.), 4096.)
    eq(__LOC__, floatPowMod(2., 5., 3.), 4.)
    eq(__LOC__, bigintPowMul(2n, 3n, 2n), 64n)
  })
})

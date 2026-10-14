open Mocha
open Test_utils

let addZero = (x: int) => x + 0
let zeroAdd = (x: int) => 0 + x
let subZero = (x: int) => x - 0
let mulOne = (x: int) => x * 1
let oneMul = (x: int) => 1 * x
let divOne = (x: int) => x / 1

let calls = ref(0)
let next = () => {
  calls := calls.contents + 1
  calls.contents
}

let effectful = () => next() * 1 + 0

describe(__MODULE__, () => {
  test("identity operations return the operand", () => {
    eq(__LOC__, addZero(7), 7)
    eq(__LOC__, zeroAdd(-7), -7)
    eq(__LOC__, subZero(2147483647), 2147483647)
    eq(__LOC__, mulOne(-2147483648), -2147483648)
    eq(__LOC__, oneMul(42), 42)
    eq(__LOC__, divOne(-5), -5)
  })
  test("the operand is evaluated once", () => {
    eq(__LOC__, effectful(), 1)
    eq(__LOC__, calls.contents, 1)
  })
})

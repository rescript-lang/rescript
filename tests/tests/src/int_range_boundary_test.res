open Mocha
open Test_utils

type response = {status: int}
type status = Status(int)

let upperRange = value =>
  switch value {
  | 2147483645 | 2147483646 | 2147483647 => true
  | _ => false
  }

let lowerRange = value =>
  switch value {
  | -2147483648 | -2147483647 | -2147483646 => true
  | _ => false
  }

let recordRange = value =>
  switch value {
  | {status: 2147483645 | 2147483646 | 2147483647} => true
  | _ => false
  }

let tupleRange = value =>
  switch value {
  | (2147483645 | 2147483646 | 2147483647, true) => true
  | _ => false
  }

let variantRange = value =>
  switch value {
  | Status(2147483645 | 2147483646 | 2147483647) => true
  | _ => false
  }

let arrayRange = value =>
  switch value {
  | [2147483645 | 2147483646 | 2147483647] => true
  | _ => false
  }

let callRange = value =>
  switch value() {
  | 2147483645 | 2147483646 | 2147483647 => true
  | _ => false
  }

let guardedRange = (value, enabled) =>
  switch value {
  | 2147483645 | 2147483646 | 2147483647 if enabled => true
  | _ => false
  }

let bothRanges = value =>
  switch value {
  | -2147483648 | -2147483647 | -2147483646 => -1
  | 2147483645 | 2147483646 | 2147483647 => 1
  | _ => 0
  }

let separateCases = value =>
  switch value {
  | -2147483648 => 1
  | -2147483647 => 2
  | -2147483646 => 3
  | 2147483645 => 4
  | 2147483646 => 5
  | 2147483647 => 6
  | _ => 0
  }

let samples = [
  (-2147483648, true, false, 1),
  (-2147483647, true, false, 2),
  (-2147483646, true, false, 3),
  (-2147483645, false, false, 0),
  (-1, false, false, 0),
  (0, false, false, 0),
  (1, false, false, 0),
  (2147483644, false, false, 0),
  (2147483645, false, true, 4),
  (2147483646, false, true, 5),
  (2147483647, false, true, 6),
]

describe(__MODULE__, () => {
  test("integer ranges at both 32-bit limits", () => {
    samples->Array.forEach(
      ((value, lower, upper, _)) => {
        eq(__LOC__, upperRange(value), upper)
        eq(__LOC__, lowerRange(value), lower)
        eq(__LOC__, bothRanges(value), lower ? -1 : upper ? 1 : 0)
      },
    )
  })

  test("upper-limit ranges in nested patterns and function results", () => {
    samples->Array.forEach(
      ((value, _, upper, _)) => {
        eq(__LOC__, recordRange({status: value}), upper)
        eq(__LOC__, tupleRange((value, true)), upper)
        eq(__LOC__, tupleRange((value, false)), false)
        eq(__LOC__, variantRange(Status(value)), upper)
        eq(__LOC__, arrayRange([value]), upper)
        eq(__LOC__, arrayRange([value, value]), false)
        eq(__LOC__, arrayRange([]), false)
        eq(__LOC__, callRange(() => value), upper)
        eq(__LOC__, guardedRange(value, true), upper)
        eq(__LOC__, guardedRange(value, false), false)
      },
    )
  })

  test("distinct actions at both 32-bit limits", () => {
    samples->Array.forEach(
      ((value, _, _, expected)) => {
        eq(__LOC__, separateCases(value), expected)
      },
    )
  })
})

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

let wrappedHex = value =>
  switch value {
  | -0xFFFF_FFFF => true
  | _ => false
  }

let wrappedOctal = value =>
  switch value {
  | -0o37777777777 => true
  | _ => false
  }

let wrappedBinary = value =>
  switch value {
  | -0b1111_1111_1111_1111_1111_1111_1111_1111 => true
  | _ => false
  }

let wrappedRange = value =>
  switch value {
  | -0xFFFF_FFFF | -0xFFFF_FFFE | -0xFFFF_FFFD => true
  | _ => false
  }

let wrappedRecordRange = value =>
  switch value {
  | {status: 0xFFFF_FFFF | -0xFFFF_FFFF | -0xFFFF_FFFE | -0xFFFF_FFFD} => true
  | _ => false
  }

let wrappedSeparateCases = value =>
  switch value {
  | -0x8000_0001 => 1
  | -0xFFFF_FFFF => 2
  | -0xFFFF_FFFE => 3
  | 0xFFFF_FFFF => 4
  | 0 => 5
  | _ => 0
  }

let wrappedAliasGuard = (value, enabled) =>
  switch value {
  | -0xFFFF_FFFF if enabled => 1
  | 1 => 2
  | _ => 0
  }

let decimalAliasGuard = (value, enabled) =>
  switch value {
  | 1 if enabled => 1
  | -0xFFFF_FFFF => 2
  | _ => 0
  }

let wrappedSamples = [-2147483648, -2, -1, 0, 1, 2, 3, 4, 2147483646, 2147483647]

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

  test("wrapped nondecimal patterns match only their int32 value", () => {
    wrappedSamples->Array.forEach(
      value => {
        eq(__LOC__, wrappedHex(value), value == 1)
        eq(__LOC__, wrappedOctal(value), value == 1)
        eq(__LOC__, wrappedBinary(value), value == 1)
      },
    )
  })

  test("wrapped ranges are sorted in the int32 domain", () => {
    wrappedSamples->Array.forEach(
      value => {
        let inRange = value >= 1 && value <= 3
        eq(__LOC__, wrappedRange(value), inRange)
        eq(__LOC__, wrappedRecordRange({status: value}), value == -1 || inRange)
        let expected = switch value {
        | 2147483647 => 1
        | 1 => 2
        | 2 => 3
        | -1 => 4
        | 0 => 5
        | _ => 0
        }
        eq(__LOC__, wrappedSeparateCases(value), expected)
      },
    )
  })

  test("guards fall through between wrapped and decimal aliases", () => {
    wrappedSamples->Array.forEach(
      value => {
        [false, true]->Array.forEach(
          enabled => {
            let expected = value == 1 ? (enabled ? 1 : 2) : 0
            eq(__LOC__, wrappedAliasGuard(value, enabled), expected)
            eq(__LOC__, decimalAliasGuard(value, enabled), expected)
          },
        )
      },
    )
  })
})

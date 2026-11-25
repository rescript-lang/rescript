open Mocha
open Test_utils

/***
A dense switch on ints or chars whose highest case can't be reached keeps its
default: values above, below and between the cases reach the default arm. The
unreachable cases are the point of the test, hence the warning attributes.
*/

@inline(never) let id = x => x

@warning("-11")
let classify = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (2 | 3 | 4, _) => 1
  | (0, _) => 2
  | (5, true) => 3
  | _ => 99
  }

// Control: the failure range ends at a later case, not at the top of the list.
@warning("-11")
let classifyMiddle = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (0 | 1, _) => 1
  | (2, true) => 3
  | (3 | 4 | 5, _) => 2
  | _ => 99
  }

@warning("-11")
let classifyTable = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (0, _) => 10
  | (1, _) => 12
  | (2, _) => 11
  | (3, _) => 14
  | (4, _) => 13
  | (5, true) => 15
  | _ => 99
  }

@warning("-11")
let classifyHole = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (0, _) => 10
  | (1, _) => 12
  | (2, _) => 11
  | (3, _) => 13
  | (5, true) => 15
  | _ => 99
  }

@warning("-11")
let classifyFirst = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (0, true) => 10
  | (1, _) => 12
  | (2, _) => 11
  | (3, _) => 14
  | (4, _) => 13
  | (5, true) => 15
  | _ => 99
  }

@warning("-11")
let classifySeveral = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (0, _) => 10
  | (1, _) => 12
  | (2, _) => 11
  | (3, _) => 13
  | (4, true) => 14
  | (5, true) => 15
  | _ => 99
  }

@warning("-11")
let classifyChar = (c: char, flag: bool) =>
  switch (c, flag) {
  | (_, true) => 0
  | ('a', _) => 1
  | ('b', _) => 3
  | ('c', _) => 2
  | ('d', _) => 5
  | ('e', _) => 4
  | ('f', true) => 6
  | _ => 99
  }

@warning("-11")
let classifyCharRange = (c: char, flag: bool) =>
  switch (c, flag) {
  | (_, true) => 0
  | ('c' .. 'e', _) => 1
  | ('a', _) => 2
  | ('f', true) => 3
  | _ => 99
  }

// The gap at 3 goes to the default along with the unreachable case above it.
@warning("-11")
let classifyGapBeforeLast = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (0 | 1, _) => 1
  | (2, _) => 2
  | (4, true) => 3
  | _ => 99
  }

@warning("-11")
let classifyGapInMiddle = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (0 | 1, _) => 1
  | (3, true) => 3
  | (4, _) => 2
  | _ => 99
  }

// Consecutive unreachable cases, and the values around them, are one range of
// the default: the output tests only for the reachable cases.
@warning("-11")
let classifyTwoAfterCase = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (0, _) => 1
  | (1, true) => 2
  | (2, true) => 3
  | _ => 99
  }

@warning("-11")
let classifyTwoInMiddle = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (0, _) => 1
  | (1, true) => 2
  | (2, true) => 3
  | (3, _) => 4
  | _ => 99
  }

@warning("-11")
let classifyFirstThenCases = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (0, true) => 1
  | (1, _) => 2
  | (2, _) => 3
  | _ => 99
  }

@warning("-11")
let classifyGapAfterFirst = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (0, true) => 1
  | (2, _) => 2
  | _ => 99
  }

describe(__MODULE__, () => {
  test("a value above the cases", () => {
    eq(__LOC__, classify(id(6), false), 99)
    eq(__LOC__, classify(id(100), false), 99)
  })

  test("values below and inside the cases", () => {
    eq(__LOC__, classify(id(-1), false), 99)
    eq(__LOC__, classify(id(1), false), 99)
    eq(__LOC__, classify(id(0), false), 2)
    eq(__LOC__, classify(id(3), false), 1)
    eq(__LOC__, classify(id(5), false), 99)
    eq(__LOC__, classify(id(6), true), 0)
  })

  test("an unreachable case in the middle", () => {
    eq(__LOC__, classifyMiddle(id(0), false), 1)
    eq(__LOC__, classifyMiddle(id(2), false), 99)
    eq(__LOC__, classifyMiddle(id(3), false), 2)
    eq(__LOC__, classifyMiddle(id(7), false), 99)
  })

  test("a dense table followed by an unreachable case", () => {
    eq(__LOC__, classifyTable(id(-1), false), 99)
    eq(__LOC__, classifyTable(id(0), false), 10)
    eq(__LOC__, classifyTable(id(4), false), 13)
    eq(__LOC__, classifyTable(id(5), false), 99)
    eq(__LOC__, classifyTable(id(6), false), 99)
    eq(__LOC__, classifyTable(id(5), true), 0)
  })

  test("an unreachable case after a hole", () => {
    eq(__LOC__, classifyHole(id(-1), false), 99)
    eq(__LOC__, classifyHole(id(0), false), 10)
    eq(__LOC__, classifyHole(id(3), false), 13)
    eq(__LOC__, classifyHole(id(4), false), 99)
    eq(__LOC__, classifyHole(id(5), false), 99)
    eq(__LOC__, classifyHole(id(6), false), 99)
    eq(__LOC__, classifyHole(id(100), false), 99)
  })

  test("an unreachable first and last case", () => {
    eq(__LOC__, classifyFirst(id(-1), false), 99)
    eq(__LOC__, classifyFirst(id(0), false), 99)
    eq(__LOC__, classifyFirst(id(1), false), 12)
    eq(__LOC__, classifyFirst(id(4), false), 13)
    eq(__LOC__, classifyFirst(id(5), false), 99)
    eq(__LOC__, classifyFirst(id(6), false), 99)
    eq(__LOC__, classifyFirst(id(100), false), 99)
  })

  test("several unreachable cases at the top", () => {
    eq(__LOC__, classifySeveral(id(-1), false), 99)
    eq(__LOC__, classifySeveral(id(0), false), 10)
    eq(__LOC__, classifySeveral(id(3), false), 13)
    eq(__LOC__, classifySeveral(id(4), false), 99)
    eq(__LOC__, classifySeveral(id(5), false), 99)
    eq(__LOC__, classifySeveral(id(6), false), 99)
    eq(__LOC__, classifySeveral(id(100), false), 99)
  })

  test("a dense char table followed by an unreachable case", () => {
    eq(__LOC__, classifyChar(id('a'), false), 1)
    eq(__LOC__, classifyChar(id('e'), false), 4)
    eq(__LOC__, classifyChar(id('f'), false), 99)
    eq(__LOC__, classifyChar(id('z'), false), 99)
    eq(__LOC__, classifyChar(id('A'), false), 99)
  })

  test("a dense char range followed by an unreachable case", () => {
    eq(__LOC__, classifyCharRange(id('a'), false), 2)
    eq(__LOC__, classifyCharRange(id('b'), false), 99)
    eq(__LOC__, classifyCharRange(id('d'), false), 1)
    eq(__LOC__, classifyCharRange(id('f'), false), 99)
    eq(__LOC__, classifyCharRange(id('z'), false), 99)
    eq(__LOC__, classifyCharRange(id('A'), false), 99)
  })

  test("a gap before an unreachable last case", () => {
    eq(__LOC__, classifyGapBeforeLast(id(-1), false), 99)
    eq(__LOC__, classifyGapBeforeLast(id(0), false), 1)
    eq(__LOC__, classifyGapBeforeLast(id(1), false), 1)
    eq(__LOC__, classifyGapBeforeLast(id(2), false), 2)
    eq(__LOC__, classifyGapBeforeLast(id(3), false), 99)
    eq(__LOC__, classifyGapBeforeLast(id(4), false), 99)
    eq(__LOC__, classifyGapBeforeLast(id(5), false), 99)
  })

  test("a gap before an unreachable case in the middle", () => {
    eq(__LOC__, classifyGapInMiddle(id(-1), false), 99)
    eq(__LOC__, classifyGapInMiddle(id(0), false), 1)
    eq(__LOC__, classifyGapInMiddle(id(1), false), 1)
    eq(__LOC__, classifyGapInMiddle(id(2), false), 99)
    eq(__LOC__, classifyGapInMiddle(id(3), false), 99)
    eq(__LOC__, classifyGapInMiddle(id(4), false), 2)
    eq(__LOC__, classifyGapInMiddle(id(5), false), 99)
  })

  test("consecutive unreachable cases at the top", () => {
    eq(__LOC__, classifyTwoAfterCase(id(-1), false), 99)
    eq(__LOC__, classifyTwoAfterCase(id(0), false), 1)
    eq(__LOC__, classifyTwoAfterCase(id(1), false), 99)
    eq(__LOC__, classifyTwoAfterCase(id(2), false), 99)
    eq(__LOC__, classifyTwoAfterCase(id(3), false), 99)
  })

  test("consecutive unreachable cases in the middle", () => {
    eq(__LOC__, classifyTwoInMiddle(id(-1), false), 99)
    eq(__LOC__, classifyTwoInMiddle(id(0), false), 1)
    eq(__LOC__, classifyTwoInMiddle(id(1), false), 99)
    eq(__LOC__, classifyTwoInMiddle(id(2), false), 99)
    eq(__LOC__, classifyTwoInMiddle(id(3), false), 4)
    eq(__LOC__, classifyTwoInMiddle(id(4), false), 99)
  })

  test("an unreachable first case", () => {
    eq(__LOC__, classifyFirstThenCases(id(-1), false), 99)
    eq(__LOC__, classifyFirstThenCases(id(0), false), 99)
    eq(__LOC__, classifyFirstThenCases(id(1), false), 2)
    eq(__LOC__, classifyFirstThenCases(id(2), false), 3)
    eq(__LOC__, classifyFirstThenCases(id(3), false), 99)
  })

  test("a gap after an unreachable first case", () => {
    eq(__LOC__, classifyGapAfterFirst(id(-1), false), 99)
    eq(__LOC__, classifyGapAfterFirst(id(0), false), 99)
    eq(__LOC__, classifyGapAfterFirst(id(1), false), 99)
    eq(__LOC__, classifyGapAfterFirst(id(2), false), 2)
    eq(__LOC__, classifyGapAfterFirst(id(3), false), 99)
  })
})

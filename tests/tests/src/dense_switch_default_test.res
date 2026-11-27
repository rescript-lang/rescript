open Mocha
open Test_utils

/***
A dense switch on ints or chars whose highest case can't be reached keeps its
default: values above, below and between the cases reach the default arm. A
case guarded by `if false` can't be reached; `classifyTable` has one that an
earlier arm already covers, hence its warning attribute.
*/

@inline(never) let id = x => x

let classify = (n: int) =>
  switch n {
  | 2 .. 4 => 1
  | 0 => 2
  | 5 if false => 3
  | _ => 99
  }

// Control: the failure range ends at a later case, not at the top of the list.
let classifyMiddle = (n: int) =>
  switch n {
  | 0 .. 1 => 1
  | 2 if false => 3
  | 3 .. 5 => 2
  | _ => 99
  }

@warning("-11")
let classifyTable = (n: int, flag: bool) =>
  switch (n, flag) {
  | (_, true) => 0
  | (0, _) => 10
  | (1, _) => 11
  | (2, _) => 12
  | (3, _) => 13
  | (4, _) => 14
  | (5, true) => 15
  | _ => 99
  }

let classifyHole = (n: int) =>
  switch n {
  | 0 => 10
  | 1 => 11
  | 2 => 12
  | 3 => 13
  | 5 if false => 15
  | _ => 99
  }

let classifyFirst = (n: int) =>
  switch n {
  | 0 if false => 10
  | 1 => 11
  | 2 => 12
  | 3 => 13
  | 4 => 14
  | 5 if false => 15
  | _ => 99
  }

let classifySeveral = (n: int) =>
  switch n {
  | 0 => 10
  | 1 => 11
  | 2 => 12
  | 3 => 13
  | 4 if false => 14
  | 5 if false => 15
  | _ => 99
  }

let classifyChar = (c: char) =>
  switch c {
  | 'a' => 1
  | 'b' => 2
  | 'c' => 3
  | 'd' => 4
  | 'e' => 5
  | 'f' if false => 6
  | _ => 99
  }

// The gap at 3 goes to the default along with the unreachable case above it.
let classifyGapBeforeLast = (n: int) =>
  switch n {
  | 0 | 1 => 1
  | 2 => 2
  | 4 if false => 3
  | _ => 99
  }

let classifyGapInMiddle = (n: int) =>
  switch n {
  | 0 | 1 => 1
  | 3 if false => 3
  | 4 => 2
  | _ => 99
  }

// Consecutive unreachable cases, and the values around them, are one range of
// the default: the output tests only for the reachable cases.
let classifyTwoAfterCase = (n: int) =>
  switch n {
  | 0 => 1
  | 1 if false => 2
  | 2 if false => 3
  | _ => 99
  }

let classifyTwoInMiddle = (n: int) =>
  switch n {
  | 0 => 1
  | 1 if false => 2
  | 2 if false => 3
  | 3 => 4
  | _ => 99
  }

let classifyFirstThenCases = (n: int) =>
  switch n {
  | 0 if false => 1
  | 1 => 2
  | 2 => 3
  | _ => 99
  }

let classifyGapAfterFirst = (n: int) =>
  switch n {
  | 0 if false => 1
  | 2 => 2
  | _ => 99
  }

describe(__MODULE__, () => {
  test("a value above the cases", () => {
    eq(__LOC__, classify(id(6)), 99)
    eq(__LOC__, classify(id(100)), 99)
  })

  test("values below and inside the cases", () => {
    eq(__LOC__, classify(id(-1)), 99)
    eq(__LOC__, classify(id(1)), 99)
    eq(__LOC__, classify(id(0)), 2)
    eq(__LOC__, classify(id(3)), 1)
    eq(__LOC__, classify(id(5)), 99)
  })

  test("an unreachable case in the middle", () => {
    eq(__LOC__, classifyMiddle(id(0)), 1)
    eq(__LOC__, classifyMiddle(id(2)), 99)
    eq(__LOC__, classifyMiddle(id(3)), 2)
    eq(__LOC__, classifyMiddle(id(7)), 99)
  })

  test("a dense table followed by an unreachable case", () => {
    eq(__LOC__, classifyTable(id(-1), false), 99)
    eq(__LOC__, classifyTable(id(0), false), 10)
    eq(__LOC__, classifyTable(id(4), false), 14)
    eq(__LOC__, classifyTable(id(5), false), 99)
    eq(__LOC__, classifyTable(id(6), false), 99)
    eq(__LOC__, classifyTable(id(5), true), 0)
  })

  test("an unreachable case after a hole", () => {
    eq(__LOC__, classifyHole(id(-1)), 99)
    eq(__LOC__, classifyHole(id(0)), 10)
    eq(__LOC__, classifyHole(id(3)), 13)
    eq(__LOC__, classifyHole(id(4)), 99)
    eq(__LOC__, classifyHole(id(5)), 99)
    eq(__LOC__, classifyHole(id(6)), 99)
    eq(__LOC__, classifyHole(id(100)), 99)
  })

  test("an unreachable first and last case", () => {
    eq(__LOC__, classifyFirst(id(-1)), 99)
    eq(__LOC__, classifyFirst(id(0)), 99)
    eq(__LOC__, classifyFirst(id(1)), 11)
    eq(__LOC__, classifyFirst(id(4)), 14)
    eq(__LOC__, classifyFirst(id(5)), 99)
    eq(__LOC__, classifyFirst(id(6)), 99)
    eq(__LOC__, classifyFirst(id(100)), 99)
  })

  test("several unreachable cases at the top", () => {
    eq(__LOC__, classifySeveral(id(-1)), 99)
    eq(__LOC__, classifySeveral(id(0)), 10)
    eq(__LOC__, classifySeveral(id(3)), 13)
    eq(__LOC__, classifySeveral(id(4)), 99)
    eq(__LOC__, classifySeveral(id(5)), 99)
    eq(__LOC__, classifySeveral(id(6)), 99)
    eq(__LOC__, classifySeveral(id(100)), 99)
  })

  test("a dense char table followed by an unreachable case", () => {
    eq(__LOC__, classifyChar(id('a')), 1)
    eq(__LOC__, classifyChar(id('e')), 5)
    eq(__LOC__, classifyChar(id('f')), 99)
    eq(__LOC__, classifyChar(id('z')), 99)
    eq(__LOC__, classifyChar(id('A')), 99)
  })

  test("a gap before an unreachable last case", () => {
    eq(__LOC__, classifyGapBeforeLast(id(-1)), 99)
    eq(__LOC__, classifyGapBeforeLast(id(0)), 1)
    eq(__LOC__, classifyGapBeforeLast(id(1)), 1)
    eq(__LOC__, classifyGapBeforeLast(id(2)), 2)
    eq(__LOC__, classifyGapBeforeLast(id(3)), 99)
    eq(__LOC__, classifyGapBeforeLast(id(4)), 99)
    eq(__LOC__, classifyGapBeforeLast(id(5)), 99)
  })

  test("a gap before an unreachable case in the middle", () => {
    eq(__LOC__, classifyGapInMiddle(id(-1)), 99)
    eq(__LOC__, classifyGapInMiddle(id(0)), 1)
    eq(__LOC__, classifyGapInMiddle(id(1)), 1)
    eq(__LOC__, classifyGapInMiddle(id(2)), 99)
    eq(__LOC__, classifyGapInMiddle(id(3)), 99)
    eq(__LOC__, classifyGapInMiddle(id(4)), 2)
    eq(__LOC__, classifyGapInMiddle(id(5)), 99)
  })

  test("consecutive unreachable cases at the top", () => {
    eq(__LOC__, classifyTwoAfterCase(id(-1)), 99)
    eq(__LOC__, classifyTwoAfterCase(id(0)), 1)
    eq(__LOC__, classifyTwoAfterCase(id(1)), 99)
    eq(__LOC__, classifyTwoAfterCase(id(2)), 99)
    eq(__LOC__, classifyTwoAfterCase(id(3)), 99)
  })

  test("consecutive unreachable cases in the middle", () => {
    eq(__LOC__, classifyTwoInMiddle(id(-1)), 99)
    eq(__LOC__, classifyTwoInMiddle(id(0)), 1)
    eq(__LOC__, classifyTwoInMiddle(id(1)), 99)
    eq(__LOC__, classifyTwoInMiddle(id(2)), 99)
    eq(__LOC__, classifyTwoInMiddle(id(3)), 4)
    eq(__LOC__, classifyTwoInMiddle(id(4)), 99)
  })

  test("an unreachable first case", () => {
    eq(__LOC__, classifyFirstThenCases(id(-1)), 99)
    eq(__LOC__, classifyFirstThenCases(id(0)), 99)
    eq(__LOC__, classifyFirstThenCases(id(1)), 2)
    eq(__LOC__, classifyFirstThenCases(id(2)), 3)
    eq(__LOC__, classifyFirstThenCases(id(3)), 99)
  })

  test("a gap after an unreachable first case", () => {
    eq(__LOC__, classifyGapAfterFirst(id(-1)), 99)
    eq(__LOC__, classifyGapAfterFirst(id(0)), 99)
    eq(__LOC__, classifyGapAfterFirst(id(1)), 99)
    eq(__LOC__, classifyGapAfterFirst(id(2)), 2)
    eq(__LOC__, classifyGapAfterFirst(id(3)), 99)
  })
})

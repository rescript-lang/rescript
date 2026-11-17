open Mocha
open Test_utils

/***
Constant folding a match on a variant whose constant constructors carry
`@as(int)` must select the case of the constructor being matched, not the case
whose position in the type happens to equal the constructor's runtime value.

Each function below is called twice: once with a constant argument, which the
optimizer inlines and folds, and once through [id], whose [@inline(never)] keeps
the constant away from the fold so the emitted dispatch answers. Both must give
the expected answer. The compiled output next to this file shows which calls
fold and that the emitted dispatch is unchanged.
*/

@inline(never) let id = x => x

// Runtime values that are the swapped positions of the constructors.
type swapped = | @as(1) A | @as(0) B

let swappedName = x =>
  switch x {
  | A => "A"
  | B => "B"
  }

// Runtime values that start past the last position.
type frequency =
  | @as(3) Weekly
  | @as(4) Monthly
  | @as(5) BiWeekly
  | @as(6) Quarterly
  | @as(7) Annual

let frequencyValue = x =>
  switch x {
  | Weekly => 3
  | Monthly => 4
  | BiWeekly => 5
  | Quarterly => 6
  | Annual => 7
  }

// `@as(int)` mixed with a constructor represented by its name, a negative
// value and zero.
type mixed = | @as(1) One | Named | @as(-2) MinusTwo | @as(0) Zero

let mixedName = x =>
  switch x {
  | One => "One"
  | Named => "Named"
  | MinusTwo => "MinusTwo"
  | Zero => "Zero"
  }

// A default arm covers the constructors whose runtime value is a position.
type withDefault = | @as(1) First | @as(0) Second | @as(5) Third

let withDefaultName = x =>
  switch x {
  | First => "First"
  | _ => "other"
  }

// A coercion keeps the runtime value but not the position.
type reversed = Second2 | First2
type ordered = First2 | Second2

let orderedName = (x: ordered) =>
  switch x {
  | First2 => "First2"
  | Second2 => "Second2"
  }

// The same for constructors with a payload, including an inline record. Every
// payload constructor is at a different position in the two types.
type reversedBlocks = Leaf | Pair(int, int) | Single(string) | Point({x: int})
type orderedBlocks = Leaf | Single(string) | Point({x: int}) | Pair(int, int)

let orderedBlocksName = (x: orderedBlocks) =>
  switch x {
  | Leaf => "Leaf"
  | Single(_) => "Single"
  | Point(_) => "Point"
  | Pair(_) => "Pair"
  }

describe(__MODULE__, () => {
  test("swapped values", () => {
    eq(__LOC__, swappedName(A), "A")
    eq(__LOC__, swappedName(B), "B")
    eq(__LOC__, swappedName(id(A)), "A")
    eq(__LOC__, swappedName(id(B)), "B")
  })

  test("values past the last position", () => {
    eq(__LOC__, frequencyValue(Weekly), 3)
    eq(__LOC__, frequencyValue(Monthly), 4)
    eq(__LOC__, frequencyValue(BiWeekly), 5)
    eq(__LOC__, frequencyValue(Quarterly), 6)
    eq(__LOC__, frequencyValue(Annual), 7)
    eq(__LOC__, frequencyValue(id(Weekly)), 3)
    eq(__LOC__, frequencyValue(id(Monthly)), 4)
    eq(__LOC__, frequencyValue(id(BiWeekly)), 5)
    eq(__LOC__, frequencyValue(id(Quarterly)), 6)
    eq(__LOC__, frequencyValue(id(Annual)), 7)
  })

  test("mixed, negative and zero values", () => {
    eq(__LOC__, mixedName(One), "One")
    eq(__LOC__, mixedName(Named), "Named")
    eq(__LOC__, mixedName(MinusTwo), "MinusTwo")
    eq(__LOC__, mixedName(Zero), "Zero")
    eq(__LOC__, mixedName(id(One)), "One")
    eq(__LOC__, mixedName(id(Named)), "Named")
    eq(__LOC__, mixedName(id(MinusTwo)), "MinusTwo")
    eq(__LOC__, mixedName(id(Zero)), "Zero")
  })

  test("default arm", () => {
    eq(__LOC__, withDefaultName(First), "First")
    eq(__LOC__, withDefaultName(Second), "other")
    eq(__LOC__, withDefaultName(Third), "other")
    eq(__LOC__, withDefaultName(id(First)), "First")
    eq(__LOC__, withDefaultName(id(Second)), "other")
    eq(__LOC__, withDefaultName(id(Third)), "other")
  })

  test("switch on a constant without a call", () => {
    let direct = switch A {
    | A => "A"
    | B => "B"
    }
    eq(__LOC__, direct, "A")
    let bound = Monthly
    let boundValue = switch bound {
    | Weekly => 3
    | Monthly => 4
    | BiWeekly => 5
    | Quarterly => 6
    | Annual => 7
    }
    eq(__LOC__, boundValue, 4)
  })

  test("int switch on a coerced constructor", () => {
    let coerced = switch (A :> int) {
    | 0 => "zero"
    | 1 => "one"
    | 2 => "two"
    | 3 => "three"
    | _ => "other"
    }
    eq(__LOC__, coerced, "one")
  })

  test("coercion to a type with another constructor order", () => {
    eq(__LOC__, orderedName((First2: reversed :> ordered)), "First2")
    eq(__LOC__, orderedName((Second2: reversed :> ordered)), "Second2")
    eq(__LOC__, orderedName(id((First2: reversed :> ordered))), "First2")
    eq(__LOC__, orderedName(id((Second2: reversed :> ordered))), "Second2")
  })

  test("coercion of payload constructors to another constructor order", () => {
    eq(__LOC__, orderedBlocksName((Pair(1, 2): reversedBlocks :> orderedBlocks)), "Pair")
    eq(__LOC__, orderedBlocksName((Single("s"): reversedBlocks :> orderedBlocks)), "Single")
    eq(__LOC__, orderedBlocksName((Point({x: 1}): reversedBlocks :> orderedBlocks)), "Point")
    eq(__LOC__, orderedBlocksName((Leaf: reversedBlocks :> orderedBlocks)), "Leaf")
    eq(__LOC__, orderedBlocksName(id((Pair(1, 2): reversedBlocks :> orderedBlocks))), "Pair")
    eq(__LOC__, orderedBlocksName(id((Single("s"): reversedBlocks :> orderedBlocks))), "Single")
    eq(__LOC__, orderedBlocksName(id((Point({x: 1}): reversedBlocks :> orderedBlocks))), "Point")
    eq(__LOC__, orderedBlocksName(id((Leaf: reversedBlocks :> orderedBlocks))), "Leaf")
  })
})

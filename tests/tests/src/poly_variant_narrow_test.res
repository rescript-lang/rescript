open Mocha
open Test_utils

let handleBC = (x: [#B | #C]) =>
  switch x {
  | #B => "b"
  | #C => "c"
  }

let narrowBasic = (x: [#A | #B | #C]) =>
  switch x {
  | #A => "a"
  | ...rest => handleBC(rest)
  }

let handleCD = (x: [#C | #D]) =>
  switch x {
  | #C => 3
  | #D => 4
  }

let narrowOrPattern = (x: [#A | #B | #C | #D]) =>
  switch x {
  | #A | #B => 1
  | ...rest => handleCD(rest)
  }

let handleYZ = (x: [#Y | #Z]) =>
  switch x {
  | #Y => 10
  | #Z => 20
  }

let narrowWithAlias = (x: [#X | #Y | #Z]) =>
  switch x {
  | #X => 0
  | ...rest as r => handleYZ(rest) + handleYZ(r)
  }

let handlePQR = (x: [#P | #Q | #R]) =>
  switch x {
  | #P => 100
  | #Q => 200
  | #R => 300
  }

let narrowGuardedArm = (x: [#P | #Q | #R], cond: bool) =>
  switch x {
  | #P if cond => 1
  | ...rest => handlePQR(rest)
  }

// Nested: ...rest inside an Error(...) constructor on a result<_, poly-variant>.
let handleBarBaz = (x: [#Bar | #Baz]) =>
  switch x {
  | #Bar => 11
  | #Baz => 22
  }

let narrowNested = (r: result<int, [#Foo | #Bar | #Baz]>) =>
  switch r {
  | Error(#Foo) => 0
  | Error(...rest) => handleBarBaz(rest)
  | Ok(a) => a
  }

// Open row: scrutinee type inferred (not annotated). Residual stays open
// but can unify with a closed consumer when the remaining tags line up.
let narrowOpenRow = x =>
  switch x {
  | #A => "a"
  | ...rest => handleBC(rest)
  }

// Explicit open-row annotation — same story as the inferred case.
let narrowOpenAnnotated = (x: [> #A | #B | #C]) =>
  switch x {
  | #A => "a"
  | ...rest => handleBC(rest)
  }

// Record nesting: ...rest at a record field whose type is a poly-variant.
type wrap<'a> = {inner: 'a}

let narrowRecordField = (w: wrap<[#A | #B | #C]>) =>
  switch w {
  | {inner: #A} => "a"
  | {inner: ...rest} => handleBC(rest)
  }

// Tuple with permissive sibling: narrowing fires at the position with the rest.
let narrowTupleSibling = (p: (int, [#A | #B | #C])) =>
  switch p {
  | (0, #A) => "zero-a"
  | (_, #A) => "nonzero-a"
  | (_, ...rest) => handleBC(rest)
  }

// Multiple constructors each carrying their own poly-variant: paths keep them
// independent.
let handleXY = (x: [#X | #Y]) =>
  switch x {
  | #X => "x"
  | #Y => "y"
  }

type twoErrors<'a, 'b> = First('a) | Second('b)

let narrowMultiConstructor = (e: twoErrors<[#A | #B | #C], [#X | #Y | #Z]>) =>
  switch e {
  | First(#A) => "first-a"
  | Second(#Z) => "second-z"
  | First(...rest) => handleBC(rest)
  | Second(...rest) => handleXY(rest)
  }

// Guard on the rest arm itself — the rest arm's own matched set is empty
// (it doesn't match a specific tag syntactically), so later arms see
// everything the earlier unguarded arms matched.
let narrowRestWithGuard = (x: [#A | #B | #C], skip: bool) =>
  switch x {
  | #A => 1
  | ...rest if skip => 99
  | ...rest => handleBC(rest) == "b" ? 2 : 3
  }

// Variant within variant: ...rest inside #Outer(#Inner(...)).
let handleInnerBC = (x: [#InnerB | #InnerC]) =>
  switch x {
  | #InnerB => "ib"
  | #InnerC => "ic"
  }

let narrowVariantInVariant = (x: [#Outer([#InnerA | #InnerB | #InnerC])]) =>
  switch x {
  | #Outer(#InnerA) => "ia"
  | #Outer(...rest) => handleInnerBC(rest)
  }

// row_fixed via prenex-quantified type annotation. The scrutinee's row is
// universally quantified inside the body, so row_more is a Tunivar (which
// makes row_fixed trigger in Btype.row_fixed). Narrowing must still compute
// the correct residual; here we pass the residual to a consumer expecting
// another open-row type.
let handleOpenBC: 'b. ([> #B | #C] as 'b) => string = x =>
  switch x {
  | #B => "b"
  | #C => "c"
  | _ => "other"
  }

let narrowFixedRow: 'a. ([> #A | #B | #C] as 'a) => string = x =>
  switch x {
  | #A => "a"
  | ...rest => handleOpenBC(rest)
  }

describe("poly variant narrow", () => {
  test("basic single-tag subtraction", () => {
    eq(__LOC__, narrowBasic(#A), "a")
    eq(__LOC__, narrowBasic(#B), "b")
    eq(__LOC__, narrowBasic(#C), "c")
  })

  test("or-pattern subtracts both tags", () => {
    eq(__LOC__, narrowOrPattern(#A), 1)
    eq(__LOC__, narrowOrPattern(#B), 1)
    eq(__LOC__, narrowOrPattern(#C), 3)
    eq(__LOC__, narrowOrPattern(#D), 4)
  })

  test("...rest as r binds both names", () => {
    eq(__LOC__, narrowWithAlias(#X), 0)
    eq(__LOC__, narrowWithAlias(#Y), 20)
    eq(__LOC__, narrowWithAlias(#Z), 40)
  })

  test("guarded arms are not subtracted", () => {
    eq(__LOC__, narrowGuardedArm(#P, true), 1)
    eq(__LOC__, narrowGuardedArm(#P, false), 100)
    eq(__LOC__, narrowGuardedArm(#Q, true), 200)
    eq(__LOC__, narrowGuardedArm(#R, false), 300)
  })

  test("nested: ...rest inside Error(...)", () => {
    eq(__LOC__, narrowNested(Error(#Foo)), 0)
    eq(__LOC__, narrowNested(Error(#Bar)), 11)
    eq(__LOC__, narrowNested(Error(#Baz)), 22)
    eq(__LOC__, narrowNested(Ok(42)), 42)
  })

  test("open row: residual stays open", () => {
    eq(__LOC__, narrowOpenRow(#A), "a")
    eq(__LOC__, narrowOpenRow(#B), "b")
    eq(__LOC__, narrowOpenRow(#C), "c")
  })

  test("explicit [> ...] annotation narrows", () => {
    eq(__LOC__, narrowOpenAnnotated(#A), "a")
    eq(__LOC__, narrowOpenAnnotated(#B), "b")
    eq(__LOC__, narrowOpenAnnotated(#C), "c")
  })

  test("record field nesting", () => {
    eq(__LOC__, narrowRecordField({inner: #A}), "a")
    eq(__LOC__, narrowRecordField({inner: #B}), "b")
    eq(__LOC__, narrowRecordField({inner: #C}), "c")
  })

  test("tuple with permissive sibling", () => {
    eq(__LOC__, narrowTupleSibling((0, #A)), "zero-a")
    eq(__LOC__, narrowTupleSibling((1, #A)), "nonzero-a")
    eq(__LOC__, narrowTupleSibling((0, #B)), "b")
    eq(__LOC__, narrowTupleSibling((1, #C)), "c")
  })

  test("multiple constructors at independent paths", () => {
    eq(__LOC__, narrowMultiConstructor(First(#A)), "first-a")
    eq(__LOC__, narrowMultiConstructor(First(#B)), "b")
    eq(__LOC__, narrowMultiConstructor(First(#C)), "c")
    eq(__LOC__, narrowMultiConstructor(Second(#X)), "x")
    eq(__LOC__, narrowMultiConstructor(Second(#Y)), "y")
    eq(__LOC__, narrowMultiConstructor(Second(#Z)), "second-z")
  })

  test("guard on the ...rest arm itself", () => {
    eq(__LOC__, narrowRestWithGuard(#A, true), 1)
    eq(__LOC__, narrowRestWithGuard(#B, true), 99)
    eq(__LOC__, narrowRestWithGuard(#B, false), 2)
    eq(__LOC__, narrowRestWithGuard(#C, false), 3)
  })

  test("variant within variant", () => {
    eq(__LOC__, narrowVariantInVariant(#Outer(#InnerA)), "ia")
    eq(__LOC__, narrowVariantInVariant(#Outer(#InnerB)), "ib")
    eq(__LOC__, narrowVariantInVariant(#Outer(#InnerC)), "ic")
  })

  test("prenex-quantified row (row_fixed) still narrows", () => {
    eq(__LOC__, narrowFixedRow(#A), "a")
    eq(__LOC__, narrowFixedRow(#B), "b")
    eq(__LOC__, narrowFixedRow(#C), "c")
  })
})

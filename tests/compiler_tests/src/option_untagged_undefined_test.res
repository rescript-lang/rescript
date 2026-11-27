// A switch on an option of a variant with an `@as(undefined)` case takes the
// arm of that case for `Some` of it, which is represented by an object, when
// the arm is a wildcard or the match's default (#9184)

open Mocha
open Test_utils

@inline(never) let some = x => Some(x)

@unboxed type t = | @as(undefined) U | S(string) | I(int)

let catchAll = (x: option<t>) =>
  switch x {
  | None => "None"
  | Some(S(_)) => "S"
  | Some(I(_)) => "I"
  | Some(_) => "U"
  }

let intFirst = (x: option<t>) =>
  switch x {
  | None => "None"
  | Some(I(_)) => "I"
  | Some(S(_)) => "S"
  | Some(_) => "U"
  }

let explicit = (x: option<t>) =>
  switch x {
  | None => "None"
  | Some(S(_)) => "S"
  | Some(I(_)) => "I"
  | Some(U) => "U"
  }

let orPattern = (x: option<t>) =>
  switch x {
  | None => "None"
  | Some(S(_) | I(_)) => "SI"
  | Some(_) => "U"
  }

let oneNamed = (x: option<t>) =>
  switch x {
  | None => "None"
  | Some(I(_)) => "I"
  | Some(_) => "U"
  }

let matchDefault = (x: option<t>) =>
  switch x {
  | Some(S(_)) => "S"
  | Some(I(_)) => "I"
  | _ => "U or None"
  }

// Literal cases only
@unboxed type literals = | @as(undefined) LU | @as(null) LN | @as("a") LA

let literalsCatchAll = (x: option<literals>) =>
  switch x {
  | None => "None"
  | Some(LN) => "N"
  | Some(LA) => "A"
  | Some(_) => "U"
  }

let literalsDefault = (x: option<literals>) =>
  switch x {
  | Some(LN) => "N"
  | Some(LA) => "A"
  | _ => "U or None"
  }

// Literals and payloads
@unboxed type mixed = | @as(undefined) MU | @as(null) MN | MS(string) | MI(int)

let mixedCatchAll = (x: option<mixed>) =>
  switch x {
  | None => "None"
  | Some(MN) => "N"
  | Some(MS(_)) => "S"
  | Some(MI(_)) => "I"
  | Some(_) => "U"
  }

let mixedPayloads = (x: option<mixed>) =>
  switch x {
  | None => "None"
  | Some(MS(_)) => "S"
  | Some(MI(_)) => "I"
  | Some(_) => "U or N"
  }

let mixedIncomplete = (x: option<mixed>) =>
  switch x {
  | None => "None"
  | Some(MS(_)) => "S"
  | Some(_) => "other"
  }

// An object payload and a single payload
@unboxed type withObject = | @as(undefined) OU | OS(string) | OO({x: int})

let objectCatchAll = (x: option<withObject>) =>
  switch x {
  | None => "None"
  | Some(OS(_)) => "S"
  | Some(OO(_)) => "O"
  | Some(_) => "U"
  }

@unboxed type single = | @as(undefined) SU | SS(string)

let singleCatchAll = (x: option<single>) =>
  switch x {
  | None => "None"
  | Some(SS(_)) => "S"
  | Some(_) => "U"
  }

// An optional record field
type record = {field?: t}

let field = (r: record) =>
  switch r {
  | {field: S(_)} => "S"
  | {field: I(_)} => "I"
  | {field: ?Some(_)} => "U"
  | {field: ?None} => "None"
  }

describe(__MODULE__, () => {
  test("a wildcard takes Some of the undefined case", () => {
    eq(__LOC__, "U", catchAll(Some(U)))
    eq(__LOC__, "U", catchAll(some(U)))
    eq(__LOC__, "S", catchAll(some(S("s"))))
    eq(__LOC__, "I", catchAll(some(I(1))))
    eq(__LOC__, "None", catchAll(None))
    eq(__LOC__, "U", intFirst(Some(U)))
    eq(__LOC__, "U", intFirst(some(U)))
    eq(__LOC__, "S", intFirst(some(S("s"))))
    eq(__LOC__, "I", intFirst(some(I(1))))
  })
  test("an arm naming the undefined case takes it", () => {
    eq(__LOC__, "U", explicit(Some(U)))
    eq(__LOC__, "U", explicit(some(U)))
    eq(__LOC__, "S", explicit(some(S("s"))))
    eq(__LOC__, "I", explicit(some(I(1))))
    eq(__LOC__, "None", explicit(None))
  })
  test("an or-pattern of every payload case and a single named case", () => {
    eq(__LOC__, "U", orPattern(Some(U)))
    eq(__LOC__, "U", orPattern(some(U)))
    eq(__LOC__, "SI", orPattern(some(S("s"))))
    eq(__LOC__, "SI", orPattern(some(I(1))))
    eq(__LOC__, "U", oneNamed(Some(U)))
    eq(__LOC__, "U", oneNamed(some(U)))
    eq(__LOC__, "U", oneNamed(some(S("s"))))
    eq(__LOC__, "I", oneNamed(some(I(1))))
  })
  test("the match's default takes Some of the undefined case", () => {
    eq(__LOC__, "U or None", matchDefault(Some(U)))
    eq(__LOC__, "U or None", matchDefault(some(U)))
    eq(__LOC__, "S", matchDefault(some(S("s"))))
    eq(__LOC__, "I", matchDefault(some(I(1))))
    eq(__LOC__, "U or None", matchDefault(None))
  })
  test("a variant with literal cases only", () => {
    eq(__LOC__, "U", literalsCatchAll(Some(LU)))
    eq(__LOC__, "U", literalsCatchAll(some(LU)))
    eq(__LOC__, "N", literalsCatchAll(some(LN)))
    eq(__LOC__, "A", literalsCatchAll(some(LA)))
    eq(__LOC__, "None", literalsCatchAll(None))
    eq(__LOC__, "U or None", literalsDefault(Some(LU)))
    eq(__LOC__, "U or None", literalsDefault(some(LU)))
    eq(__LOC__, "N", literalsDefault(some(LN)))
    eq(__LOC__, "A", literalsDefault(some(LA)))
  })
  test("a variant with literal and payload cases", () => {
    eq(__LOC__, "U", mixedCatchAll(Some(MU)))
    eq(__LOC__, "U", mixedCatchAll(some(MU)))
    eq(__LOC__, "N", mixedCatchAll(some(MN)))
    eq(__LOC__, "S", mixedCatchAll(some(MS("s"))))
    eq(__LOC__, "I", mixedCatchAll(some(MI(1))))
    eq(__LOC__, "U or N", mixedPayloads(Some(MU)))
    eq(__LOC__, "U or N", mixedPayloads(some(MU)))
    eq(__LOC__, "U or N", mixedPayloads(some(MN)))
    eq(__LOC__, "S", mixedPayloads(some(MS("s"))))
    eq(__LOC__, "I", mixedPayloads(some(MI(1))))
    eq(__LOC__, "other", mixedIncomplete(some(MU)))
    eq(__LOC__, "S", mixedIncomplete(some(MS("s"))))
    eq(__LOC__, "other", mixedIncomplete(some(MI(1))))
  })
  test("an object payload and a single payload", () => {
    eq(__LOC__, "U", objectCatchAll(Some(OU)))
    eq(__LOC__, "U", objectCatchAll(some(OU)))
    eq(__LOC__, "S", objectCatchAll(some(OS("s"))))
    eq(__LOC__, "O", objectCatchAll(some(OO({x: 1}))))
    eq(__LOC__, "U", singleCatchAll(Some(SU)))
    eq(__LOC__, "U", singleCatchAll(some(SU)))
    eq(__LOC__, "S", singleCatchAll(some(SS("s"))))
  })
  test("an optional record field holding the undefined case", () => {
    eq(__LOC__, "U", field({field: U}))
    eq(__LOC__, "U", field({field: ?some(U)}))
    eq(__LOC__, "S", field({field: S("s")}))
    eq(__LOC__, "I", field({field: I(1)}))
    eq(__LOC__, "None", field({}))
  })
})

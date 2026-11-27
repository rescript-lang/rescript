open Mocha
open Test_utils

// A guard may change the value being matched. The cases after it read the
// changed value, so they must test it again and may all fail to match it.

type pair = {a: bool, mutable b: (option<int>, option<int>)}

let nextB = ref(None)

let setB = r => {
  switch nextB.contents {
  | Some(b) => r.b = b
  | None => ()
  }
  false
}

// The tests on b.0 before the guard must not be trusted after it (#9129).
let pair = r =>
  switch r {
  | {a: true, b: (None, _)} => 1
  | {a: true, b: (Some(_), _)} if setB(r) => 0
  | {a: false} => 3
  | {b: (_, None)} => 4
  | {b: (Some(x), Some(_))} => x
  }

let pairWith = (b, next) => {
  nextB := next
  pair({a: true, b})
}

type flagged = {d: bool, mutable c: option<int>}

let clear = r => {
  r.c = None
  false
}

// The guarded case reads no mutable field, but the case after it does.
let flagged = r =>
  switch r {
  | {c: None} => 0
  | {d: true} if clear(r) => 1
  | {c: Some(x)} => x
  }

describe(__MODULE__, () => {
  test("guards that mutate nothing", () => {
    eq(__LOC__, 1, pairWith((None, None), None))
    eq(__LOC__, 4, pairWith((Some(1), None), None))
    eq(__LOC__, 7, pairWith((Some(7), Some(2)), None))
    eq(__LOC__, 3, pair({a: false, b: (None, None)}))
    eq(__LOC__, 5, flagged({d: false, c: Some(5)}))
  })

  test("a guard that mutates into a later case", () => {
    eq(__LOC__, 8, pairWith((Some(1), None), Some((Some(8), Some(2)))))
    eq(__LOC__, 4, pairWith((Some(1), Some(2)), Some((None, None))))
  })

  test("a guard that mutates into no case", () => {
    throws(__LOC__, () => ignore(pairWith((Some(1), None), Some((None, Some(2))))))
    throws(__LOC__, () => ignore(flagged({d: true, c: Some(5)})))
  })
})

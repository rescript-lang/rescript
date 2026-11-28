open Mocha
open Test_utils

/***
A self tail call, compiled to a loop that reassigns its parameters, evaluates
its arguments left to right, like any other call.
*/

let order = ref("")

@inline(never)
let note = (name, value: int) => {
  order := order.contents ++ name
  value
}

let rec selfCall = (a, b, n) =>
  if n == 0 {
    a + b
  } else {
    selfCall(note("a", a), note("b", b), n - 1)
  }

let rec threeArgs = (a, b, c, n) =>
  if n == 0 {
    a + b + c
  } else {
    threeArgs(note("a", a), note("b", b), note("c", c), n - 1)
  }

// [b] is passed through unchanged, so it is not reassigned.
let rec skipped = (a, b, c, n) =>
  if n == 0 {
    a + b + c
  } else {
    skipped(note("a", a), b, note("c", c), n - 1)
  }

// [b]'s argument mutates the state [a]'s argument reads.
let rec reads = (a, b, n) =>
  if n == 0 {
    a * 10 + b
  } else {
    let r = ref(a)
    reads(
      r.contents,
      {
        r := r.contents + 1
        r.contents
      },
      n - 1,
    )
  }

// [b]'s argument compiles to statements, so [a]'s is held in a temporary.
let rec statements = (a, b, n) =>
  if n == 0 {
    a + b
  } else {
    statements(
      note("a", a),
      try note("b", b) catch {
      | Not_found => 0
      },
      n - 1,
    )
  }

// A cast makes the argument count differ from the parameter count, so [b] is
// `undefined` in the recursive call.
let rec tooFewArgs = (a, b) =>
  if a == 0 {
    b
  } else {
    (Obj.magic(tooFewArgs): int => int)(note("a", a - 1))
  }

let rec tooManyArgs = a =>
  if a == 0 {
    a
  } else {
    (Obj.magic(tooManyArgs): (int, int) => int)(note("a", a - 1), note("b", a))
  }

let left = ref(0)
let steps = ref(0)

@inline(never)
let tick = () => {
  order := order.contents ++ "t"
  left := left.contents - 1
}

// A `()` function has no parameter to reassign. [steps] ends the loop when the
// argument is not evaluated.
let rec unitArg = () => {
  steps := steps.contents + 1
  if left.contents > 0 && steps.contents < 10 {
    unitArg(tick())
  } else {
    steps.contents
  }
}

let rec unitTooManyArgs = () => {
  steps := steps.contents + 1
  if steps.contents == 1 {
    (Obj.magic(unitTooManyArgs): (int, int) => int)(note("a", 1), note("b", 2))
  } else {
    steps.contents
  }
}

@inline(never) let other = (a, b, _n) => a + b

let otherCall = (a, b) => other(note("a", a), note("b", b), 0)

describe(__MODULE__, () => {
  test("a self tail call", () => {
    order := ""
    eq(__LOC__, selfCall(1, 2, 1), 3)
    eq(__LOC__, order.contents, "ab")
  })

  test("a self tail call with three arguments, twice", () => {
    order := ""
    eq(__LOC__, threeArgs(1, 2, 3, 2), 6)
    eq(__LOC__, order.contents, "abcabc")
  })

  test("a self tail call passing a parameter through", () => {
    order := ""
    eq(__LOC__, skipped(1, 2, 3, 1), 6)
    eq(__LOC__, order.contents, "ac")
  })

  test("a self tail call whose argument a later one mutates", () => {
    eq(__LOC__, reads(1, 0, 1), 12)
  })

  test("a self tail call with a statement argument", () => {
    order := ""
    eq(__LOC__, statements(1, 2, 1), 3)
    eq(__LOC__, order.contents, "ab")
  })

  test("a self tail call with too few arguments", () => {
    order := ""
    eq(__LOC__, tooFewArgs(0, 5), 5)
    eq(__LOC__, tooFewArgs(1, 5), Obj.magic(undefined))
    eq(__LOC__, order.contents, "a")
  })

  test("a self tail call with too many arguments", () => {
    order := ""
    eq(__LOC__, tooManyArgs(2), 0)
    eq(__LOC__, order.contents, "abab")
  })

  test("a self tail call with a unit argument", () => {
    order := ""
    steps := 0
    left := 3
    eq(__LOC__, unitArg(), 4)
    eq(__LOC__, order.contents, "ttt")
  })

  test("a self tail call with too many arguments for a unit function", () => {
    order := ""
    steps := 0
    eq(__LOC__, unitTooManyArgs(), 2)
    eq(__LOC__, order.contents, "ab")
  })

  test("another call", () => {
    order := ""
    eq(__LOC__, otherCall(1, 2), 3)
    eq(__LOC__, order.contents, "ab")
  })
})

open Mocha
open Test_utils

/***
JS doesn't allow a bare `in` operator in the initializer of a `for` loop
header, where the start and the end of a ReScript `for` loop are printed.
*/

@inline(never) let id = x => x

let inEnd = d => {
  let visited = []
  for i in 0 to d->Dict.has("a") ? 2 : 1 {
    visited->Array.push(i)
  }
  visited
}

let inStart = d => {
  let visited = []
  for i in d->Dict.has("a") ? 1 : 2 to 3 {
    visited->Array.push(i)
  }
  visited
}

let inStartAndEnd = d => {
  let visited = []
  for i in d->Dict.has("a") ? 3 : 2 downto d->Dict.has("b") ? 0 : 1 {
    visited->Array.push(i)
  }
  visited
}

let inNestedCondition = (d, flag) => {
  let visited = []
  for i in 0 to flag && d->Dict.has("a") ? 1 : d->Dict.has("b") ? 2 : 3 {
    visited->Array.push(i)
  }
  visited
}

// Inside brackets no extra parentheses are needed
let inBrackets = d => {
  let visited = []
  for i in 0 to ["a", "b", "c"]->Array.filter(key => d->Dict.has(key))->Array.length {
    visited->Array.push(i)
  }
  visited
}

// Nor are they in the loop's body
let inBody = d => {
  let visited = []
  for i in 0 to id(1) {
    visited->Array.push((i, d->Dict.has("a")))
  }
  visited
}

describe(__MODULE__, () => {
  let a = id(dict{"a": 1})
  let b = id(dict{"b": 1})

  test("in the end", () => {
    eq(__LOC__, inEnd(a), [0, 1, 2])
    eq(__LOC__, inEnd(b), [0, 1])
  })
  test("in the start", () => {
    eq(__LOC__, inStart(a), [1, 2, 3])
    eq(__LOC__, inStart(b), [2, 3])
  })
  test("in the start and the end", () => {
    eq(__LOC__, inStartAndEnd(a), [3, 2, 1])
    eq(__LOC__, inStartAndEnd(b), [2, 1, 0])
  })
  test("in nested conditions", () => {
    eq(__LOC__, inNestedCondition(a, true), [0, 1])
    eq(__LOC__, inNestedCondition(b, true), [0, 1, 2])
    eq(__LOC__, inNestedCondition(a, false), [0, 1, 2, 3])
  })
  test("inside brackets", () => {
    eq(__LOC__, inBrackets(a), [0, 1])
  })
  test("in the loop's body", () => {
    eq(__LOC__, inBody(a), [(0, true), (1, true)])
  })
})

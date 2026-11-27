open Mocha
open Test_utils

/***
Operands of the `in` operator and of loop headers that need parentheses in JS.
*/

@inline(never) let id = x => x

let log = []

@inline(never)
let logged = () => log->Array.push("logged")

external boolAsString: bool => string = "%identity"

let condProperty = (d, flag) => d->Dict.has(flag ? "x" : "y")
let condObject = (flag, d, e) => (flag ? d : e)->Dict.has("a")
let comparisonProperty = (d, a, b) => d->Dict.has(boolAsString(a === b))

let seqProperty = d =>
  d->Dict.has({
    logged()
    id("a")
  })

let seqObject = d =>
  {
    logged()
    id(d)
  }->Dict.has("a")

let inProperty = (d, e) => e->Dict.has(boolAsString(d->Dict.has("a")))
let dictObject = key => dict{"a": 1}->Dict.has(key)
// No parentheses are needed around an operand that binds more tightly
let concatProperty = (d, suffix) => d->Dict.has("a" ++ suffix)

let lessThan = (d, e) => d->Dict.has("a") < e->Dict.has("a")
let equal = (d, e) => d->Dict.has("a") === e->Dict.has("a")
let concatRight = (d, s) => s ++ boolAsString(d->Dict.has("a"))
let concatLeft = (d, s) => boolAsString(d->Dict.has("a")) ++ s

let seqStart = () => {
  let visited = []
  for i in {
    logged()
    id(2)
  } to 4 {
    visited->Array.push(i)
  }
  visited
}

let seqStartVariableEnd = last => {
  let visited = []
  for i in {
    logged()
    id(2)
  } downto last {
    visited->Array.push(i)
  }
  visited
}

// The number of calls to `logged` since the last count
let takeLogCount = () => {
  let count = log->Array.length
  log->Array.splice(~start=0, ~remove=count, ~insert=[])
  count
}

describe(__MODULE__, () => {
  let a = id(dict{"a": 1})
  let x = id(dict{"x": 1})
  let empty = id(dict{})

  test("conditional operands of in", () => {
    eq(__LOC__, condProperty(x, true), true)
    eq(__LOC__, condProperty(x, false), false)
    eq(__LOC__, condObject(true, a, x), true)
    eq(__LOC__, condObject(false, a, x), false)
  })
  test("sequence operands of in", () => {
    eq(__LOC__, seqProperty(a), true)
    eq(__LOC__, seqProperty(x), false)
    eq(__LOC__, seqObject(a), true)
    eq(__LOC__, seqObject(x), false)
    eq(__LOC__, takeLogCount(), 4)
  })
  test("operator operands of in", () => {
    eq(__LOC__, comparisonProperty(id(dict{"true": 1}), id(1), id(1)), true)
    eq(__LOC__, comparisonProperty(id(dict{"true": 1}), id(1), id(2)), false)
    eq(__LOC__, inProperty(a, id(dict{"true": 1})), true)
    eq(__LOC__, inProperty(x, id(dict{"true": 1})), false)
    eq(__LOC__, concatProperty(id(dict{"ab": 1}), "b"), true)
  })
  test("dict literal as the object of in", () => {
    eq(__LOC__, dictObject("a"), true)
    eq(__LOC__, dictObject("b"), false)
  })
  test("in as an operand", () => {
    eq(__LOC__, lessThan(empty, a), true)
    eq(__LOC__, lessThan(a, a), false)
    eq(__LOC__, equal(empty, empty), true)
    eq(__LOC__, equal(a, empty), false)
    eq(__LOC__, concatRight(a, "is "), "is true")
    eq(__LOC__, concatLeft(a, "!"), "true!")
  })
  test("sequence as the start of a for loop", () => {
    eq(__LOC__, seqStart(), [2, 3, 4])
    eq(__LOC__, seqStartVariableEnd(id(1)), [2, 1])
    eq(__LOC__, takeLogCount(), 2)
  })
})

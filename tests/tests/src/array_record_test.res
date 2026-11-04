open Mocha
open Test_utils

// Records whose fields have the runtime names "0", "1", ... in declaration
// order are JavaScript arrays

@scope("Array") @val external isArray: 'a => bool = "isArray"
@get external length: 'a => int = "length"

type withOptional = {@as("0") a?: int, @as("1") b: int}

let updateOptional = (r: withOptional) => {...r, b: 2}

type big = {
  @as("0") f0: int,
  @as("1") f1: int,
  @as("2") f2: int,
  @as("3") f3: int,
  @as("4") f4: int,
  @as("5") f5: int,
  @as("6") f6: int,
  @as("7") f7: int,
  @as("8") f8: int,
  @as("9") f9: int,
  @as("10") f10: int,
  @as("11") f11: int,
  @as("12") f12: int,
  @as("13") f13: int,
  @as("14") f14: int,
  @as("15") f15: int,
  @as("16") f16: int,
  @as("17") f17: int,
  @as("18") f18: int,
  @as("19") f19: int,
}

let bigValue: big = {
  f0: 0,
  f1: 1,
  f2: 2,
  f3: 3,
  f4: 4,
  f5: 5,
  f6: 6,
  f7: 7,
  f8: 8,
  f9: 9,
  f10: 10,
  f11: 11,
  f12: 12,
  f13: 13,
  f14: 14,
  f15: 15,
  f16: 16,
  f17: 17,
  f18: 18,
  f19: 19,
}

let updateBig = (r: big) => {...r, f1: 100}

type mutablePair = {@as("0") mutable x: int, @as("1") y: string}

let makeMutable = (x, y) => {x, y}
let updateMutable = (r: mutablePair) => {...r, y: "z"}

type empty = {@as("0") e?: int}

let emptyValue: empty = {}

// Extending an array record with the next index is an array record too
type pair = {@as("0") first: int, @as("1") second: int}
type triple = {...pair, @as("2") third: int}

let tripleValue: triple = {first: 1, second: 2, third: 3}

// Runtime names out of declaration order: an object
type swapped = {@as("1") p: int, @as("0") q: int}

let swappedValue = {p: 1, q: 2}

// Rest of an array record into an object record
type source = {@as("0") s0: int, @as("1") s1: int, @as("2") s2?: int}
type tail = {@as("1") s1: int, @as("2") s2?: int}

let split = (r: source) => {
  let {s0, ...tail as rest} = r
  (s0, rest)
}

describe(__MODULE__, () => {
  test("spread over a record with an optional field is an array", () => {
    let r = updateOptional({a: 1, b: 0})
    ok(__LOC__, isArray(r))
    eq(__LOC__, (r.a, r.b), (Some(1), 2))
    let r = updateOptional({b: 0})
    ok(__LOC__, isArray(r))
    eq(__LOC__, (r.a, r.b), (None, 2))
  })

  test("spread over a record with 20 fields is an array", () => {
    let r = updateBig(bigValue)
    ok(__LOC__, isArray(r))
    eq(__LOC__, length(r), 20)
    eq(__LOC__, (r.f0, r.f1, r.f19), (0, 100, 19))
  })

  test("mutable fields write the array element", () => {
    let r = makeMutable(1, "a")
    r.x = 5
    let r = updateMutable(r)
    ok(__LOC__, isArray(r))
    eq(__LOC__, (r.x, r.y), (5, "z"))
  })

  test("an empty literal is an array", () => {
    ok(__LOC__, isArray(emptyValue))
    eq(__LOC__, emptyValue.e, None)
  })

  test("a type spread keeps the array layout", () => {
    ok(__LOC__, isArray(tripleValue))
    eq(__LOC__, tripleValue.third, 3)
  })

  test("runtime names out of order give an object", () => {
    ok(__LOC__, !isArray(swappedValue))
    eq(__LOC__, (swappedValue.p, swappedValue.q), (1, 2))
  })

  test("pattern matching reads the elements", () => {
    let classify = (r: source) =>
      switch r {
      | {s0: 0, s1} => s1
      | {s2} => s2
      | _ => -1
      }
    eq(__LOC__, classify({s0: 0, s1: 7}), 7)
    eq(__LOC__, classify({s0: 1, s1: 7, s2: 9}), 9)
    eq(__LOC__, classify({s0: 1, s1: 7}), -1)
  })

  test("rest of an array record", () => {
    let (s0, rest) = split({s0: 1, s1: 2, s2: 3})
    eq(__LOC__, s0, 1)
    eq(__LOC__, (rest.s1, rest.s2), (2, Some(3)))
  })
})

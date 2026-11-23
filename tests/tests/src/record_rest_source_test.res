open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

/***
After `let {x: _, ...rest} = r`, later reads of `r.x` must see the current
value of `r.x`, whatever wrote it after the destructuring. A name the pattern
binds keeps the value at the destructuring.
*/

type mutableXy = {mutable x: int, y: int}
type xy = {x: int, y: int}
type onlyY = {y: int}

let writeAfterRest = (a: mutableXy) => {
  let b = (a :> xy)
  let {x: _, ...onlyY as rest} = b
  a.x = 42
  (rest, a)
}

let readAfterWrite = (a: mutableXy) => {
  let b = (a :> xy)
  let {x: _, ...onlyY as rest} = b
  a.x = 42
  (rest, b.x)
}

type counter = {name: string, bump: unit => unit, other: int}
type onlyOther = {other: int}

let makeCounter: unit => counter = %raw(`() => ({name: "old", bump() { this.name = "new" }, other: 1})`)

let methodCallThenRead = (c: counter) => {
  let {name: _, bump: _, ...onlyOther as rest} = c
  c.bump()
  (rest, c.name)
}

@inline(never) let bump = (a: mutableXy) => a.x = 42

let calleeThenRead = (a: mutableXy) => {
  let b = (a :> xy)
  let {x: _, ...onlyY as rest} = b
  bump(a)
  (rest, b.x)
}

let aliasThenRead = (a: mutableXy, b: xy) => {
  let {x: _, ...onlyY as rest} = b
  a.x = 42
  (rest, b.x)
}

let awaitThenRead = async (a: mutableXy, p: promise<unit>) => {
  let b = (a :> xy)
  let {x: _, ...onlyY as rest} = b
  await p
  (rest, b.x)
}

let closureThenRead = (a: mutableXy, flag) => {
  let b = (a :> xy)
  let (get, rest) = if flag {
    let {x: _, ...onlyY as rest} = b
    (() => b.x, rest)
  } else {
    (() => 0, {y: 0})
  }
  a.x = 42
  (rest, get())
}

let viewSource: mutableXy = {x: 1, y: 2}
let view = (viewSource :> xy)
let {x: _, ...onlyY as viewRest} = view
let getViewX = () => view.x

@set_index external setIndex: (xy, string, int) => unit = ""

let computedWriteThenRead = (b: xy) => {
  let {x: _, ...onlyY as rest} = b
  setIndex(b, "x", 42)
  (rest, b.x)
}

let boundThenAwait = async (c: counter, p: promise<unit>) => {
  let {name, bump: _, ...onlyOther as rest} = c
  await p
  (rest, name)
}

let boundThenCall = (c: counter, f: unit => unit) => {
  let {name, bump: _, ...onlyOther as rest} = c
  f()
  (rest, name, c.name)
}

let boundThenWrite = (a: mutableXy) => {
  let {x, ...onlyY as rest} = (a :> xy)
  a.x = 42
  (rest, x, a.x)
}

let boundWithoutRest = (c: counter, f: unit => unit) => {
  let {name, bump: _, ...onlyOther as _rest} = c
  f()
  name
}

let boundInCase = (c: option<counter>, f: unit => unit) =>
  switch c {
  | Some({name, bump: _, ...onlyOther as rest}) =>
    f()
    Some(rest, name)
  | None => None
  }

let boundAfterTest = (c: counter, f: unit => unit) =>
  switch c {
  | {name: "none", bump: _, ...onlyOther as rest} => (rest, "nothing")
  | {name, bump: _, ...onlyOther as rest} =>
    f()
    (rest, name)
  }

let callBound = (c: counter) => {
  let {name: _, bump, ...onlyOther as rest} = c
  bump()
  rest
}

let callSourceAndBound = ({name, bump, ...onlyOther as rest} as c: counter) => {
  c.bump()
  bump()
  (rest, name, c.name)
}

type inner = {i: int, j: int}
type outer = {inner: inner, z: int}
type onlyZ = {z: int}
type onlyJ = {j: int}

let boundFieldOfField = (o: outer, f: unit => unit) => {
  let {inner: {i}, ...onlyZ as rest} = o
  f()
  (rest, i)
}

let nestedRests = (o: outer, f: unit => unit) => {
  let {inner: {i, ...onlyJ as innerRest}, ...onlyZ as rest} = o
  f()
  (rest, innerRest, i)
}

let restsInTuple = (
  ({x: a, ...onlyY as rest1}, {x: b, ...onlyY as rest2}): (xy, xy),
  f: unit => unit,
) => {
  f()
  (a, rest1, b, rest2)
}

let explicitAndBound = (c: counter) => {
  let n = c.name
  let {name, bump: _, ...onlyOther as rest} = c
  c.bump()
  (n, name, rest, n, name)
}

/* A field next to a rest that binds no variable is only read where it is tested:
 it gets no local, and a wildcard no read besides the destructuring's. */

let constantField = (a: xy) =>
  switch a {
  | {x: 1, ...onlyY as rest} => Some(rest)
  | _ => None
  }

let wildcardField = ({x: _, ...onlyY as rest}: xy) => rest

type optionalName = {maybeName: option<string>, other: int}

let constructorField = (o: optionalName) =>
  switch o {
  | {maybeName: Some(_), ...onlyOther as rest} => ("named", rest)
  | {maybeName: None, ...onlyOther as rest} => ("unnamed", rest)
  }

@module("node:assert")
external deepStrictEqual: ('a, 'a) => unit = "deepStrictEqual"

// the number of reads of a getter named `name`
let countingCounter: unit => (counter, unit => int) = %raw(`() => {
  let reads = 0;
  return [
    { get name() { reads = reads + 1; return "read" + reads; }, bump() {}, other: 1 },
    () => reads,
  ];
}`)

let nestedGetter: unit => (outer, unit => int) = %raw(`() => {
  let reads = 0;
  return [
    { get inner() { reads = reads + 1; return { i: reads, j: 2 }; }, z: 3 },
    () => reads,
  ];
}`)

// a `bump` that returns its `this` through `seen`
let thisCounter: unit => (counter, unit => bool) = %raw(`() => {
  let seen = "not called";
  return [
    { name: "old", bump() { seen = this; }, other: 1 },
    () => seen === undefined,
  ];
}`)

// the number of reads of a getter named `x`
let countingXy: int => (xy, unit => int) = %raw(`(x) => {
  let reads = 0;
  return [
    { get x() { reads = reads + 1; return x; }, y: 2 },
    () => reads,
  ];
}`)

let setInner: (outer, inner) => unit = %raw(`(o, inner) => { o.inner = inner; }`)
let setName: (counter, string) => unit = %raw(`(c, name) => { c.name = name; }`)

describe(__MODULE__, () => {
  test("a write after the rest", () => {
    let (rest, a) = writeAfterRest({x: 1, y: 2})
    deepStrictEqual((rest, a.x), ({y: 2}, 42))
  })

  test("a read through the source after a write", () => {
    let (rest, x) = readAfterWrite({x: 1, y: 2})
    deepStrictEqual((rest, x), ({y: 2}, 42))
  })

  test("a method call that writes the source", () => {
    let (rest, name) = methodCallThenRead(makeCounter())
    deepStrictEqual((rest, name), ({other: 1}, "new"))
  })

  test("a callee that writes the source", () => {
    let (rest, x) = calleeThenRead({x: 1, y: 2})
    deepStrictEqual((rest, x), ({y: 2}, 42))
  })

  test("a write through an alias of the source", () => {
    let a: mutableXy = {x: 1, y: 2}
    let (rest, x) = aliasThenRead(a, a :> xy)
    deepStrictEqual((rest, x), ({y: 2}, 42))
  })

  testAsync("a write during an await", async () => {
    let a: mutableXy = {x: 1, y: 2}
    let p = Promise.resolve()->Promise.thenResolve(() => a.x = 42)
    let (rest, x) = await awaitThenRead(a, p)
    deepStrictEqual((rest, x), ({y: 2}, 42))
  })

  test("a closure that outlives the rest", () => {
    let (rest, x) = closureThenRead({x: 1, y: 2}, true)
    deepStrictEqual((rest, x), ({y: 2}, 42))
  })

  test("exported top-level state written later", () => {
    viewSource.x = 42
    deepStrictEqual((viewRest, getViewX()), ({y: 2}, 42))
  })

  test("a computed-member write", () => {
    let (rest, x) = computedWriteThenRead({x: 1, y: 2})
    deepStrictEqual((rest, x), ({y: 2}, 42))
  })

  testAsync("a bound field after an await", async () => {
    let c = makeCounter()
    let p = Promise.resolve()->Promise.thenResolve(() => setName(c, "new"))
    let (rest, name) = await boundThenAwait(c, p)
    deepStrictEqual((rest, name), ({other: 1}, "old"))
  })

  test("a bound field after a call", () => {
    let c = makeCounter()
    let result = boundThenCall(c, () => setName(c, "new"))
    deepStrictEqual(result, ({other: 1}, "old", "new"))
  })

  test("a bound field after a write", () => {
    deepStrictEqual(boundThenWrite({x: 1, y: 2}), ({y: 2}, 1, 42))
  })

  test("a bound field next to an unused rest", () => {
    let c = makeCounter()
    eq(__LOC__, boundWithoutRest(c, () => setName(c, "new")), "old")
  })

  test("a bound field in a switch case", () => {
    let c = makeCounter()
    let result = boundInCase(Some(c), () => setName(c, "new"))
    deepStrictEqual(result, Some({other: 1}, "old"))
  })

  test("a bound field that an earlier case tests", () => {
    let c = makeCounter()
    let result = boundAfterTest(c, () => setName(c, "new"))
    deepStrictEqual(result, ({other: 1}, "old"))
  })

  test("a call of a bound function has no this", () => {
    let (c, calledWithoutThis) = thisCounter()
    let rest = callBound(c)
    deepStrictEqual((rest, calledWithoutThis()), ({other: 1}, true))
  })

  test("a method call on the source next to a call of a bound function", () => {
    let (c, calledWithoutThis) = thisCounter()
    let result = callSourceAndBound(c)
    deepStrictEqual((result, calledWithoutThis()), (({other: 1}, "old", "old"), true))
  })

  test("a field of a field", () => {
    let o = {inner: {i: 1, j: 2}, z: 3}
    let result = boundFieldOfField(o, () => setInner(o, {i: 42, j: 42}))
    deepStrictEqual(result, ({z: 3}, 1))
  })

  test("nested rests", () => {
    let o = {inner: {i: 1, j: 2}, z: 3}
    let result = nestedRests(o, () => setInner(o, {i: 42, j: 42}))
    deepStrictEqual(result, ({z: 3}, {j: 2}, 1))
  })

  test("rests in a tuple", () => {
    let a: mutableXy = {x: 1, y: 2}
    let result = restsInTuple((a :> xy, {x: 3, y: 4}), () => a.x = 42)
    deepStrictEqual(result, (1, {y: 2}, 3, {y: 4}))
  })

  test("an explicit read next to a bound field", () => {
    deepStrictEqual(explicitAndBound(makeCounter()), ("old", "old", {other: 1}, "old", "old"))
  })

  test("a bound field is read once", () => {
    let (c, reads) = countingCounter()
    let (rest, name) = boundThenCall(c, () => ())->(((rest, name, _)) => (rest, name))
    // the second read is the explicit one of the source
    deepStrictEqual((rest, name, reads()), ({other: 1}, "read1", 2))
    let (o, reads) = nestedGetter()
    let result = nestedRests(o, () => ())
    deepStrictEqual((result, reads()), (({z: 3}, {j: 2}, 1), 1))
  })

  test("a constant next to a rest", () => {
    deepStrictEqual(constantField({x: 1, y: 2}), Some({y: 2}))
    deepStrictEqual(constantField({x: 3, y: 2}), None)
    // a field that fails its test is read for the test only
    let (a, reads) = countingXy(3)
    deepStrictEqual((constantField(a), reads()), (None, 1))
  })

  test("a wildcard next to a rest", () => {
    // the one read leaves the field out of the rest
    let (a, reads) = countingXy(1)
    deepStrictEqual((wildcardField(a), reads()), ({y: 2}, 1))
  })

  test("a constructor without variables next to a rest", () => {
    deepStrictEqual(constructorField({maybeName: Some("a"), other: 1}), ("named", {other: 1}))
    deepStrictEqual(constructorField({maybeName: None, other: 1}), ("unnamed", {other: 1}))
  })
})

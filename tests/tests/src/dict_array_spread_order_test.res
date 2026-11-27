open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

@inline(never)
let id = x => x

// A spread is copied where it stands, before later elements run
let dictTry = (base: dict<int>) =>
  dict{
    ...base,
    "b": try {
      base->Dict.set("a", 100)
      id(5)
    } catch {
    | _ => 0
    },
  }

let dictLaterSpread = (base: dict<int>, other) =>
  dict{
    "first": id(0),
    ...base,
    ...try {
      base->Dict.set("a", 100)
      other()
    } catch {
    | _ => dict{}
    },
    ...base,
  }

let arrayTry = (base: array<int>) =>
  [
    ...base,
    try {
      base->Array.push(100)
      id(5)
    } catch {
    | _ => 0
    },
  ]

let arrayCall = (base: array<int>, f) => [...base, f()]

let arrayLaterSpread = (base: array<int>, f) => [0, ...base, ...f(), ...base]

let arrayCallThenTry = (base: array<int>, f) =>
  [
    ...base,
    f(),
    try f() catch {
    | _ => 0
    },
  ]

// The copy stays a value of its own when it is held before later statements
let arrayNested = (base: array<int>, f) =>
  id((
    [...base, f()],
    try f() catch {
    | _ => 0
    },
  ))

let arrayAwait = async (base: array<int>, f) => [...base, await f()]

// A getter runs code
let arrayObjectRead = (base: array<int>, obj: {"x": int}) => [...base, obj["x"]]

// The code that a later element runs can be in any part of its expression
let arrayIndexedCall = (base: array<int>, f) => [...base, f()->Array.getUnsafe(0)]

let arrayIndexCall = (base: array<int>, items, f) => [...base, items->Array.getUnsafe(f())]

let arraySequence = base => [...base, base->Array.setUnsafe(2, ())]

let arrayAppendLeft = (base: array<string>, f) => [...base, f() ++ "!"]

let arrayAppendRight = (base: array<string>, f) => [...base, "!" ++ f()]

let arrayOperandLeft = (base: array<int>, obj: {"x": int}) => [...base, obj["x"] + 1]

let arrayOperandRight = (base: array<int>, obj: {"x": int}) => [...base, 1 + obj["x"]]

let arrayOperandCall = (base: array<int>, f) => [...base, f() * 2]

let arrayCondTest = (base: array<int>, f) => [...base, f() > 0 ? 1 : 2]

let arrayCondThen = (base: array<int>, flag, f) => [...base, flag ? f() : 2]

let arrayCondElse = (base: array<int>, flag, f) => [...base, flag ? 1 : f()]

let arrayTemplate = (base: array<string>, x, f) => [...base, `${x}-${f()}`]

let arrayDictElement = (base, f) => [...base, dict{"x": 1, "y": f()}]

let arrayDictSpreadElement = (base: array<dict<int>>, other) => [...base, dict{"x": 1, ...other}]

// No copies: the later elements run no code, or only run where they stand
type r = {mutable a: int, items: array<int>}

let arrayRecordSpreadCall = (base, f) => [...base, {...f(), a: 1}]

let arrayRecordSpreadField = (base, r, f) => [...base, {...r, a: f()}]

let dictCall = (base: dict<int>, f) => dict{...base, "b": f()}

let dictPure = (base: dict<int>, r) =>
  dict{
    ...base,
    "a": {
      let a = r.a
      a * a
    },
  }

let arrayPure = (base: array<int>, r) =>
  [...base, r.a + 1, ...r.items, base->Array.length, r.a > 0 ? 1 : 2]

let arrayPureStatement = (base: array<int>, r) =>
  [
    ...base,
    {
      let a = r.a
      a * a
    },
  ]

let arrayPureConstants = (base, r) => [...base, Nullable.undefined, Nullable.null, r]

let arrayPureFunction = (base: array<unit => unit>) => [...base, () => base->Array.push(() => ())]

let arrayPureIndex = (base: array<int>, items) => [...base, items->Array.getUnsafe(0)]

let arrayPureAppend = (base: array<string>, x) => [...base, x ++ "!"]

let arrayPureTemplate = (base: array<string>, x) => [...base, `${x}-${x}`]

let arrayPureDict = (base: array<dict<int>>, x) => [...base, dict{"x": x}]

let arrayPureRecordSpread = (base, r, x) => [...base, {...r, a: x}]

// A record with an optional field is built from a spread of its source
type opt = {mutable a: int, b?: int}

let arrayOptionalRecordSpreadCall = (base, f): array<opt> => [...base, {...f(), a: 1}]

let arrayPureOptionalRecordSpread = (base, r: opt, x): array<opt> => [...base, {...r, a: x}]

let arrayNoElement = (base: array<int>, f) => [f(), ...base]

let arrayLiteral = f => [...[f(), 2], f()]

describe(__MODULE__, () => {
  let base = [1, 2]
  let pushing = base =>
    () => {
      base->Array.push(100)
      id(5)
    }

  test("a dict spread is copied before later statements", () => {
    eq(__LOC__, dictTry(dict{"a": 1}), dict{"a": 1, "b": 5})
    let dict = dictLaterSpread(dict{"a": 1}, () => dict{"c": 3})
    eq(__LOC__, dict, dict{"first": 0, "a": 100, "c": 3})
    let base = dict{"a": 1}
    let dict = dictLaterSpread(
      base,
      () => {
        base->Dict.delete("a")
        dict{"c": 3}
      },
    )
    eq(__LOC__, dict, dict{"first": 0, "a": 1, "c": 3})
  })

  test("an array spread is copied before later statements", () => {
    eq(__LOC__, arrayTry([1, 2]), [1, 2, 5])
  })

  test("an array spread is copied before later calls", () => {
    let base = [1, 2]
    eq(__LOC__, arrayCall(base, pushing(base)), [1, 2, 5])
    eq(__LOC__, base, [1, 2, 100])

    let base = [1, 2]
    let result = arrayLaterSpread(
      base,
      () => {
        base->Array.push(100)
        base
      },
    )
    eq(__LOC__, result, [0, 1, 2, 1, 2, 100, 1, 2, 100])

    let base = [1, 2]
    eq(__LOC__, arrayCallThenTry(base, pushing(base)), [1, 2, 5, 5])

    let base = [1, 2]
    eq(__LOC__, arrayNested(base, pushing(base)), ([1, 2, 5], 5))
  })

  test("an array spread is copied before a later getter", () => {
    let base = [1, 2]
    let obj: {"x": int} = %raw(`(base => ({get x() { base.push(100); return 5; }}))`)(base)
    eq(__LOC__, arrayObjectRead(base, obj), [1, 2, 5])
  })

  testAsync("an array spread is copied before a later await", async () => {
    let base = [1, 2]
    eq(__LOC__, await arrayAwait(base, async () => pushing(base)()), [1, 2, 5])
  })

  test("an array spread is copied before code in any part of a later element", () => {
    let base = [1, 2]
    eq(__LOC__, arrayIndexedCall(base, () => [pushing(base)()]), [1, 2, 5])
    let base = [1, 2]
    eq(__LOC__, arrayIndexCall(base, [7], () => pushing(base)() - 5), [1, 2, 7])
    let base = [1, 2]
    eq(__LOC__, arrayOperandCall(base, pushing(base)), [1, 2, 10])
    let base = [1, 2]
    eq(__LOC__, arrayCondTest(base, pushing(base)), [1, 2, 1])
    let base = [1, 2]
    eq(__LOC__, arrayCondThen(base, true, pushing(base)), [1, 2, 5])
    let base = [1, 2]
    eq(__LOC__, arrayCondElse(base, false, pushing(base)), [1, 2, 5])
    let base = [1, 2]
    let obj: {"x": int} = %raw(`(base => ({get x() { base.push(100); return 5; }}))`)(base)
    eq(__LOC__, arrayOperandLeft(base, obj), [1, 2, 6])
    let base = [1, 2]
    let obj: {"x": int} = %raw(`(base => ({get x() { base.push(100); return 5; }}))`)(base)
    eq(__LOC__, arrayOperandRight(base, obj), [1, 2, 6])
  })

  test("an array spread of strings is copied before a later call", () => {
    let pushingString = (base: array<string>) =>
      () => {
        base->Array.push("z")
        "y"
      }
    let base = ["a"]
    eq(__LOC__, arrayAppendLeft(base, pushingString(base)), ["a", "y!"])
    let base = ["a"]
    eq(__LOC__, arrayAppendRight(base, pushingString(base)), ["a", "!y"])
    let base = ["a"]
    eq(__LOC__, arrayTemplate(base, "x", pushingString(base)), ["a", "x-y"])
  })

  test("an array spread is copied before a later object or record that runs code", () => {
    let base = [dict{"a": 1}]
    let f = () => {
      base->Array.push(dict{"z": 0})
      2
    }
    eq(__LOC__, arrayDictElement(base, f), [dict{"a": 1}, dict{"x": 1, "y": 2}])
    let base = [dict{"a": 1}]
    let other = %raw(`(base => ({get y() { base.push({}); return 2; }}))`)(base)
    eq(__LOC__, arrayDictSpreadElement(base, other), [dict{"a": 1}, dict{"x": 1, "y": 2}])
    let base = [{a: 0, items: []}]
    let f = () => {
      base->Array.push({a: 9, items: []})
      {a: 5, items: [3]}
    }
    eq(__LOC__, arrayRecordSpreadCall(base, f), [{a: 0, items: []}, {a: 1, items: [3]}])
    let base = [{a: 0, items: []}]
    let f = () => {
      base->Array.push({a: 9, items: []})
      4
    }
    let r = {a: 5, items: [3]}
    eq(__LOC__, arrayRecordSpreadField(base, r, f), [{a: 0, items: []}, {a: 4, items: [3]}])
    let base: array<opt> = [{a: 0}]
    let f = (): opt => {
      base->Array.push({a: 9})
      {a: 5, b: 3}
    }
    eq(__LOC__, arrayOptionalRecordSpreadCall(base, f), [{a: 0}, {a: 1, b: 3}])
  })

  test("a later element that changes the spread where it stands", () => {
    let base = [(), ()]
    eq(__LOC__, arraySequence(base)->Array.length, 3)
  })

  test("elements that run no code", () => {
    let r = {a: 3, items: [7]}
    eq(__LOC__, dictCall(dict{"a": 1}, () => 5), dict{"a": 1, "b": 5})
    eq(__LOC__, dictPure(dict{"b": 1}, r), dict{"b": 1, "a": 9})
    eq(__LOC__, arrayPure(base, r), [1, 2, 4, 7, 2, 1])
    eq(__LOC__, arrayPureStatement(base, r), [1, 2, 9])
    eq(__LOC__, arrayNoElement(base, () => 0), [0, 1, 2])
    eq(__LOC__, arrayLiteral(() => 1), [1, 2, 1])
    eq(__LOC__, arrayPureIndex(base, [7]), [1, 2, 7])
    eq(__LOC__, arrayPureAppend(["a"], "x"), ["a", "x!"])
    eq(__LOC__, arrayPureTemplate(["a"], "x"), ["a", "x-x"])
    eq(__LOC__, arrayPureDict([dict{"a": 1}], 2), [dict{"a": 1}, dict{"x": 2}])
    eq(__LOC__, arrayPureRecordSpread([r], r, 1), [r, {a: 1, items: [7]}])
    eq(__LOC__, arrayPureOptionalRecordSpread([], {a: 0, b: 2}, 1), [{a: 1, b: 2}])
    eq(__LOC__, arrayPureFunction([])->Array.length, 1)
    eq(__LOC__, Array.length(arrayPureConstants([], Nullable.make(1))), 3)
  })
})

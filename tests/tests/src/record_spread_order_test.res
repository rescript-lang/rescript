open Mocha
open Test_utils

/***
`{...base, b: e}` copies `base` before `e` is evaluated, also when `e` needs
statements and the record has an optional field.
*/

@inline(never) let id = x => x

type r = {mutable a: int, b: int, c?: int}

let mutateInOverride = (base: r) => {
  ...base,
  b: try {
    base.a = 100
    id(5)
  } catch {
  | _ => 0
  },
}

let order = []

@inline(never)
let trace = (name, value) => {
  order->Array.push(name)
  value
}

let traced = (base: r) => {
  ...trace("base", base),
  b: try {
    trace("b", id(5))
  } catch {
  | _ => 0
  },
}

let tracedTwoOverrides = (base: r) => {
  ...trace("base2", base),
  a: trace("a2", 7),
  b: switch id(base.a) {
  | 1 => trace("b2", 5)
  | n => n
  },
}

let boundThenUsed = (base: r) => {
  let x = {...base, a: trace("a", 1), b: trace("b", 2)}
  ignore(id(x.a))
  x
}

let boundThenUsedTry = (base: r) => {
  let x = {
    ...base,
    a: try trace("a", id(1)) catch {
    | _ => 0
    },
    b: try trace("b", id(2)) catch {
    | _ => 0
    },
  }
  ignore(id(x.a))
  x
}

type m = {mutable x: int, mutable y: int, z?: int}

let assignAfter = (base: m) => {
  let copy = {...base, z: 1}
  copy.y = try trace("first", id(1)) catch {
  | _ => 0
  }
  copy.x = try trace("second", id(2)) catch {
  | _ => 0
  }
  copy
}

let assignAfterExpression = (base: m) => {
  let copy = {...base, z: 1}
  copy.y = trace("first", 1)
  copy.x = trace("second", 2)
  copy
}

let readCopy = (base: m) => {
  let copy = {...base, x: 1}
  copy.y = copy.x + 1
  copy
}

let readCopyInSwitch = (base: m) => {
  let copy = {...base, x: 10}
  copy.y =
    switch id(copy.x) {
    | 10 => trace("ten", 5)
    | n => n
    }
  copy
}

let assignTwice = (base: m) => {
  let copy = {...base, x: 1}
  copy.x = 2
  copy
}

let assignTwiceTry = (base: m) => {
  let copy = {
    ...base,
    x: try id(3) catch {
    | _ => 0
    },
  }
  copy.x = 4
  copy
}

/* The statements only define pure values: still one literal */
let pureStatements = (base: r, x: r) => {
  ...base,
  b: {
    let n = x.b
    n + n
  },
}

/* The base is evaluated before the read of [x.a] */
let pureStatementsAfterCall = (base: r, x: r) => {
  ...{
    x.a = 100
    id(base)
  },
  b: {
    let n = x.a
    n + n
  },
}

let callThenPureStatements = (base: r, x: r) => {
  ...base,
  a: {
    x.a = 100
    id(1)
  },
  b: {
    let n = x.a
    n + n
  },
}

/* No copy for a result that isn't used */
let discarded = (base: r) => {
  ignore({
    ...base,
    a: trace("a", 1),
    b: try trace("b", id(5)) catch {
    | _ => 0
    },
  })
  1
}

let throwing = (base: r) => {...trace("base", base), a: trace("a", 1), b: throw(Not_found)}

type v = Inline({mutable a: int, b: int, c?: int}) | Empty

let inlineRecord = v =>
  switch v {
  | Inline(x) =>
    Inline({
      ...x,
      b: try {
        x.a = 100
        id(5)
      } catch {
      | _ => 0
      },
    })
  | Empty => Empty
  }

exception Extension({mutable a: int, b: int, c?: int})

let exceptionRecord = e =>
  switch e {
  | Extension(x) =>
    Extension({
      ...x,
      b: try {
        x.a = 100
        id(5)
      } catch {
      | _ => 0
      },
    })
  | e => e
  }

let clearOrder = () => order->Array.splice(~start=0, ~remove=Array.length(order), ~insert=[])

let simpleOverride = (base: r) => {...base, b: id(9)}

@val external hasOwn: ('a, string) => bool = "Object.hasOwn"
@val external getPrototypeOf: 'a => 'b = "Object.getPrototypeOf"
@val external objectPrototype: 'a = "Object.prototype"

type proto<'a> = {name: string, @as("__proto__") proto?: 'a}

let protoOverride = (base: proto<'a>, value: 'a) => {
  ...base,
  proto: try id(value) catch {
  | _ => value
  },
}

describe(__MODULE__, () => {
  test("an override that mutates the base", () => {
    eq(__LOC__, mutateInOverride({a: 1, b: 2}).a, 1)
  })

  test("evaluation order", () => {
    clearOrder()
    ignore(traced({a: 1, b: 2}))
    eq(__LOC__, order, ["base", "b"])
  })

  test("evaluation order with several overrides", () => {
    clearOrder()
    let res = tracedTwoOverrides({a: 1, b: 2})
    eq(__LOC__, order, ["base2", "a2", "b2"])
    eq(__LOC__, (res.a, res.b), (7, 5))
  })

  test("an expression override", () => {
    let base = {a: 1, b: 2}
    let res = simpleOverride(base)
    eq(__LOC__, (res.a, res.b, base.b), (1, 9, 2))
  })

  test("a copy that is bound and then used", () => {
    clearOrder()
    let res = boundThenUsed({a: 0, b: 0})
    eq(__LOC__, order, ["a", "b"])
    eq(__LOC__, (res.a, res.b), (1, 2))
    clearOrder()
    let res = boundThenUsedTry({a: 0, b: 0})
    eq(__LOC__, order, ["a", "b"])
    eq(__LOC__, (res.a, res.b), (1, 2))
  })

  test("fields assigned after the spread", () => {
    clearOrder()
    eq(__LOC__, assignAfter({x: 0, y: 0}), {x: 2, y: 1, z: 1})
    eq(__LOC__, order, ["first", "second"])
    clearOrder()
    eq(__LOC__, assignAfterExpression({x: 0, y: 0}), {x: 2, y: 1, z: 1})
    eq(__LOC__, order, ["first", "second"])
  })

  test("a field assigned a value that reads the copy", () => {
    eq(__LOC__, readCopy({x: 0, y: 0}), {x: 1, y: 2})
    eq(__LOC__, readCopyInSwitch({x: 1, y: 2}), {x: 10, y: 5})
  })

  test("the last assignment of a field wins", () => {
    eq(__LOC__, assignTwice({x: 0, y: 0}).x, 2)
    eq(__LOC__, assignTwiceTry({x: 0, y: 0}).x, 4)
  })

  test("overrides whose statements are pure", () => {
    let base = {a: 1, b: 2}
    eq(__LOC__, pureStatements(base, {a: 3, b: 4}), {a: 1, b: 8})
    eq(__LOC__, pureStatementsAfterCall(base, {a: 3, b: 4}), {a: 1, b: 200})
    eq(__LOC__, callThenPureStatements(base, {a: 3, b: 4}), {a: 1, b: 200})
  })

  test("a discarded result", () => {
    clearOrder()
    eq(__LOC__, discarded({a: 1, b: 2}), 1)
    eq(__LOC__, order, ["a", "b"])
  })

  test("an override that throws", () => {
    clearOrder()
    throws(__LOC__, () => throwing({a: 1, b: 2})->ignore)
    eq(__LOC__, order, ["base", "a"])
  })

  test("an inline record", () =>
    switch inlineRecord(Inline({a: 1, b: 2})) {
    | Inline({a, b}) => eq(__LOC__, (a, b), (1, 5))
    | Empty => ok(__LOC__, false)
    }
  )

  test("an override of a field named __proto__ is an own property", () => {
    let withInt = protoOverride({name: "int"}, 5)
    ok(__LOC__, hasOwn(withInt, "__proto__"))
    eq(__LOC__, withInt.proto, Some(5))
    let withObject = protoOverride({name: "object"}, {"z": 1})
    ok(__LOC__, hasOwn(withObject, "__proto__"))
    eq(__LOC__, withObject.proto, Some({"z": 1}))
    ok(__LOC__, getPrototypeOf(withObject) === objectPrototype)
    let overridden = protoOverride({name: "own", proto: 1}, 2)
    eq(__LOC__, overridden.proto, Some(2))
  })

  test("an exception's record", () =>
    switch exceptionRecord(Extension({a: 1, b: 2})) {
    | Extension({a, b}) => eq(__LOC__, (a, b), (1, 5))
    | _ => ok(__LOC__, false)
    }
  )
})

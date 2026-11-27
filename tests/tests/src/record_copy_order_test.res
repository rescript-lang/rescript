@@config({flags: ["-bs-jsx", "4"]})

open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

/***
`{...base, a: e}` reads the mutable fields of `base` before `e` can assign
them, also when it copies the fields one by one.
*/

@inline(never) let id = x => x

type s = {a1: int, mutable b1: int}

let override = (base: s) => {
  ...base,
  a1: {
    base.b1 = 100
    id(5)
  },
}

let call = (base: s, f) => {...base, a1: f()}

let await_ = async (base: s, f) => {...base, a1: await f()}

/* The override runs no code: the field is read in place */
let pureOverride = (base: s) => {...base, a1: base.b1 + 1}

type i = {x: int, y: int}

/* Immutable fields are read in place */
let immutable = (base: i) => {...base, x: id(5)}

type many = {
  mutable first: int,
  second: int,
  mutable third: int,
  fourth: int,
  mutable fifth: int,
}

let twoOverrides = (base: many) => {
  ...base,
  second: {
    base.first = 100
    base.third = 100
    id(2)
  },
  fourth: try {
    base.fifth = 100
    id(4)
  } catch {
  | _ => 0
  },
}

type v = Inline({p: int, mutable q: int}) | Empty

let inlineRecord = v =>
  switch v {
  | Inline(x) =>
    Inline({
      ...x,
      p: {
        x.q = 100
        id(5)
      },
    })
  | Empty => Empty
  }

exception Extension({p: int, mutable q: int})

let exceptionRecord = e =>
  switch e {
  | Extension(x) =>
    Extension({
      ...x,
      p: {
        x.q = 100
        id(5)
      },
    })
  | e => e
  }

type exotic = {\"a-1": int, mutable \"b-1": int, @as("class") mutable default: int}

let exoticNames = (base: exotic, f) => {...base, \"a-1": f()}

/* No base to copy from: the optional field stays out */
type optional = {l: int, mutable m?: int}

let noBase = f => {l: f()}

let aliased = () => {
  let base = {a1: 1, b1: 1}
  let copy = {
    ...base,
    a1: {
      base.b1 = 7
      id(3)
    },
  }
  (copy.a1, copy.b1, base.b1)
}

type props = {className: string, mutable title: string}

module Jsx = {
  @react.componentWithProps
  let make = (props: props) => React.string(props.className ++ props.title)
}

let jsxProps = (base: props) =>
  <Jsx
    {...base}
    className={
      base.title = "changed"
      id("c")
    }
  />

describe(__MODULE__, () => {
  test("an override that assigns a field of the base", () => {
    let base = {a1: 1, b1: 1}
    eq(__LOC__, override(base), {a1: 5, b1: 1})
    eq(__LOC__, base, {a1: 1, b1: 100})
  })

  test("a call that assigns a field of the base", () => {
    let base = {a1: 1, b1: 1}
    let f = () => {
      base.b1 = 100
      5
    }
    eq(__LOC__, call(base, f), {a1: 5, b1: 1})
  })

  testAsync("an override that awaits", async () => {
    let base = {a1: 1, b1: 1}
    let promise = await_(
      base,
      async () => {
        await Promise.resolve()
        5
      },
    )
    base.b1 = 100
    eq(__LOC__, await promise, {a1: 5, b1: 1})
  })

  test("overrides that assign nothing", () => {
    eq(__LOC__, pureOverride({a1: 1, b1: 1}), {a1: 2, b1: 1})
    eq(__LOC__, immutable({x: 1, y: 2}), {x: 5, y: 2})
  })

  test("several overrides", () => {
    eq(
      __LOC__,
      twoOverrides({first: 1, second: 1, third: 1, fourth: 1, fifth: 1}),
      {first: 1, second: 2, third: 1, fourth: 4, fifth: 1},
    )
  })

  test("an inline record", () =>
    eq(__LOC__, inlineRecord(Inline({p: 1, q: 1})), Inline({p: 5, q: 1}))
  )

  test("an exception's record", () =>
    switch exceptionRecord(Extension({p: 1, q: 1})) {
    | Extension({p, q}) => eq(__LOC__, (p, q), (5, 1))
    | _ => ok(__LOC__, false)
    }
  )

  test("field names that aren't identifiers", () => {
    let base = {\"a-1": 1, \"b-1": 1, default: 1}
    let f = () => {
      base.\"b-1" = 100
      base.default = 100
      5
    }
    eq(__LOC__, exoticNames(base, f), {\"a-1": 5, \"b-1": 1, default: 1})
  })

  test("a record without a base", () => eq(__LOC__, noBase(() => id(1)), {l: 1}))

  test("a base that doesn't escape", () => eq(__LOC__, aliased(), (3, 1, 7)))

  test("JSX props", () => {
    let element: {"props": props} = Obj.magic(jsxProps({className: "", title: "t"}))
    eq(__LOC__, element["props"], {className: "c", title: "t"})
  })
})

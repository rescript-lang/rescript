@@config({flags: ["-bs-jsx", "4"]})

open Mocha
open Test_utils

/***
`<Foo {...base} b={e} />` copies `base` before `e` is evaluated, also when `e`
needs statements.
*/

@inline(never) let id = x => x

module Foo = {
  type props = {mutable a: int, b?: int, c?: int, children?: React.element}

  @react.componentWithProps
  let make = (props: props) => React.int(props.a)
}

@get external props: React.element => Foo.props = "props"
@get external key: React.element => Nullable.t<string> = "key"

let order = []

let clearOrder = () => order->Array.splice(~start=0, ~remove=Array.length(order), ~insert=[])

@inline(never)
let trace = (name, value) => {
  order->Array.push(name)
  value
}

@inline(never)
let setA = (base: Foo.props, value) => {
  base.a = 100
  value
}

let mutateInProp = (base: Foo.props) =>
  <Foo
    {...base}
    b={try {
      base.a = 100
      id(5)
    } catch {
    | _ => 0
    }}
  />

/* The call is held, so the base is copied before it too */
let mutateInEarlierProp = (base: Foo.props) =>
  <Foo
    {...base}
    b={setA(base, 1)}
    c={try id(5) catch {
    | _ => 0
    }}
  />

let mutateInChild = (base: Foo.props) =>
  <Foo {...base}>
    {try {
      base.a = 100
      React.int(id(5))
    } catch {
    | _ => React.null
    }}
  </Foo>

let mutateInKey = (base: Foo.props) =>
  <Foo
    key={try {
      base.a = 100
      id("k")
    } catch {
    | _ => ""
    }}
    {...base}
    b=1
  />

let traced = (base: Foo.props) =>
  <Foo
    {...trace("base", base)}
    b={trace("b", 1)}
    c={try trace("c", id(5)) catch {
    | _ => 0
    }}
  />

/* The statements only define pure values: no copy */
let pureStatements = (base: Foo.props, x: Foo.props) =>
  <Foo
    {...base}
    b={
      let n = x.a
      n + n
    }
  />

/* No statements: no copy */
let expression = (base: Foo.props) => <Foo {...base} b={setA(base, 9)} />

/* A lone spread is passed itself */
let loneSpread = (base: Foo.props) =>
  <Foo
    key={try id("k") catch {
    | _ => ""
    }}
    {...base}
  />

describe(__MODULE__, () => {
  test("a prop that mutates the base", () => {
    let base: Foo.props = {a: 1, b: 2, c: 3}
    let res = props(mutateInProp(base))
    eq(__LOC__, (res.a, res.b, res.c), (1, Some(5), Some(3)))
    eq(__LOC__, base.a, 100)
  })

  test("a prop before the one that needs statements mutates the base", () => {
    let res = props(mutateInEarlierProp({a: 1}))
    eq(__LOC__, (res.a, res.b, res.c), (1, Some(1), Some(5)))
  })

  test("a child that mutates the base", () => {
    let res = props(mutateInChild({a: 1}))
    eq(__LOC__, (res.a, res.children), (1, Some(React.int(5))))
  })

  test("a key that mutates the base", () => {
    let res = mutateInKey({a: 1})
    eq(__LOC__, (props(res).a, props(res).b), (1, Some(1)))
    eq(__LOC__, key(res), Nullable.make("k"))
  })

  test("evaluation order", () => {
    clearOrder()
    ignore(traced({a: 1}))
    eq(__LOC__, order, ["base", "b", "c"])
  })

  test("a prop whose statements are pure", () => {
    let res = props(pureStatements({a: 1}, {a: 3}))
    eq(__LOC__, (res.a, res.b), (1, Some(6)))
  })

  test("a prop without statements", () => {
    let res = props(expression({a: 1}))
    eq(__LOC__, (res.a, res.b), (1, Some(9)))
  })

  test("a lone spread", () => {
    let res = loneSpread({a: 1})
    eq(__LOC__, (props(res).a, key(res)), (1, Nullable.make("k")))
  })
})

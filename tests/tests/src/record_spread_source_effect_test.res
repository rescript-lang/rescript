@@warning("-23")
@@config({flags: ["-bs-jsx", "4"]})

open Mocha
open Test_utils

type point = {x: int, y: int}
type options = {width?: int, height: int}
type polymorphic<'a> = {compare: option<('a, 'a) => int>, first: option<'a>}
type rec recursive = {self: unit => recursive, value: int}
type callback = {run: unit => int}
type rec node = {next: node, value: int}

module Component = {
  @react.component
  let make = (~a: int, ~b: int=2) => React.int(a + b)
}

@inline(never)
let id = x => x

let calls = []

@inline(never)
let logged = (name, value) => {
  calls->Array.push(name)
  value
}

let takeCalls = () => {
  let taken = calls->Array.copy
  calls->Array.splice(~start=0, ~remove=calls->Array.length, ~insert=[])
  taken
}

// The source doesn't stop the record from being polymorphic
let polymorphic = {
  ...logged("polymorphic", id({compare: None, first: Some(1)})),
  compare: None,
  first: None,
}

let strings: polymorphic<string> = polymorphic
let ints: polymorphic<int> = polymorphic

let rec base = {self: () => base, value: 0}

let rec recursive = {
  ...logged("recursive", id(base)),
  self: () => recursive,
  value: 2,
}

let rec selfSpread = {...selfSpread, self: () => selfSpread, value: 3}

let rec singleCallback = {...singleCallback, run: () => 4}

let makeNode = value => {
  let rec node = {next: node, value}
  node
}

// The record still refers to itself once its source has been evaluated
let rec cycle = {...logged("cycle", makeNode(0)), next: cycle, value: 7}

let rec letCycle = {
  let value = logged("letCycle", id(8))
  {...logged("letCycleSource", makeNode(0)), next: letCycle, value}
}

// The same holds for a record built at the end of a sequence
let rec sequenceCycle = {
  ignore(logged("sequenceCycle", ()))
  {next: sequenceCycle, value: 9}
}

let moduleCalls = takeCalls()

// The source is evaluated, so the module it comes from stays imported when it
// has side effects, even though its value is not needed
let fromOtherModule = {...Record_spread_source_effect_dep.base, x: 5, y: 6}

let overridePure = (source: point) => {...source, x: 1, y: logged("y", 2)}

let overrideJsxPure = props => <Component {...props} a=1 b=2 />

describe(__MODULE__, () => {
  test("a source whose fields are all overridden runs first", () => {
    let point = {...logged("source", {x: 0, y: 0}), x: logged("x", id(1)), y: logged("y", id(2))}
    eq(__LOC__, takeCalls(), ["source", "x", "y"])
    eq(__LOC__, point, {x: 1, y: 2})
  })

  test("a source of a record with optional fields runs", () => {
    let options = {...logged("source", {height: 0}), width: 1, height: 2}
    eq(__LOC__, takeCalls(), ["source"])
    eq(__LOC__, options, {width: 1, height: 2})
    // A copy of the source would have `height` first
    eq(__LOC__, JSON.stringifyAny(options), Some(`{"width":1,"height":2}`))
  })

  test("a source that raises stops the record from being built", () => {
    let result = try {
      let point = {...logged("source", JsError.throwWithMessage("source")), x: 1, y: 2}
      point.x
    } catch {
    | JsExn(_) => -1
    }
    eq(__LOC__, result, -1)
  })

  test("the source of a JSX element's props runs", () => {
    let element = <Component {...logged("props", {Component.a: 0})} a=1 b=2 />
    eq(__LOC__, takeCalls(), ["props"])
    ignore(element)
  })

  test("sources of module-level and recursive records run", () => {
    eq(
      __LOC__,
      moduleCalls,
      ["polymorphic", "recursive", "cycle", "letCycle", "letCycleSource", "sequenceCycle"],
    )
    eq(__LOC__, strings.first, None)
    eq(__LOC__, ints.first, None)
    eq(__LOC__, recursive.self().value, 2)
    eq(__LOC__, selfSpread.self().value, 3)
    eq(__LOC__, singleCallback.run(), 4)
    eq(__LOC__, cycle.next === cycle, true)
    eq(__LOC__, cycle.next.value, 7)
    eq(__LOC__, letCycle.next === letCycle, true)
    eq(__LOC__, letCycle.next.value, 8)
    eq(__LOC__, sequenceCycle.next === sequenceCycle, true)
    eq(__LOC__, sequenceCycle.next.value, 9)
    eq(__LOC__, fromOtherModule, {x: 5, y: 6})
  })

  test("a pure source is left out", () => {
    eq(__LOC__, overridePure(id({x: 0, y: 0})), {x: 1, y: 2})
    eq(__LOC__, takeCalls(), ["y"])
    ignore(overrideJsxPure(id({Component.a: 0})))
  })
})

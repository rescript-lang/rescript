open Mocha
open Test_utils

let mapped = Some(2)->Option.map(x => x + 1)
let flatMapped = Some(2)->Option.flatMap(x => Some(x + 2))
let nested: option<option<int>> = Some(None)
let mappedNested = nested->Option.map(x => x)
let mapValue = opt => opt->Option.map(x => x + 1)
let flatMapValue = opt => opt->Option.flatMap(x => Some(x + 2))
let forEachValue = (opt, f) => opt->Option.forEach(f)

let visited: array<int> = []
let _ = Some(4)->Option.forEach(x => visited->Array.push(x))
let _ = None->Option.forEach(_ => visited->Array.push(0))

let fromExport = Cross_module_inline_export.apply(3, x => x + 1)
let fromOrdinary = Cross_module_inline_export.ordinary(3, x => x + 1)

describe("cross-module inline opt-in", () => {
  test("Option helpers", () => {
    eq(__LOC__, mapped, Some(3))
    eq(__LOC__, flatMapped, Some(4))
    eq(__LOC__, mappedNested, Some(None))
    eq(__LOC__, mapValue(Some(2)), Some(3))
    eq(__LOC__, mapValue(None), None)
    eq(__LOC__, flatMapValue(Some(2)), Some(4))
    eq(__LOC__, flatMapValue(None), None)
    forEachValue(Some(5), x => visited->Array.push(x))
    eq(__LOC__, visited, [4, 5])
  })
  test("only marked exports are inlined", () => {
    eq(__LOC__, fromExport, 4)
    eq(__LOC__, fromOrdinary, 4)
  })
})

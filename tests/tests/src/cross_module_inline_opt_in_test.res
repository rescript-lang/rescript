open Mocha
open Test_utils

let mapped = Some(2)->Option.map(x => x + 1)
let flatMapped = Some(2)->Option.flatMap(x => Some(x + 2))
let nested: option<option<int>> = Some(None)
let mappedNested = nested->Option.map(x => x)
let mapValue = opt => opt->Option.map(x => x + 1)
let flatMapValue = opt => opt->Option.flatMap(x => Some(x + 2))
let forEachValue = (opt, f) => opt->Option.forEach(f)
let filterValue = (opt, p) => opt->Option.filter(p)
let mapOrValue = (opt, default, f) => opt->Option.mapOr(default, f)
let getOrValue = (opt, default) => opt->Option.getOr(default)
let orElseValue = (opt, other) => opt->Option.orElse(other)
let isSomeValue = opt => opt->Option.isSome
let isNoneValue = opt => opt->Option.isNone

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
  test("additional Option helpers", () => {
    eq(__LOC__, filterValue(Some(4), x => x > 3), Some(4))
    eq(__LOC__, filterValue(Some(2), x => x > 3), None)
    eq(__LOC__, filterValue(None, _ => true), None)
    eq(__LOC__, mapOrValue(Some(2), 0, x => x + 1), 3)
    eq(__LOC__, mapOrValue(None, 0, x => x + 1), 0)
    eq(__LOC__, getOrValue(Some(2), 0), 2)
    eq(__LOC__, getOrValue(None, 0), 0)
    eq(__LOC__, orElseValue(Some(2), Some(3)), Some(2))
    eq(__LOC__, orElseValue(None, Some(3)), Some(3))
    eq(__LOC__, isSomeValue(Some(2)), true)
    eq(__LOC__, isSomeValue(None), false)
    eq(__LOC__, isNoneValue(Some(2)), false)
    eq(__LOC__, isNoneValue(None), true)
  })
  test("eager fallback arguments", () => {
    let effects = []
    let getDefault = () => {
      effects->Array.push("getOr")
      0
    }
    let mapDefault = () => {
      effects->Array.push("mapOr")
      0
    }
    let other = () => {
      effects->Array.push("orElse")
      Some(0)
    }
    eq(__LOC__, Some(2)->Option.getOr(getDefault()), 2)
    eq(__LOC__, Some(2)->Option.mapOr(mapDefault(), x => x), 2)
    eq(__LOC__, Some(2)->Option.orElse(other()), Some(2))
    eq(__LOC__, effects, ["getOr", "mapOr", "orElse"])
  })
})

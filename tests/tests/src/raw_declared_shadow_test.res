// A name that `%%raw` code declares in the module is no property of
// `globalThis`. A binding with the same name is renamed instead, and the name
// is read as it is.
open Mocha
open Test_utils

%%raw(`
function rawHelper() { return 1 }
const rawLimit = 10
if (true) { var rawHoisted = 41 }
function rawTop() { return 2 }
var rawCache = {a: 100}
`)

module G = {
  @val external rawHelper: unit => int = "rawHelper"
  @val external rawLimit: int = "rawLimit"
  @val external rawHoisted: int = "rawHoisted"
  @val external rawTop: unit => int = "rawTop"
}

// local binding initialized from the raw function
let local = () => {
  let rawHelper = G.rawHelper() + 1
  rawHelper * rawHelper
}

// parameter
let param = (rawLimit: int) => rawLimit * rawLimit + G.rawLimit

// loop variable
let loop = () => {
  let sum = ref(0)
  for rawLimit in 0 to 2 {
    sum := sum.contents + rawLimit * rawLimit + G.rawLimit
  }
  sum.contents
}

// a `var` hoisted out of a block of the raw code
let hoisted = () => {
  let rawHoisted = G.rawHoisted + 1
  rawHoisted * 2
}

// toplevel binding, exported under its own name
let rawTop = () => G.rawTop() + 1

// The program never reads `rawCache` as a global, so the parameter keeps its
// name and the raw snippet reads the parameter.
let fromParam = (rawCache: dict<int>, key: string): int => {
  ignore(rawCache)
  ignore(key)
  %raw(`rawCache[key]`)
}

describe(__LOC__, () => {
  test("names declared by %%raw shadowed by bindings with the same name", () => {
    eq(__LOC__, local(), 4)
    eq(__LOC__, param(3), 19)
    eq(__LOC__, loop(), 35)
    eq(__LOC__, hoisted(), 84)
    eq(__LOC__, rawTop(), 3)
    eq(__LOC__, fromParam(dict{"a": 1}, "a"), 1)
  })
})

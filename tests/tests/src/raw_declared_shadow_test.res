// A name that `%%raw` code declares in the module is no property of
// `globalThis`. A binding with the same name is renamed instead, and the name
// is read as it is.
open Mocha
open Test_utils

%%raw(`
function rawHelper() { return 1 }
const rawLimit = 10
`)

module G = {
  @val external rawHelper: unit => int = "rawHelper"
  @val external rawLimit: int = "rawLimit"
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

describe(__LOC__, () => {
  test("names declared by %%raw shadowed by bindings with the same name", () => {
    eq(__LOC__, local(), 4)
    eq(__LOC__, param(3), 19)
    eq(__LOC__, loop(), 35)
  })
})

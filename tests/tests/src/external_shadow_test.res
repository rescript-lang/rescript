// A reference to a JS global must not be captured by a binding with the same
// name: wherever such a binding is in scope, the global is read through
// `globalThis`.
open Mocha
open Test_utils

%%raw(`
globalThis.shadowA = 10;
globalThis.shadowB = 20;
globalThis.shadowC = 30;
globalThis.shadowD = 40;
globalThis.shadowE = 50;
globalThis.shadowF = 60;
globalThis.Test_utils = 70;
`)

module G = {
  @val external shadowA: int = "shadowA"
  @val external shadowB: int = "shadowB"
  @val external shadowC: int = "shadowC"
  @val external shadowD: int = "shadowD"
  @val external shadowE: int = "shadowE"
  @val external shadowF: int = "shadowF"
  // same name as the imported module Test_utils
  @val external testUtils: int = "Test_utils"
}

// toplevel binding before any use of the global: the later reads used to get
// the binding instead
let shadowA = (n: int) => n * n + G.shadowA
let readAAfter = () => G.shadowA
let valueA = G.shadowA

// toplevel binding after a function that reads the global: it is in scope in
// the whole module, also before its declaration
let readFBefore = () => G.shadowF
let shadowF = (n: int) => n * n * n + G.shadowF

// parameter
let paramB = (shadowB: int) => shadowB * shadowB + G.shadowB

// local binding after a read in the same block
let localC = (n: int) => {
  let before = G.shadowC + n
  let shadowC = before * before + n
  shadowC * shadowC + G.shadowC
}

// binding in one case of a switch, read in another: the cases share a scope
let switchD = (x: int) =>
  switch x {
  | 0 =>
    let shadowD = x * x + 7
    shadowD * shadowD + G.shadowD
  | 1 => G.shadowD + 1
  | 2 => G.shadowD * 2
  | 3 => G.shadowD - 3
  | _ => G.shadowD + x
  }

// loop variable
let loopE = () => {
  let sum = ref(0)
  for shadowE in 0 to 2 {
    sum := sum.contents + shadowE * shadowE + G.shadowE
  }
  sum.contents
}

describe(__LOC__, () => {
  test("globals shadowed by bindings with the same name", () => {
    eq(__LOC__, shadowA(2), 14)
    eq(__LOC__, readAAfter(), 10)
    eq(__LOC__, valueA, 10)
    eq(__LOC__, readFBefore(), 60)
    eq(__LOC__, shadowF(2), 68)
    eq(__LOC__, paramB(3), 29)
    eq(__LOC__, localC(1), 925474)
    eq(__LOC__, switchD(0), 89)
    eq(__LOC__, switchD(1), 41)
    eq(__LOC__, switchD(2), 80)
    eq(__LOC__, switchD(3), 37)
    eq(__LOC__, switchD(5), 45)
    eq(__LOC__, loopE(), 155)
    eq(__LOC__, G.testUtils, 70)
  })
})

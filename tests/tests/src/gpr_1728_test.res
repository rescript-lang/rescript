open Mocha
open Test_utils

@scope("Number") external parseInt: string => int = "parseInt"

let foo = x => parseInt(x) !== 3

let badInlining = obj =>
  if foo(obj["field"]) {
    ()
  }

eq(__LOC__, badInlining({"field": "3"}), ())

eq(__LOC__, parseInt("-13"), -13)
eq(__LOC__, parseInt("+13"), 13)
eq(__LOC__, parseInt("13"), 13)
eq(__LOC__, parseInt("+0x32"), 50)
eq(__LOC__, parseInt("-0x32"), -50)
eq(__LOC__, parseInt("0x32"), 50)
describe(__MODULE__, () => {
  test("bad_inlining_test", () => {
    eq(__LOC__, badInlining({"field": "3"}), ())
  })

  test("parseInt_tests", () => {
    eq(__LOC__, parseInt("-13"), -13)
    eq(__LOC__, parseInt("+13"), 13)
    eq(__LOC__, parseInt("13"), 13)
    eq(__LOC__, parseInt("+0x32"), 50)
    eq(__LOC__, parseInt("-0x32"), -50)
    eq(__LOC__, parseInt("0x32"), 50)
  })
})

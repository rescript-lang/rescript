open Mocha
open Test_utils

let make = () => /a/g

describe(__MODULE__, () => {
  test("fresh instances and independent lastIndex", () => {
    let first = make()
    let second = make()
    eq(__LOC__, first === second, false)
    eq(__LOC__, first->RegExp.test("aa"), true)
    eq(__LOC__, first->RegExp.lastIndex, 1)
    eq(__LOC__, second->RegExp.lastIndex, 0)
    eq(__LOC__, first->RegExp.test("aa"), true)
    eq(__LOC__, first->RegExp.test("aa"), false)
    eq(__LOC__, first->RegExp.lastIndex, 0)
  })
  test("escapes and flags survive lowering", () => {
    eq(__LOC__, /a\/b\d/i->RegExp.test("A/b2"), true)
    eq(__LOC__, /[/]/->RegExp.test("/"), true)
    eq(__LOC__, /世界/u->RegExp.test("世界"), true)
  })
})

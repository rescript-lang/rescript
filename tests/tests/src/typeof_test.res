open Mocha
open Test_utils

let string_or_number = (type t, x) => {
  let ty = Js.Types.classify(x)
  switch ty {
  | JSString(v) =>
    Js.log(v ++ "hei")
    true /* type check */
  | JSNumber(v) =>
    Js.log(v +. 3.)
    true /* type check */
  | JSUndefined => false
  | JSNull => false
  | JSFalse | JSTrue => false
  | JSFunction(_) =>
    Js.log("Function")
    false
  | JSObject(_) => false
  | JSSymbol(_) => false
  | JSBigInt(v) =>
    v->Js.BigInt.toString->Js.log
    true
  }
}

describe(__MODULE__, () => {
  test("int_type", () => {
    eq(__LOC__, Js.typeof(3), "number")
  })

  test("string_type", () => {
    eq(__LOC__, Js.typeof("x"), "string")
  })

  test("number_gadt_test", () => {
    eq(__LOC__, Js.Types.test(3, Number), true)
  })

  test("boolean_gadt_test", () => {
    eq(__LOC__, Js.Types.test(true, Boolean), true)
  })

  test("undefined_gadt_test", () => {
    eq(__LOC__, Js.Types.test(Js.undefined, Undefined), true)
  })

  test("string_on_number1", () => {
    eq(__LOC__, string_or_number("xx"), true)
  })

  test("string_on_number2", () => {
    eq(__LOC__, string_or_number(3.02), true)
  })

  test("string_on_number3", () => {
    eq(__LOC__, string_or_number(x => x), false)
  })

  test("string_gadt_test", () => {
    eq(__LOC__, Js.Types.test("3", String), true)
  })

  test("string_gadt_test_neg", () => {
    eq(__LOC__, Js.Types.test(3, String), false)
  })

  test("function_gadt_test", () => {
    eq(__LOC__, Js.Types.test(x => x, Function), true)
  })

  test("object_gadt_test", () => {
    eq(__LOC__, Js.Types.test({"x": 3}, Object), true)
  })
})

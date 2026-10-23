open Mocha
open Test_utils

let string_or_number = (type t, x) => {
  let ty = Type.Classify.classify(x)
  switch ty {
  | String(v) =>
    Console.log(v ++ "hei")
    true /* type check */
  | Number(v) =>
    Console.log(v +. 3.)
    true /* type check */
  | Undefined => false
  | Null => false
  | Bool(_) => false
  | Function(_) =>
    Console.log("Function")
    false
  | Object(_) => false
  | Symbol(_) => false
  | BigInt(v) =>
    v->BigInt.toString->Console.log
    true
  }
}

describe(__MODULE__, () => {
  test("int_type", () => {
    eq(__LOC__, Type.typeof(3), #number)
  })

  test("string_type", () => {
    eq(__LOC__, Type.typeof("x"), #string)
  })

  test("number_gadt_test", () => {
    eq(__LOC__, Type.typeof(3), #number)
  })

  test("boolean_gadt_test", () => {
    eq(__LOC__, Type.typeof(true), #boolean)
  })

  test("undefined_gadt_test", () => {
    eq(__LOC__, Type.typeof(undefined), #undefined)
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
    eq(__LOC__, Type.typeof("3"), #string)
  })

  test("string_gadt_test_neg", () => {
    eq(__LOC__, Type.typeof(3) == #string, false)
  })

  test("function_gadt_test", () => {
    eq(__LOC__, Type.typeof(x => x), #function)
  })

  test("object_gadt_test", () => {
    eq(__LOC__, Type.typeof({"x": 3}), #object)
  })
})

open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("flat cross-module exports", () => {
    eq(__LOC__, Hoisted_function_attr_use.one, "one")
    eq(__LOC__, Hoisted_function_attr_use.two, "two")
    eq(__LOC__, Hoisted_function_attr_use.three, "three")
  })

  test("exotic identifiers", () => {
    eq(__LOC__, Hoisted_function_attr_use.keyword, "keyword")
    eq(__LOC__, Hoisted_function_attr_use.dollar, "dollar")
    eq(__LOC__, Hoisted_function_attr_use.operator, "operator")
  })

  test("structurally distinct paths", () => {
    eq(__LOC__, Hoisted_function_attr_use.nested, "nested")
    eq(__LOC__, Hoisted_function_attr_use.exoticPath, "exotic")
  })

  test("recursive modules", () => {
    eq(__LOC__, Hoisted_function_attr_use.recursive, "recursive")
  })

  test("explicit function type annotations", () => {
    eq(__LOC__, Hoisted_function_attr_use.typed, "typed")
  })
})

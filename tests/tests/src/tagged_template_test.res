open Mocha
open Test_utils

// A tag bound with the builtin `taggedTemplate` type. No decorator needed: the
// type is what makes the compiler emit real JS tagged-template syntax.
module Pg = {
  @module("./tagged_template_lib.js")
  external sql: taggedTemplate<string, string> = "sql"
}

let table = "users"
let id = "5"

let queryWithModule = Pg.sql`SELECT * FROM ${table} WHERE id = ${id}`

// The tag still emits backticks when used through `open` (i.e. once it has
// crossed the module boundary as a value of the `taggedTemplate` type).
open Pg
let query = sql`
" SELECT * FROM ${table} WHERE id = ${id}`

@module("./tagged_template_lib.js")
external length: taggedTemplate<int, int> = "length"

let extraLength = 10
let length = length`hello ${extraLength} what's the total length? Is it ${3}?`

// Problem 1: a tag constructed at runtime by a factory (postgres-style).
@module("./tagged_template_lib.js")
external makeSql: string => taggedTemplate<string, string> = "makeSql"

let prefixedSql = makeSql("PREFIX ")
let factoryQuery = prefixedSql`SELECT * FROM ${table}`

// Problem 2c: the tag flows through a function parameter and is still emitted
// as a real tagged template at the call site inside the function.
let runQuery = (tag: taggedTemplate<string, string>) => tag`SELECT id = ${id}`
let paramQuery = runQuery(Pg.sql)

// Problem 3: a ReScript-authored tag, lifted into the type with
// `TaggedTemplate.make`, is usable as a tag too.
type params = I(int) | S(string)

let s = TaggedTemplate.make((strings, parameters) => {
  Array.reduceWithIndex(parameters, Array.getUnsafe(strings, 0), (acc, param, i) => {
    let suffix = Array.getUnsafe(strings, i + 1)
    let p = switch param {
    | I(i) => Int.toString(i)
    | S(s) => s
    }
    acc ++ p ++ suffix
  })
})

let greeting = s`hello ${S("Ada")} you're ${I(36)} years old!`

describe("tagged templates", () => {
  test("with externals, it should return a string with the correct interpolations", () =>
    eq(
      __LOC__,
      query,
      `
" SELECT * FROM 'users' WHERE id = '5'`,
    )
  )

  test(
    "with module scoped externals, it should also return a string with the correct interpolations",
    () => eq(__LOC__, queryWithModule, "SELECT * FROM 'users' WHERE id = '5'"),
  )

  test("with externals, it should return the result of the function", () => eq(__LOC__, length, 52))

  test("with a runtime-constructed tag (factory), it should emit tagged-template syntax", () =>
    eq(__LOC__, factoryQuery, "PREFIX SELECT * FROM 'users'")
  )

  test("with a tag passed as a function argument, it should emit tagged-template syntax", () =>
    eq(__LOC__, paramQuery, "SELECT id = '5'")
  )

  test(
    "with a ReScript tag lifted via TaggedTemplate.make, it should return the correct interpolation",
    () => eq(__LOC__, greeting, "hello Ada you're 36 years old!"),
  )

  test(
    "a template literal tagged with json should generate a regular string interpolation for now",
    () => eq(__LOC__, json`some random ${"string"}`, "some random string"),
  )

  test("a regular string interpolation should continue working", () =>
    eq(__LOC__, `some random ${"string"} interpolation`, "some random string interpolation")
  )
})

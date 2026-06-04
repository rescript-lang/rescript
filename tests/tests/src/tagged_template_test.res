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

// Problem 2b: a `taggedTemplate` value defined in another module, consumed here
// with backtick syntax. `Tagged_template_binding` only exposes `sql`'s type, yet
// the call site still emits a real tagged template.
let crossModuleQuery = Tagged_template_binding.sql`SELECT * FROM ${table}`

// Proof that the compiler emits a *real* JS tagged template (a frozen
// `TemplateStringsArray` with `.raw`) rather than a plain/variadic function
// call. `rawTag` inspects the argument it receives.
type rawCall = {
  hasRaw: bool,
  raw: array<string>,
  cooked: array<string>,
  values: array<int>,
}

@module("./tagged_template_lib.js")
external rawTag: taggedTemplate<int, rawCall> = "rawTag"

let rawResult = rawTag`a ${1} b ${2} c`

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

  test("with a tag imported from another module, it should emit tagged-template syntax", () =>
    eq(__LOC__, crossModuleQuery, "X: SELECT * FROM 'users'")
  )

  test("it should call the tag as a real tagged template (TemplateStringsArray with .raw)", () => {
    eq(__LOC__, rawResult.hasRaw, true)
    eq(__LOC__, rawResult.cooked, ["a ", " b ", " c"])
    eq(__LOC__, rawResult.raw, ["a ", " b ", " c"])
    eq(__LOC__, rawResult.values, [1, 2])
  })

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

module Array = Ocaml_Array

module Pg = {
  @module("./tagged_template_lib.js") @taggedTemplate
  external sql: (array<string>, array<string>) => string = "sql"
}

let table = "users"
let id = "5"

let queryWithModule = Pg.sql`SELECT * FROM ${table} WHERE id = ${id}`

open Pg
let query = sql`
" SELECT * FROM ${table} WHERE id = ${id}`

@module("./tagged_template_lib.js") @taggedTemplate
external length: (array<string>, array<int>) => int = "length"

let extraLength = 10
let length = length`hello ${extraLength} what's the total length? Is it ${3}?`

let foo = (strings, values) => {
  let res = ref("")
  let valueCount = Belt.Array.length(values)
  for i in 0 to valueCount - 1 {
    res := res.contents ++ strings[i] ++ Js.Int.toString(values[i] * 10)
  }
  res.contents ++ strings[valueCount]
}

let res = foo`| 5 × 10 = ${5} |`

Mt.from_pair_suites(
  "tagged templates",
  list{
    (
      "with externals, it should return a string with the correct interpolations",
      () => Eq(
        query,
        `
" SELECT * FROM 'users' WHERE id = '5'`,
      ),
    ),
    (
      "with module scoped externals, it should also return a string with the correct interpolations",
      () => Eq(queryWithModule, "SELECT * FROM 'users' WHERE id = '5'"),
    ),
    ("with externals, it should return the result of the function", () => Eq(length, 52)),
    (
      "with rescript function, it should return a string with the correct encoding and interpolations",
      () => Eq(res, "| 5 × 10 = 50 |"),
    ),
    (
      "a template literal tagged with json should generate a regular string interpolation for now",
      () => Eq(json`some random ${"string"}`, "some random string"),
    ),
    (
      "a regular string interpolation should continue working",
      () => Eq(`some random ${"string"} interpolation`, "some random string interpolation"),
    ),
  },
)

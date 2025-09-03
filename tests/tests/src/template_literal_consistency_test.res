/* Test case for template literal compilation consistency
 * This should demonstrate that both external and ReScript tagged template calls
 * generate the same JavaScript template literal syntax
 */

// External tagged template (already works correctly)
@module("./lib.js") @taggedTemplate
external sqlExternal: (array<string>, array<string>) => string = "sql"

// ReScript function that should now also generate template literal syntax
let sqlReScript = (strings, values) => {
  // Simple implementation for testing
  let result = ref("")
  let valCount = Belt.Array.length(values)
  for i in 0 to valCount - 1 {
    result := result.contents ++ strings[i] ++ Belt.Int.toString(values[i])
  }
  result.contents ++ strings[valCount]
}

// Regular function with two array args - should NOT be treated as template literal
let regularFunction = (arr1, arr2) => {
  "regular function result"
}

// Test data
let table = "users" 
let id = 42

// Both calls should now generate identical JavaScript template literal syntax:
// sqlExternal`SELECT * FROM ${table} WHERE id = ${id}`
// sqlReScript`SELECT * FROM ${table} WHERE id = ${id}`
let externalResult = sqlExternal`SELECT * FROM ${table} WHERE id = ${id}`
let rescriptResult = sqlReScript`SELECT * FROM ${table} WHERE id = ${id}`

// Simple cases
let simple1 = sqlExternal`hello ${123} world`
let simple2 = sqlReScript`hello ${123} world`

// Edge cases: empty interpolations
let empty1 = sqlExternal`no interpolations`
let empty2 = sqlReScript`no interpolations`

// Regular function call (should remain as function call, not template literal)
let regularCall = regularFunction(["not", "template"], ["literal", "call"])

// Test various data types
let numberTest1 = sqlExternal`number: ${42}`
let numberTest2 = sqlReScript`number: ${42}`

let stringTest1 = sqlExternal`string: ${"test"}`  
let stringTest2 = sqlReScript`string: ${"test"}`
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

// Test data
let table = "users"
let id = 42

// Both calls should now generate identical JavaScript template literal syntax:
// sqlExternal`SELECT * FROM ${table} WHERE id = ${id}`
// sqlReScript`SELECT * FROM ${table} WHERE id = ${id}`
let externalResult = sqlExternal`SELECT * FROM ${table} WHERE id = ${id}`
let rescriptResult = sqlReScript`SELECT * FROM ${table} WHERE id = ${id}`

// Simple case
let simple1 = sqlExternal`hello ${123} world`
let simple2 = sqlReScript`hello ${123} world`
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

// NEW: Test regular template literals (the main issue)
// These should generate template literal syntax instead of string concatenation

let name = "World"
let count = 42

// Basic template literal with one interpolation
let basicTemplate = `Hello ${name}!`

// Template literal with multiple interpolations  
let multiTemplate = `Hello ${name}, you have ${count} messages`

// Template literal with number interpolation
let numberTemplate = `Count: ${count}`

// Template literal with mixed types
let mixedTemplate = `User: ${name} (${count} years old)`

// Template literals with empty strings
let emptyStart = `${name} is here`
let emptyEnd = `Welcome ${name}`
let emptyMiddle = `${name}${count}`

// Template literal with just interpolation (edge case)
let justInterpolation = `${name}`

// Nested template expressions
let nested = `Outer: ${`Inner: ${name}`}`
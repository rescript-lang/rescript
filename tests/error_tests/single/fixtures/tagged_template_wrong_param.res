// The interpolated values must match the tag's `'param` type. Here `sql`
// expects `int` interpolations, but a string is interpolated.
@module("./sql.js")
external sql: taggedTemplate<int, string> = "sql"

let result = sql`SELECT * FROM users WHERE id = ${"not-an-int"}`

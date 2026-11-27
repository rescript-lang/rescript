// A `taggedTemplate` value can only be used with backtick syntax, not called
// as a regular function. The type is not a function type, so applying it to
// arguments is a type error.
@module("./sql.js")
external sql: taggedTemplate<string, string> = "sql"

let result = sql(["SELECT * FROM users WHERE id = ", ""], ["5"])

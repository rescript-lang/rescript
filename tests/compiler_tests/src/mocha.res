@module("mocha")
external test: (string, unit => unit) => unit = "test"

@module("mocha")
external describe: (string, unit => unit) => unit = "describe"

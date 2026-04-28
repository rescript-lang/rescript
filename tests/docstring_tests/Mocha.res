@module("mocha")
external test: (string, unit => unit) => unit = "test"

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

@module("mocha")
external describe: (string, unit => unit) => unit = "describe"

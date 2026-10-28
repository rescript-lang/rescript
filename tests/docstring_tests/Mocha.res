@module("mocha")
external test: (string, unit => unit) => unit = "test"

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

@module("mocha") @scope("test")
external testSkip: (string, unit => unit) => unit = "skip"

@module("mocha") @scope("test")
external testAsyncSkip: (string, unit => promise<unit>) => unit = "skip"

@module("mocha")
external describe: (string, unit => unit) => unit = "describe"

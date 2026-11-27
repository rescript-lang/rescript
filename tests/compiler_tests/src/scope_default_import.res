// A leading "default" scope imports the module's default export

@module("mod1") @scope(("default", "x")) external fn: int => unit = "fn"
@module("mod1") @scope("default") external value: int = "value"
@module("mod1") @new @scope("default") external make: unit => {"a": int} = "Klass"
@module("mod1") @scope("other") external other: int = "other"

@module("mod2") @scope("default") external defaultOfDefault: int = "default"

let f = () => fn(value + other + defaultOfDefault)
let g = () => make()

// https://github.com/rescript-lang/rescript/issues/8055

let foo = (): option<array<string>> => Some(["foo"])
let bar = foo()

let nested = (): option<option<int>> => Some(Some(1))
let baz = nested()

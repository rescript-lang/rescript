let foo = (): option<array<string>> => Some(["foo"])
let bar = foo()

let nested = (): option<option<int>> => Some(Some(1))
let baz = nested()

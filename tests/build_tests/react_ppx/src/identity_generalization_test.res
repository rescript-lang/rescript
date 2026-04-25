external id: 'a => 'a = "%identity"

// `%identity` applications should generalize like the wrapped expression.
// Otherwise, `f` becomes monomorphic and one of these calls fails to typecheck.
let f = id(x => x)

let intValue = f(1)
let stringValue = f("one")

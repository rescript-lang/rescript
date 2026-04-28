// Legacy uncurried `(. ...)` syntax — accepted for backwards compatibility
// but emits a deprecation warning.

let f = (. x) => x + 1
let g = (. a, b) => a + b
let h = (.) => 42

let r1 = f(. 3)
let r2 = g(. 1, 2)
let r3 = h(.)

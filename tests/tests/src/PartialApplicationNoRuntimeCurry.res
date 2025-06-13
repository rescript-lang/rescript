let add = x => (y, z) => x + y + z

let f = u => {
  let f = add(u)
  f(1, ...)
}

// Test partial application with user-defined function type
type fn2 = (int, int) => int
let add: fn2 = (a, b) => a + b
let add5: int => int = add(5, ...)

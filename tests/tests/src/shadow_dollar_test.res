open Mocha
open Test_utils

// Shadowed variables get a `$N` suffix in JS; it must not collide with
// identifiers that already contain `$`.

let paramCollision = (x, \"x$1") => {
  let x = \"x$1"(x)
  let y = \"x$1"(2)
  x + y
}

let laterBinding = x => {
  let x = x + 1
  let \"x$1" = x * 10
  (x, \"x$1", x, \"x$1")
}

let earlierBinding = x => {
  let \"x$1" = x * 10
  let x = x + 1
  (x, \"x$1", x, \"x$1")
}

let nested = x => {
  let inner = () => {
    let x = x + 1
    let \"x$1" = x * 10
    (x, \"x$1", x, \"x$1")
  }
  let x = x + 2
  (x, inner(), x)
}

describe(__MODULE__, () => {
  test("shadowed names do not collide with $ identifiers", () => {
    eq(__LOC__, paramCollision(1, v => v * 10), 30)
    eq(__LOC__, laterBinding(1), (2, 20, 2, 20))
    eq(__LOC__, earlierBinding(1), (2, 10, 2, 10))
    eq(__LOC__, nested(1), (3, (2, 20, 2, 20), 3))
  })
})

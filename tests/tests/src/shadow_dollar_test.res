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

// Raw JS names the program refers to must not be captured by a renamed local.
%%raw(`globalThis["y$1"] = 100; globalThis["z$2"] = 200; globalThis["w$1"] = 300`)
@val external y1: int = "y$1"
@val external z2: int = "z$2"
@val external w1: int = "w$1"

let base: int = %raw("1")

let y = base * 2
let topShadow = {
  let y = y + y1
  y + y
}

let z = base * 2
let topSibling = if z < 0 {
  let \"z$1" = z * 10
  \"z$1" + \"z$1"
} else {
  let z = z + z2
  z + z
}

let fnShadow = w => {
  let w = w + w1
  w + w
}

describe(__MODULE__, () => {
  test("shadowed names do not collide with $ identifiers", () => {
    eq(__LOC__, paramCollision(1, v => v * 10), 30)
    eq(__LOC__, laterBinding(1), (2, 20, 2, 20))
    eq(__LOC__, earlierBinding(1), (2, 10, 2, 10))
    eq(__LOC__, nested(1), (3, (2, 20, 2, 20), 3))
  })

  test("shadowed names do not capture raw JS names", () => {
    eq(__LOC__, topShadow, 204)
    eq(__LOC__, topSibling, 404)
    eq(__LOC__, fnShadow(1), 602)
  })
})

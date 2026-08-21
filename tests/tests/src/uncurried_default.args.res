// The generated JS also pins *where* optional-parameter defaults are
// computed: each default must be evaluated when its own parameter group is
// applied (x's in the outer function, z's in the inner closure below), not
// pushed into the innermost body.
module StandardNotation = {
  let withOpt = (~x=1, y) => (~z=1, w) => x + y + z + w
  let testWithOpt = withOpt(3)(4)
  let partial = withOpt(~x=10, 3)(~z=4, 11)
  let total = withOpt(~x=10, 3)(~z=4, 11)

  let foo1 = (~x=3, ~y) => x + y
  let r1 = foo1(~y=11)

  let foo2 = (~y, ~x=3, ~z=4) => x + y + z
  let r2 = foo2(~y=11)

  let foo3 = (~x=3, ~y=4) => x + y
  let r3 = foo3()
}

open StandardNotation

let withOpt = (~x=1, y) => (~z=1, w) => x + y + z + w
let testWithOpt = withOpt(3)(4)
let total = withOpt(~x=10, 3)(~z=4, 11)

let foo1 = (~x=3, ~y) => x + y
let r1 = foo1(~y=11)

let foo2 = (~y, ~x=3, ~z=4, ()) => x + y + z
let r2 = foo2(~y=11, ...)

let foo3 = (~x=3, ~y=4, ()) => x + y

module M: {
  let foo: (unit => int) => int
} = {
  let foo = func => func() + 1
}

// Scoping of defaults: a default sees only the parameters to its left —
// never a later parameter, and never a same-group shadow. `outerScope` in
// the default below is the top-level binding, not the parameter.
let outerScope = "outer"
let shadowedDefault = (~x=outerScope, outerScope: int) => (x, outerScope)

// A default may use parameters to its left.
let laterUsesEarlier = (~x=1, ~y=x + 1, ()) => x + y

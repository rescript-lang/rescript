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

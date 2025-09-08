let foo = (~x=1, ~y=2, ~z=3, w) => x + y + z + w

let bar = (~x=?, ~y, ~z=?, w) => y + w

Js.log(foo(~x=3, 4))

Js.log(bar(~y=3, 4))

let threeArgs = (~a=1, ~b=2, ~c=3, d) => a + b + c + d

Js.log(threeArgs(~a=4, ~c=7, 1))
Js.log(threeArgs(~a=4, 1))

let twoArgs = (~a=1, ~b=2, c) => a + b + c

Js.log(1 |> twoArgs)

let oneArg = (~a=1, ~z, b) => a + b

let wrapOneArg = (~a=?, n) => oneArg(~a?, ~z=33, n)

Js.log(wrapOneArg(~a=3, 44))

let fourArgs = (~a=1, ~b=2, ~c=3, ~d=4, n) => a + b + c + d + n

let wrapfourArgs = (~a=?, ~b=?, ~c=?, n) => fourArgs(~a?, ~b?, ~c?, n)

Js.log(wrapfourArgs(~a=3, ~c=44, 44))
Js.log(wrapfourArgs(~b=4, ~c=44, 44))

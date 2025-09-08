Js.log(OptArg.bar(~z=3, ~y=3, 4))

let foo = (~x=3, y) => x + y

let bar = () => foo(~x=12, 3)

Js.log(bar)

let notSuppressesOptArgs = (~x=1, ~y=2, ~z=3, w) => x + y + z + w

let _ = notSuppressesOptArgs(3)

@live
let liveSuppressesOptArgs = (~x=1, ~y=2, ~z=3, w) => x + y + z + w

let _ = liveSuppressesOptArgs(~x=3, 3)

type manyKind = | @as("a") A | @as("b") B
type otherKind = | @as("c") C
type many = {kind: manyKind, value: int}
@unboxed type t = Many(many) | Other({kind: otherKind, reason: string})

let choose: t => string
let bodyDestructure: (t, unit => unit) => string
let bodyMatch: (t, unit => unit) => string

type manyKind = | @as("a") A | @as("b") B
type otherKind = | @as("c") C
type many = {kind: manyKind, value: int}
@unboxed type t = Many(many) | Other({kind: otherKind, reason: string})

@inline(always)
let choose = value =>
  switch value {
  | Many({kind: A}) => "A"
  | Many({kind: B}) => "B"
  | Other(_) => "C"
  }

@inline(always)
let bodyDestructure = (value, effect) =>
  switch value {
  | Many(payload) =>
    effect()
    let {kind} = payload
    switch kind {
    | A => "A"
    | B => "B"
    }
  | Other(_) => "C"
  }

@inline(always)
let bodyMatch = (value, effect) =>
  switch value {
  | Many(_) =>
    effect()
    choose(value)
  | Other(_) => "C"
  }

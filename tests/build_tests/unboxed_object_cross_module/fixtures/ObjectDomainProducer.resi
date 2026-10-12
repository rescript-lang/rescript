type special = | @as("special") SpecialKind
type record<'kind> = {kind: 'kind, value: int}
@unboxed
type t =
  | Literal(record<special>)
  | Text(record<string>)
  | Number({kind: float, amount: float})

let make: unit => t
let read: t => int

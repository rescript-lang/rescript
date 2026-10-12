type special = | @as("special") SpecialKind
type record<'kind> = {kind: 'kind, value: int}
@unboxed
type t =
  | Literal(record<special>)
  | Text(record<string>)
  | Number({kind: float, amount: float})

let make = () => Text({kind: "special", value: 42})
let read = value =>
  switch value {
  | Literal({value}) => value
  | Text({value}) => -value
  | Number({amount}) => Float.toInt(amount)
  }

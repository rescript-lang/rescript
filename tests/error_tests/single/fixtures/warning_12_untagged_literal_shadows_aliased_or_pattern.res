// The "a" alternative of an aliased or-pattern can never match either.
@unboxed type t = | @as("a") A | S(string)

let f = x =>
  switch x {
  | S(("a" | "b") as s) => s
  | S(_) | A => "other"
  }

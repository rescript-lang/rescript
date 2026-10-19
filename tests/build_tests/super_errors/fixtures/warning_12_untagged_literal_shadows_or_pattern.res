// Only the "a" alternative can never match: "a" is always matched as A.
@unboxed type t = | @as("a") A | S(string)

let f = x =>
  switch x {
  | S("a" | "b") => 2
  | S(_) | A => 3
  }

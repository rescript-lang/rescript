// Only the "a" alternative can never match: "a" is always matched as A.
@unboxed type t = | @as("a") A | S(string)

let f = x =>
  switch x {
  | S("a" | "b") => 2
  | S(_) | A => 3
  }

// With warning 11 off, the dead alternative still gets warning 12, while the
// dead case gets nothing.
@warning("-11")
let g = x =>
  switch x {
  | S("a") => 1
  | S("a" | "c") => 2
  | S(_) | A => 3
  }

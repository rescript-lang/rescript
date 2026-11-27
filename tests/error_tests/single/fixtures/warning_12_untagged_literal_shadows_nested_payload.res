// An untagged payload inside another is the value itself, so the outer
// literal cases take precedence over it: 1 is always matched as One, and "x"
// as X inside W.
@unboxed type inner = | @as("x") X | N(int) | S(string)
@unboxed type outer = | @as(1) One | W(inner)

let f = v =>
  switch v {
  | One => "one"
  | W(N(1 | 2)) => "small"
  | W(S("x" | "y")) => "letter"
  | W(N(1)) => "dead"
  | W(_) => "other"
  }

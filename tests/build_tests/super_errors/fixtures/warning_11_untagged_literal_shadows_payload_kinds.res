// Literal values are compared as runtime values.

// Without @as, a constructor is represented by its name.
@unboxed type color = Primary | Color(string)
let byName = x =>
  switch x {
  | Color("Primary") => 1
  | Color(_) | Primary => 2
  }

// @as(1) and 1.0 are the same JavaScript number.
@unboxed type num = | @as(1) One | F(float)
let intAndFloat = x =>
  switch x {
  | F(1.0) => 1
  | F(_) | One => 2
  }

// A nested untagged constant has the value of its @as.
@unboxed type inner = | @as("x") P | Q(int)
@unboxed type outer = | @as("x") X | W(inner)
let nestedConstant = x =>
  switch x {
  | W(P) => 1
  | W(Q(_)) | X => 2
  }

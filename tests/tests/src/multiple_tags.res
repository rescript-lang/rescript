type unambiguous1 =
  | @as(3) A(int)
  | @as("3") B(int)

let a1 = A(10)
let b1 = B(10)

type un_ambiguous2 =
  | @as("x") A
  | @as("x") B(int)

let a2 = A
let b2 = B(10)

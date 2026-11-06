let compare = (x: int, y: int): int =>
  if x < y {
    -1
  } else if x == y {
    0
  } else {
    1
  }

let min = (x: int, y: int): int =>
  if x < y {
    x
  } else {
    y
  }

let max = (x: int, y: int): int =>
  if x > y {
    x
  } else {
    y
  }

let div = (x: int, y: int) =>
  if y == 0 {
    throw(Division_by_zero)
  } else {
    Primitive_int_extern.div(x, y)
  }

let mod_ = (x: int, y: int) =>
  if y == 0 {
    throw(Division_by_zero)
  } else {
    Primitive_int_extern.mod_(x, y)
  }

let compare = (x: bigint, y: bigint): int =>
  if x < y {
    -1
  } else if x == y {
    0
  } else {
    1
  }

let min = (x: bigint, y: bigint): bigint =>
  if x < y {
    x
  } else {
    y
  }

let max = (x: bigint, y: bigint): bigint =>
  if x > y {
    x
  } else {
    y
  }

external div: (bigint, bigint) => bigint = "%divbigint"

let div = (x: bigint, y: bigint) =>
  if y == 0n {
    throw(Division_by_zero)
  } else {
    div(x, y)
  }

external mod_: (bigint, bigint) => bigint = "%modbigint"

let mod_ = (x: bigint, y: bigint) =>
  if y == 0n {
    throw(Division_by_zero)
  } else {
    mod_(x, y)
  }

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

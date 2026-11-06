let compare = (x: float, y: float): int =>
  if x == y {
    0
  } else if x < y {
    -1
  } else if x > y {
    1
  } else if x == x {
    1
  } else if y == y {
    -1
  } else {
    0
  }

let min = (x: float, y: float): float =>
  if x < y {
    x
  } else {
    y
  }

let max = (x: float, y: float): float =>
  if x > y {
    x
  } else {
    y
  }

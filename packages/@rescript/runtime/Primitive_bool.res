let compare = (x: bool, y: bool): int =>
  switch (x, y) {
  | (true, true) | (false, false) => 0
  | (true, false) => 1
  | (false, true) => -1
  }

let min = (x: bool, y: bool): bool =>
  if x {
    y
  } else {
    x
  }

let max = (x: bool, y: bool): bool =>
  if x {
    x
  } else {
    y
  }

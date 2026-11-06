let rec sumFrom = (xs: array<int>, i: int) =>
  if i >= Array.length(xs) {
    0
  } else {
    xs[i]->Option.getOr(0) + sumFrom(xs, i + 1)
  }

let squares = (xs: array<int>) => Array.map(xs, x => x * x)

let total = (xs: array<int>) => Array.reduce(xs, 0, (acc, x) => acc + x)

let sumOfSquares = (xs: array<int>) => sumFrom(squares(xs), 0)

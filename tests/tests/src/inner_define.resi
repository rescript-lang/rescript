module N: {
  let add: (int, int) => int
}

module type S0 = {
  let f1: unit => unit
  let f2: (unit, unit) => unit
  let f3: (unit, unit, unit) => unit
}

module N0: S0

module N1: {
  let f2: (unit, unit) => unit
  let f3: (unit, unit, unit) => unit
}

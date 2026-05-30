let f = (x: (int, string)) =>
  switch x {
  | (a, b, c) => a + Int.fromString(b)->Option.getOr(0) + c
  }

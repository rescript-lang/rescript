let fn = (x: [#A | #B]) =>
  switch x {
  | #A => 1
  | #B => 2
  | ..._rest => 3
  }

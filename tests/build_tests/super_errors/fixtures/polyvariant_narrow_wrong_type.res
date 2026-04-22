let onlyB = (x: [#B]) =>
  switch x {
  | #B => "b"
  }

let fn = (x: [#A | #B | #C]) =>
  switch x {
  | #A => "a"
  | ...rest => onlyB(rest)
  }

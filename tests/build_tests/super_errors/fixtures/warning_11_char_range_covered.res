// The last range is covered by the two before it together.
let f = c =>
  switch c {
  | 'a' .. 'm' => 1
  | 'n' .. 'z' => 2
  | 'c' .. 'x' => 3
  | _ => 4
  }

// All char literals are covered, so the example is an int beyond them.
let f = c =>
  switch c {
  | '\u{0}' .. '\u{10FFFF}' => 1
  }

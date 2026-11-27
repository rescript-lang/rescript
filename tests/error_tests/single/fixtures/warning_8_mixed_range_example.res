// A range from a char to an int covers all non-negative ints, so the
// example is a negative int, not a char.
let f = x =>
  switch x {
  | '\x00' .. 0x7FFFFFFF => 1
  }

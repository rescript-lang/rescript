// All non-negative ints are covered, so the example is negative.
let f = x =>
  switch x {
  | 0 .. 0x7FFFFFFF => true
  | -10 .. -1 => false
  }

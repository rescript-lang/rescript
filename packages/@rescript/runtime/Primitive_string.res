let compare = (s1: string, s2: string): int =>
  if s1 == s2 {
    0
  } else if s1 < s2 {
    -1
  } else {
    1
  }

let min = (x: string, y: string): string =>
  if x < y {
    x
  } else {
    y
  }

let max = (x: string, y: string): string =>
  if x > y {
    x
  } else {
    y
  }

let getChar = (s, i) =>
  if i >= Primitive_string_extern.length(s) || i < 0 {
    throw(Invalid_argument("index out of bounds"))
  } else {
    Primitive_string_extern.getChar(s, i)
  }

// TODO: delete
let make = (n, ch: char): string =>
  Primitive_string_extern.fromChar(ch)->Primitive_string_extern.repeat(n)

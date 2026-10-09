type a = One | Two(int)
type b = | ...a | Three
type c = | ...b | Four
type one = Only
type d = | ...one | Other

// Constructors of a spread that earlier cases already match aren't reported
let f = (x: c) =>
  switch x {
  | Two(_) | Three => 0
  | ...b => 1
  | Four => 2
  }

// A spread that is unused as a whole is reported at the spread
let g = (x: b) =>
  switch x {
  | One | Two(_) => 0
  | ...a | Three => 1
  }

let h = (x: d) =>
  switch x {
  | Only => 0
  | ...one | Other => 1
  }

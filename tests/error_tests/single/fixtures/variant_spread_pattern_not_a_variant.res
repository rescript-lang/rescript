type a = {x: int}
type b = One | Two | Three

let f = (b: b) =>
  switch b {
  | ...a as v => v
  | Three => One
  }

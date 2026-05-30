type a<'x> = One | Two('x)
type b = One | Two(int) | Three

let f = (b: b) =>
  switch b {
  | ...a as x => x
  | Three => One
  }

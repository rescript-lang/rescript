type a = A | B

// b spreads a and adds one more constructor
type b =
  | ...a
  | C

let upcast = (x: a): b => (x :> b)

let _ = upcast(A)

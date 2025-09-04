// Repro for incorrect "constructor ... is never used" warning with coercions
// This should compile cleanly without warnings when coercing from a -> b.

type a = A | B

type b =
  | ...a
  | C

let upcast = (x: a): b => (x :> b)

let _ = upcast(A)

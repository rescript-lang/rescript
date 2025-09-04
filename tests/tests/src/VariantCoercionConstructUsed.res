// Repro for incorrect "constructor ... is never used" warning with coercions
// This should compile cleanly without warnings when coercing from a -> b.

type a = A | B
module DoNotWarn: {
  let log: a => unit
} = {
  type b =
    | ...a
    | C

  let log = (x: a) => Js.log((x :> b))
}

let _ = DoNotWarn.log(A)

// Ensure unused-constructor warning still fires for new members introduced
// only on the target side of a coercion.

type a = A | B

module M: {
  let log: a => unit
} = {
  type b =
    | ...a
    | C

  let log = (x: a) => Js.log((x :> b))
}

let _ = M.log(A)

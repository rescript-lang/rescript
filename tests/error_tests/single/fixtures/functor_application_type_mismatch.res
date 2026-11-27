module type S = {
  type t
}

module F = (X: S) => {
  type t = X.t
  let make = (x: X.t): X.t => x
}

module IntS = {
  type t = int
}

module App = F(IntS)

// Type error on a value whose type lives behind a functor-application path.
// Exercises functor application (Path.Papply) and type-path error printing,
// confirming neither hits the assert false left in Ctype.lid_of_path after
// removing the unreachable Longident.Lapply constructor.
let bad: App.t = "not an int"

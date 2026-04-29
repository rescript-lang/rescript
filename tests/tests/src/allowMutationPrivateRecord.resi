@allowMutation
type t = private {
  mutable value: int,
  name: string,
}

let make: int => t
let value: t => int
let name: t => string

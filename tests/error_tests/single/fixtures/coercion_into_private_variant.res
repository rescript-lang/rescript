module Color: {
  type t = private Red | Green
} = {
  type t = Red | Green
}

type plain = Red | Green
let forged = ((Red: plain) :> Color.t)

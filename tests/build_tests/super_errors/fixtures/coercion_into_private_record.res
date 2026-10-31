module Blob: {
  type t = private {size: float}
  let size: t => float
} = {
  type t = {size: float}
  let size = b => b.size
}

type plain = {size: float}

// A plain record coerced into a private one: constructs a Blob.t from outside
let fake = (p: plain): Blob.t => (p :> Blob.t)
let s = Blob.size(fake({size: 1.}))

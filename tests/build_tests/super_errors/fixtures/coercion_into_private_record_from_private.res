module Blob: {
  type t = private {size: float}
} = {
  type t = {size: float}
}

// Another private record with the same fields, constructed inside its own module
module Fake: {
  type t = private {size: float}
  let make: float => t
} = {
  type t = {size: float}
  let make = size => {size: size}
}

let forged = (Fake.make(1.) :> Blob.t)

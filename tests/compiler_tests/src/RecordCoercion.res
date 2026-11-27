type r1 = {a: option<int>, b: int}

type r2 = {a?: int, b: int}

type r3 = {a?: int}

type r4 = {}

// let _ = (x: r1) => (x :> r2) // Convert a from mandatory to optional NOT ALLOWED
// let _ = (x: r2) => (x :> r1) can't turn an optional field to a mandatory one
let _ = (x: r2) => (x :> r3) // can omit field
// let _ = (x: r1) => (x :> r3) // omit field and convert from mandatory to optional NOT ALLOWED
let _ = (x: r3) => (x :> r4) // omit everything

type nested1 = {n: r2, extra: int}
type nested2 = {n: r3}
let _ = (x: nested1) => (x :> nested2)

module TestInlining = {
  type a = {
    number: int,
    name: string,
  }

  type b = {name: string}

  let a: a = {
    number: 42,
    name: "a",
  }

  let name = (a :> b).name
}

// A private record coerces into a plain one, and into itself, but nothing
// coerces into it from another type
module PrivateBlob: {
  type t = private {size: float}
  let make: float => t
} = {
  type t = {size: float}
  let make = size => {size: size}
}

type plainBlob = {size: float}

let blobToPlain = (b: PrivateBlob.t): plainBlob => (b :> plainBlob)
let blobToItself = (b: PrivateBlob.t) => (b :> PrivateBlob.t)
let blobSize = blobToPlain(PrivateBlob.make(2.)).size

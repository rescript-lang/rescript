open Mocha
open Test_utils

@deriving({jsConverter: newType})
type t<'a> = {
  x: int,
  y: bool,
  z: 'a,
}

let v0 = tToJs({x: 3, y: false, z: false})
let v1 = tToJs({x: 3, y: false, z: ""})

@deriving({jsConverter: newType})
type x = [
  | #a
  | #b
  | #c
]

let idx = v => eq(__LOC__, xFromJs(xToJs(v)), v)
let x0 = xToJs(#a)
let x1 = xToJs(#b)

describe(__MODULE__, () => {
  test("jsConverter roundtrip for #a", () => idx(#a))
  test("jsConverter roundtrip for #b", () => idx(#b))
  test("jsConverter roundtrip for #c", () => idx(#c))
})

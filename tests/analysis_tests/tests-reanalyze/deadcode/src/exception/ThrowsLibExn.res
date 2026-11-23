exception E

@throws(E)
external parse: string => int = "parseThrowing"

@throws(E)
let viaLet = x => x > 0 ? x : throw(E)

module Inner = {
  exception F

  @throws(F)
  external find: int => int = "findThrowing"
}

module Outer = {
  @throws(E)
  external findOuter: int => int = "findOuterThrowing"
}

module type S = {
  @throws(E)
  let get: int => int
}

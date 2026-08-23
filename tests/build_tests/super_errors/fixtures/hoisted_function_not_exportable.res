let run = () => {
  @res.hoistedFunction
  let local = () => ()
  local()
}

let localModule = () => {
  module Local = {
    @res.hoistedFunction
    let make = () => ()
  }
  Local.make()
}
module Make = () => {
  @res.hoistedFunction
  let make = () => ()
}
%%private(
  @res.hoistedFunction
  let privateMake = () => ()
)

let usePrivate = privateMake()
module Shadowed = {
  @res.hoistedFunction
  let make = () => "first"
  let make = () => "second"
}
@res.hoistedFunction
type markedType = int
@res.hoistedFunction
module type MarkedModule = {}
@res.hoistedFunction
module MarkedModule = {}
module type MarkedSignature = {
  @res.hoistedFunction
  let make: unit => unit
}
let after = ()

// A collection of functions to make code cleaner.
include Async

// From here: https://dev.to/johnridesabike/optional-chaining-in-reason-1im6
let oMap = Belt.Option.map
let \"<$>" = oMap
let oFlatMap = Belt.Option.flatMap
let \">>=" = oFlatMap
let mapd = Option.mapWithDefault
let \"||||" = Option.getWithDefault

let \"|+|" = (a, b) => a->BN.add(b)
let \"|*|" = (a, b) => a->BN.mul(b)
let \"|-|" = (a, b) => a->BN.sub(b)
let \"|/|" = (a, b) => a->BN.div(b)
let \"|==|" = (a, b) => a->BN.eq(b)
let \"|<|" = (a, b) => a->BN.lt(b)
let \"|>|" = (a, b) => a->BN.gt(b)

// Float
let toFixedWithPrecisionNoTrailingZeros = (number: float, ~digits) =>
  number->Js.Float.toFixedWithPrecision(~digits)->float_of_string->Float.toString

// React components
let restr = React.string
let reactMapWithDefault: (option<'a>, React.element, 'a => React.element) => React.element = (
  opt,
  default,
  f,
) =>
  switch opt {
  | None => default
  | Some(item) => f(item)
  }
let reactMap = (opt, f) => reactMapWithDefault(opt, React.null, f)

// For use with: https://github.com/reasonml-labs/bs-let
module Opt = {
  let let_ = Belt.Option.flatMap
}

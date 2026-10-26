module Target = {
  type t = {value: int}
  type options = {step: int}
  exception Failed(string)
  let make = (value, ~start=0) => {value: value + start}
  let fromOptions = (value, ~options={step: 1}) => {value: value * options.step}
  let delay = f => {value: f()}
}

type t = Target.t

exception Failed = Target.Failed

let makeFromZero = value => Target.make(value, ~start=0)
let fromOptionsPositional = (value, options) => Target.fromOptions(value, ~options)
let ofValue = value => Target.delay(() => value)
let add = (a, b) => a + b

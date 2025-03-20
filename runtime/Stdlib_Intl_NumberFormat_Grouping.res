type t

type parsed = [#bool(bool) | #always | #auto | #min2]

external fromBool: bool => t = "%identity"
external fromString: [#always | #auto | #min2] => t = "%identity"

let parseJsValue = value =>
  switch Stdlib_Type.Classify.classify(value) {
  | String("always") => Some(#always)
  | String("auto") => Some(#auto)
  | String("min2") => Some(#min2)
  | Bool(value) => Some(#bool(value))
  | _ => None
  }

/**
  `ignore(grouping)` ignores the provided grouping and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

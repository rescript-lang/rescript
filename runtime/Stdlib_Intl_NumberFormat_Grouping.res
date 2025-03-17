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

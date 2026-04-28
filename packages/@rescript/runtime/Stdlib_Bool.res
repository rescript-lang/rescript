type t = bool

let toString = b =>
  if b {
    "true"
  } else {
    "false"
  }

let fromString = s => {
  switch s {
  | "true" => Some(true)
  | "false" => Some(false)
  | _ => None
  }
}

let fromStringOrThrow = param =>
  switch param {
  | "true" => true
  | "false" => false
  | _ => throw(Invalid_argument(`Bool.fromStringOrThrow: value is neither "true" nor "false"`))
  }

@deprecated({
  reason: "Use `fromStringOrThrow` instead",
  migrate: Bool.fromStringOrThrow(),
})
let fromStringExn = fromStringOrThrow

external compare: (bool, bool) => Stdlib_Ordering.t = "%compare"

external equal: (bool, bool) => bool = "%equal"

external ignore: bool => unit = "%ignore"

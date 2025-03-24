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

let fromStringExn = param =>
  switch param {
  | "true" => true
  | "false" => false
  | _ => throw(Invalid_argument(`Bool.fromStringExn: value is neither "true" nor "false"`))
  }

external compare: (bool, bool) => Stdlib_Ordering.t = "%compare"

external equal: (bool, bool) => bool = "%equal"

type t = [#InvalidKey | #InvalidLengthValue | #InvalidValue]

let notification = error =>
  switch error {
  | #InvalidKey => ("InvalidKey", "")
  | #InvalidValue => ("InvalidValue", "")
  | #InvalidLengthValue => ("InvalidLengthValue", "")
  }

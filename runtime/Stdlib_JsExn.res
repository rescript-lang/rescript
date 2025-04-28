type t = unknown

let fromException: exn => option<t> = exn =>
  switch exn {
  | JsExn(t) => Some(t)
  | _ => None
  }

external anyToExnInternal: 'a => exn = "%wrap_exn"

let getOrUndefined: string => t => option<
  string,
> = %raw(`fieldName => t => (t && typeof t[fieldName] === "string" ? t[fieldName] : undefined)`)

let stack = getOrUndefined("stack")
let message = getOrUndefined("message")
let name = getOrUndefined("name")
let fileName = getOrUndefined("fileName")

external throw: 'a => 'b = "%raise"

external ignore: t => unit = "%ignore"

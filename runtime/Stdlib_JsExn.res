type t = unknown

let fromException: exn => option<t> = exn =>
  switch exn {
  | JsExn(t) => Some(t)
  | _ => None
  }

external anyToExnInternal: 'a => exn = "%wrap_exn"

@get external stack: t => option<string> = "stack"
@get external message: t => option<string> = "message"
@get external name: t => option<string> = "name"
@get external fileName: t => option<string> = "fileName"

external throw: 'a => 'b = "%raise"

external ignore: t => unit = "%ignore"

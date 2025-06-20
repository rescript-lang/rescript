@notUndefined
type t

@get external stack: t => option<string> = "stack"
@get external message: t => string = "message"
@get external name: t => string = "name"
@get external fileName: t => option<string> = "fileName"

@new external make: string => t = "Error"

external throw: t => 'a = "%raise"

let throwWithMessage = str => {
  let error = make(str)
  throw(error)
}

module EvalError = {
  @new external make: string => t = "EvalError"
  let throwWithMessage = s => make(s)->throw
}

module RangeError = {
  @new external make: string => t = "RangeError"
  let throwWithMessage = s => make(s)->throw
}

module ReferenceError = {
  @new external make: string => t = "ReferenceError"
  let throwWithMessage = s => make(s)->throw
}

module SyntaxError = {
  @new external make: string => t = "SyntaxError"
  let throwWithMessage = s => make(s)->throw
}

module TypeError = {
  @new external make: string => t = "TypeError"
  let throwWithMessage = s => make(s)->throw
}

module URIError = {
  @new external make: string => t = "URIError"
  let throwWithMessage = s => make(s)->throw
}

let panic = msg => make(`Panic! ${msg}`)->throw

external toJsExn: t => Stdlib_JsExn.t = "%identity"

external ignore: t => unit = "%ignore"

// Use deprecated Error APIs to validate migration to JsError/JsExn

external someExn: exn = "whatever"

let fromExn1 = someExn->Error.fromException
let fromExn2 = Error.fromException(someExn)

let err = Error.make("Some message here")

let stack1 = err->Error.stack
let stack2 = Error.stack(err)

let message1 = err->Error.message
let message2 = Error.message(err)

let name1 = err->Error.name
let name2 = Error.name(err)

let fileName1 = err->Error.fileName
let fileName2 = Error.fileName(err)

// Type alias migration
let errT: Error.t = Error.make("Another message")

// Sub-error constructors
let evalErr = Error.EvalError.make("eval error")
let rangeErr = Error.RangeError.make("range error")
let refErr = Error.ReferenceError.make("reference error")
let synErr = Error.SyntaxError.make("syntax error")
let typeErr = Error.TypeError.make("type error")
let uriErr = Error.URIError.make("uri error")

let ignore1 = err->Error.ignore
let ignore2 = Error.ignore(err)

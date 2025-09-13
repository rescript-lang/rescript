// Use deprecated Exn APIs to validate migration to JsExn/JsError

external someExn: exn = "whatever"
external someJsExn: Exn.t = "whatever"

// fromException (asJsExn)
let fromExn1 = someExn->Exn.asJsExn
let fromExn2 = Exn.asJsExn(someExn)

// Property accessors on Exn.t
let stack1 = someJsExn->Exn.stack
let stack2 = Exn.stack(someJsExn)

let message1 = someJsExn->Exn.message
let message2 = Exn.message(someJsExn)

let name1 = someJsExn->Exn.name
let name2 = Exn.name(someJsExn)

let fileName1 = someJsExn->Exn.fileName
let fileName2 = Exn.fileName(someJsExn)

// Type alias migration
let exnT: Exn.t = someJsExn

// anyToExnInternal
let _coerced = Exn.anyToExnInternal(1)

// ignore
let ignore1 = someJsExn->Exn.ignore
let ignore2 = Exn.ignore(someJsExn)

// Raise helpers
let throws1 = () => Exn.raiseError("err")
let throws2 = () => Exn.raiseEvalError("err")
let throws3 = () => Exn.raiseRangeError("err")
let throws4 = () => Exn.raiseReferenceError("err")
let throws5 = () => Exn.raiseSyntaxError("err")
let throws6 = () => Exn.raiseTypeError("err")
let throws7 = () => Exn.raiseUriError("err")

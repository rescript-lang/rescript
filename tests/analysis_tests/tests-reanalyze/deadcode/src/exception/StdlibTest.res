@throws(JsExn)
let optionGetOrThrow = o => o->Option.getOrThrow

@throws(JsExn)
let resultGetOrThrow = r => r->Result.getOrThrow

@throws(Invalid_argument)
let nullGetOrThrow = n => n->Null.getOrThrow

@throws(JsExn)
let bigIntFromStringExn = s => s->BigInt.fromStringOrThrow

@throws(JsExn)
let jsonParseExn = s => s->JSON.parseOrThrow

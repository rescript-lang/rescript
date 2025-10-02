@throws(JsExn)
let optionGetExn = o => o->Option.getExn

@throws(Not_found)
let resultGetExn = r => r->Result.getExn

@throws(Invalid_argument)
let nullGetExn = n => n->Null.getExn

@throws(JsExn)
let bigIntFromStringExn = s => s->BigInt.fromStringOrThrow

@throws(JsExn)
let jsonParseExn = s => s->JSON.parseOrThrow

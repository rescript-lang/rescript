@raises(JsExn)
let optionGetExn = o => o->Option.getExn

@raises(Not_found)
let resultGetExn = r => r->Result.getExn

@raises(Invalid_argument)
let nullGetExn = n => n->Null.getExn

@raises(JsExn)
let bigIntFromStringExn = s => s->BigInt.fromStringOrThrow

@raises(JsExn)
let jsonParseExn = s => s->JSON.parseOrThrow

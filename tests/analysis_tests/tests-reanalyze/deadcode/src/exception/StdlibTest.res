@raises(Exn.Error)
let optionGetExn = o => o->Option.getExn

@raises(Not_found)
let resultGetExn = r => r->Result.getExn

@raises(Invalid_argument)
let nullGetExn = n => n->Null.getExn

@raises(Exn.Error)
let bigIntFromStringExn = s => s->BigInt.fromStringExn

@raises(Exn.Error)
let jsonParseExn = s => s->JSON.parseExn

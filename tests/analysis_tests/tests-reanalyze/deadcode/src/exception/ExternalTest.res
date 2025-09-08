@raises(JsExn)
external bigIntFromStringExn: string => bigint = "BigInt"

@raises(JsExn)
let bigIntFromStringExn = s => s->bigIntFromStringExn

let bigIntFromStringExn2 = s => s->bigIntFromStringExn

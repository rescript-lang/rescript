@throws(JsExn)
external bigIntFromStringExn: string => bigint = "BigInt"

@throws(JsExn)
let bigIntFromStringExn = s => s->bigIntFromStringExn

let bigIntFromStringExn2 = s => s->bigIntFromStringExn

@raises(Exn.Error)
external bigIntFromStringExn: string => bigint = "BigInt"

@raises(Exn.Error)
let bigIntFromStringExn = s => s->bigIntFromStringExn

let bigIntFromStringExn2 = s => s->bigIntFromStringExn

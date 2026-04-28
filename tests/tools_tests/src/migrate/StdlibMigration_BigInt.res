let fromStringExn1 = "123"->Js.BigInt.fromStringExn
let fromStringExn2 = Js.BigInt.fromStringExn("123")

let land1 = 7n->Js.BigInt.land(4n)
let land2 = Js.BigInt.land(7n, 4n)
let land3 = 7n->Js.BigInt.toString->Js.BigInt.fromStringExn->Js.BigInt.land(4n)

let lor1 = 7n->Js.BigInt.lor(4n)
let lor2 = Js.BigInt.lor(7n, 4n)

let lxor1 = 7n->Js.BigInt.lxor(4n)
let lxor2 = Js.BigInt.lxor(7n, 4n)

let lnot1 = 2n->Js.BigInt.lnot
let lnot2 = Js.BigInt.lnot(2n)

let lsl1 = 4n->Js.BigInt.lsl(1n)
let lsl2 = Js.BigInt.lsl(4n, 1n)

let asr1 = 8n->Js.BigInt.asr(1n)
let asr2 = Js.BigInt.asr(8n, 1n)

let toString1 = 123n->Js.BigInt.toString
let toString2 = Js.BigInt.toString(123n)

let toLocaleString1 = 123n->Js.BigInt.toLocaleString
let toLocaleString2 = Js.BigInt.toLocaleString(123n)

// From the stdlib module
let stdlib_fromStringExn1 = "123"->BigInt.fromStringExn
let stdlib_fromStringExn2 = BigInt.fromStringExn("123")

let stdlib_land1 = 7n->BigInt.land(4n)
let stdlib_land2 = BigInt.land(7n, 4n)

let stdlib_lor1 = BigInt.lor(7n, 4n)

let stdlib_lxor1 = 7n->BigInt.lxor(4n)
let stdlib_lxor2 = BigInt.lxor(7n, 4n)

let stdlib_lnot1 = 2n->BigInt.lnot
let stdlib_lnot2 = BigInt.lnot(2n)

let stdlib_lsl1 = 4n->BigInt.lsl(1n)
let stdlib_lsl2 = BigInt.lsl(4n, 1n)

let stdlib_asr1 = 8n->BigInt.asr(1n)
let stdlib_asr2 = BigInt.asr(8n, 1n)

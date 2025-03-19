type t = bigint

@val
external asIntN: (~width: int, bigint) => bigint = "BigInt.asIntN"
@val external asUintN: (~width: int, bigint) => bigint = "BigInt.asUintN"

@val
external fromString: string => bigint = "BigInt"

@val @raises(Exn.Error)
external fromStringExn: string => bigint = "BigInt"
@val external fromInt: int => bigint = "BigInt"
@val external fromFloat: float => bigint = "BigInt"

@send
external toString: (bigint, ~radix: int=?) => string = "toString"

@deprecated("Use `toString` with `~radix` instead") @send
external toStringWithRadix: (bigint, ~radix: int) => string = "toString"

@send
external toLocaleString: bigint => string = "toLocaleString"

@val external toFloat: bigint => float = "Number"

let toInt = t => t->toFloat->Stdlib_Int.fromFloat

external \"+": (bigint, bigint) => bigint = "%addbigint"
external \"-": (bigint, bigint) => bigint = "%subbigint"
external \"*": (bigint, bigint) => bigint = "%mulbigint"
external \"/": (bigint, bigint) => bigint = "%divbigint"
external \"~-": bigint => bigint = "%negbigint"
external \"~+": bigint => bigint = "%identity"
external \"**": (bigint, bigint) => bigint = "%powbigint"

external add: (bigint, bigint) => bigint = "%addfloat"
external sub: (bigint, bigint) => bigint = "%subfloat"
external mul: (bigint, bigint) => bigint = "%mulfloat"
external div: (bigint, bigint) => bigint = "%divfloat"

external mod: (bigint, bigint) => bigint = "%modbigint"

external land: (bigint, bigint) => bigint = "%andbigint"
external lor: (bigint, bigint) => bigint = "%orbigint"
external lxor: (bigint, bigint) => bigint = "%xorbigint"

external lsl: (bigint, bigint) => bigint = "%lslbigint"
external asr: (bigint, bigint) => bigint = "%asrbigint"

let lnot = x => lxor(x, -1n)

/**
  `ignore(bigint)` ignores the provided bigint and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: bigint => unit = "%ignore"

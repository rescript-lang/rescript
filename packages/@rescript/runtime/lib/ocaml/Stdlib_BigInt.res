type t = bigint

@val external asIntN: (~width: int, bigint) => bigint = "BigInt.asIntN"
@val external asUintN: (~width: int, bigint) => bigint = "BigInt.asUintN"

@val
external fromStringOrThrow: string => bigint = "BigInt"

let fromString = (value: string) => {
  try Some(fromStringOrThrow(value)) catch {
  | _ => None
  }
}

@val external fromInt: int => bigint = "BigInt"

@val
external fromFloatOrThrow: float => bigint = "BigInt"

let fromFloat = (value: float) => {
  try Some(fromFloatOrThrow(value)) catch {
  | _ => None
  }
}

@send
external toString: (bigint, ~radix: int=?) => string = "toString"

@send
external toLocaleString: bigint => string = "toLocaleString"

@val external toFloat: bigint => float = "Number"

let toInt = t => t->toFloat->Stdlib_Int.fromFloat

external add: (bigint, bigint) => bigint = "%addbigint"
external sub: (bigint, bigint) => bigint = "%subbigint"
external mul: (bigint, bigint) => bigint = "%mulbigint"
external div: (bigint, bigint) => bigint = "%divbigint"

external mod: (bigint, bigint) => bigint = "%modbigint"

external bitwiseAnd: (bigint, bigint) => bigint = "%andbigint"
external bitwiseOr: (bigint, bigint) => bigint = "%orbigint"
external bitwiseXor: (bigint, bigint) => bigint = "%xorbigint"
external bitwiseNot: bigint => bigint = "%bitnot_bigint"

external shiftLeft: (bigint, bigint) => bigint = "%lslbigint"
external shiftRight: (bigint, bigint) => bigint = "%asrbigint"

external ignore: bigint => unit = "%ignore"

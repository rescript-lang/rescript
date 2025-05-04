/**
Type representing a bigint.
*/
type t = bigint

@val external asIntN: (~width: int, bigint) => bigint = "BigInt.asIntN"
@val external asUintN: (~width: int, bigint) => bigint = "BigInt.asUintN"

@val external fromString: string => bigint = "BigInt"

@val
/**
Parses the given `string` into a `bigint` using JavaScript semantics. Return the
number as a `bigint` if successfully parsed. Uncaught syntax exception otherwise.

## Examples

```rescript
BigInt.fromStringExn("123")->assertEqual(123n)

BigInt.fromStringExn("")->assertEqual(0n)

BigInt.fromStringExn("0x11")->assertEqual(17n)

BigInt.fromStringExn("0b11")->assertEqual(3n)

BigInt.fromStringExn("0o11")->assertEqual(9n)

/* catch exception */
switch BigInt.fromStringExn("a") {
| exception JsExn(_error) => assert(true)
| _bigInt => assert(false)
}
```
*/
external fromStringExn: string => bigint = "BigInt"
@val external fromInt: int => bigint = "BigInt"
@val external fromFloat: float => bigint = "BigInt"

let fromFloat = (value: float) => {
  try Some(fromFloat(value)) catch {
  | _ => None
  }
}

@send
/**
Formats a `bigint` as a string. Return a `string` representing the given value.
See [`toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) on MDN.

## Examples

```rescript
BigInt.toString(123n)->assertEqual("123")
```
*/
external toString: (bigint, ~radix: int=?) => string = "toString"

@deprecated("Use `toString` with `~radix` instead") @send
external toStringWithRadix: (bigint, ~radix: int) => string = "toString"

@send
/**
Returns a string with a language-sensitive representation of this BigInt value.

## Examples

```rescript
BigInt.toString(123n)->assertEqual("123")
```
*/
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

/**
  `ignore(bigint)` ignores the provided bigint and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: bigint => unit = "%ignore"

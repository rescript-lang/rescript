/**
Type representing a bigint.
*/
type t = bigint

@val external asIntN: (~width: int, bigint) => bigint = "BigInt.asIntN"
@val external asUintN: (~width: int, bigint) => bigint = "BigInt.asUintN"

@val
/**
Parses the given `string` into a `bigint` using JavaScript semantics. Return the
number as a `bigint` if successfully parsed. Throws a syntax exception otherwise.

## Examples

```rescript
BigInt.fromStringOrThrow("123")->assertEqual(123n)

BigInt.fromStringOrThrow("")->assertEqual(0n)

BigInt.fromStringOrThrow("0x11")->assertEqual(17n)

BigInt.fromStringOrThrow("0b11")->assertEqual(3n)

BigInt.fromStringOrThrow("0o11")->assertEqual(9n)

/* catch exception */
switch BigInt.fromStringOrThrow("a") {
| exception JsExn(_error) => assert(true)
| _bigInt => assert(false)
}
```
*/
external fromStringOrThrow: string => bigint = "BigInt"

/**
Parses the given `string` into a `bigint` using JavaScript semantics. Returns 
`Some(bigint)` if the string can be parsed, `None` otherwise.

## Examples

```rescript
BigInt.fromString("123")->assertEqual(Some(123n))

BigInt.fromString("")->assertEqual(Some(0n))

BigInt.fromString("0x11")->assertEqual(Some(17n))

BigInt.fromString("0b11")->assertEqual(Some(3n))

BigInt.fromString("0o11")->assertEqual(Some(9n))

BigInt.fromString("invalid")->assertEqual(None)
```
*/
let fromString = (value: string) => {
  try Some(fromStringOrThrow(value)) catch {
  | _ => None
  }
}

@deprecated("Use `fromStringOrThrow` instead") @val
external fromStringExn: string => bigint = "BigInt"

@val external fromInt: int => bigint = "BigInt"

@val
/**
Converts a `float` to a `bigint` using JavaScript semantics. 
Throws an exception if the float is not an integer or is infinite/NaN.

## Examples

```rescript
BigInt.fromFloatOrThrow(123.0)->assertEqual(123n)

BigInt.fromFloatOrThrow(0.0)->assertEqual(0n)

BigInt.fromFloatOrThrow(-456.0)->assertEqual(-456n)

/* This will throw an exception */
switch BigInt.fromFloatOrThrow(123.5) {
| exception JsExn(_error) => assert(true)
| _bigInt => assert(false)
}
```
*/
external fromFloatOrThrow: float => bigint = "BigInt"

let fromFloat = (value: float) => {
  try Some(fromFloatOrThrow(value)) catch {
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

@deprecated("Use `&` operator or `bitwiseAnd` instead.")
external land: (bigint, bigint) => bigint = "%andbigint"

@deprecated("Use `bitwiseOr` instead.")
external lor: (bigint, bigint) => bigint = "%orbigint"

@deprecated("Use `^` operator or `bitwiseXor` instead.")
external lxor: (bigint, bigint) => bigint = "%xorbigint"

@deprecated("Use `~` operator or `bitwiseNot` instead.")
external lnot: bigint => bigint = "%bitnot_bigint"

@deprecated("Use `<<` operator or `shiftLeft` instead.")
external lsl: (bigint, bigint) => bigint = "%lslbigint"

@deprecated("Use `>>` operator or `shiftRight` instead.")
external asr: (bigint, bigint) => bigint = "%asrbigint"

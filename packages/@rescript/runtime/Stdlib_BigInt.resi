/**
Type representing a bigint.
*/
type t = bigint

/**
`asIntN(~width, bigint)` returns a bigint value truncated to the given number of bits as a signed integer.

See [`BigInt.asIntN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt/asIntN) on MDN.

## Examples

```rescript
BigInt.asIntN(~width=4, 25n) == -7n
BigInt.asIntN(~width=4, 3n) == 3n
```
*/
@val external asIntN: (~width: int, bigint) => bigint = "BigInt.asIntN"

/**
`asUintN(~width, bigint)` returns a bigint value truncated to the given number of bits as an unsigned integer.

See [`BigInt.asUintN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt/asUintN) on MDN.

## Examples

```rescript
BigInt.asUintN(~width=4, 25n) == 9n
BigInt.asUintN(~width=4, 3n) == 3n
```
*/
@val external asUintN: (~width: int, bigint) => bigint = "BigInt.asUintN"

/**
Parses the given `string` into a `bigint` using JavaScript semantics. Return the
number as a `bigint` if successfully parsed. Throws a syntax exception otherwise.

## Examples

```rescript
BigInt.fromStringOrThrow("123") == 123n

BigInt.fromStringOrThrow("") == 0n

BigInt.fromStringOrThrow("0x11") == 17n

BigInt.fromStringOrThrow("0b11") == 3n

BigInt.fromStringOrThrow("0o11") == 9n

/* catch exception */
switch BigInt.fromStringOrThrow("a") {
| exception JsExn(_error) => assert(true)
| _bigInt => assert(false)
}
```
*/
@val
external fromStringOrThrow: string => bigint = "BigInt"

/**
Parses the given `string` into a `bigint` using JavaScript semantics. Returns
`Some(bigint)` if the string can be parsed, `None` otherwise.

## Examples

```rescript
BigInt.fromString("123") == Some(123n)

BigInt.fromString("") == Some(0n)

BigInt.fromString("0x11") == Some(17n)

BigInt.fromString("0b11") == Some(3n)

BigInt.fromString("0o11") == Some(9n)

BigInt.fromString("invalid") == None
```
*/
let fromString: string => option<bigint>

@deprecated({
  reason: "Use `fromStringOrThrow` instead",
  migrate: BigInt.fromStringOrThrow(),
})
@val
external fromStringExn: string => bigint = "BigInt"

/**
`fromInt(int)` converts an `int` to a `bigint`.

## Examples

```rescript
BigInt.fromInt(123) == 123n
BigInt.fromInt(0) == 0n
BigInt.fromInt(-456) == -456n
```
*/
@val external fromInt: int => bigint = "BigInt"

/**
Converts a `float` to a `bigint` using JavaScript semantics.
Throws an exception if the float is not an integer or is infinite/NaN.

## Examples

```rescript
BigInt.fromFloatOrThrow(123.0) == 123n

BigInt.fromFloatOrThrow(0.0) == 0n

BigInt.fromFloatOrThrow(-456.0) == -456n

/* This will throw an exception */
switch BigInt.fromFloatOrThrow(123.5) {
| exception JsExn(_error) => assert(true)
| _bigInt => assert(false)
}
```
*/
@val
external fromFloatOrThrow: float => bigint = "BigInt"

/**
`fromFloat(float)` converts a `float` to a `bigint` using JavaScript semantics.
Returns `Some(bigint)` if the float is a valid `bigint`, `None` otherwise.

## Examples

```rescript
BigInt.fromFloat(123.0) == Some(123n)
BigInt.fromFloat(0.0) == Some(0n)
BigInt.fromFloat(-456.0) == Some(-456n)
BigInt.fromFloat(123.5) == None
```
*/
let fromFloat: float => option<bigint>

/**
Formats a `bigint` as a string. Return a `string` representing the given value.
See [`toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) on MDN.

## Examples

```rescript
BigInt.toString(123n) == "123"
```
*/
@send
external toString: (bigint, ~radix: int=?) => string = "toString"

@deprecated({
  reason: "Use `toString` with `~radix` instead",
  migrate: BigInt.toString(),
})
@send
external toStringWithRadix: (bigint, ~radix: int) => string = "toString"

/**
Returns a string with a language-sensitive representation of this BigInt value.

## Examples

```rescript
BigInt.toString(123n) == "123"
```
*/
@send
external toLocaleString: bigint => string = "toLocaleString"

/**
`toFloat(bigint)` converts a `bigint` to a `float`.

## Examples

```rescript
BigInt.toFloat(123n) == 123.0
BigInt.toFloat(0n) == 0.0
BigInt.toFloat(-456n) == -456.0
```
*/
@val external toFloat: bigint => float = "Number"

/**
`toInt(bigint)` converts a `bigint` to an `int`.

## Examples

```rescript
BigInt.toInt(123n) == 123
BigInt.toInt(0n) == 0
BigInt.toInt(-456n) == -456
```
*/
let toInt: bigint => int

/**
`add(a, b)` calculates the sum of two bigints.

## Examples

```rescript
BigInt.add(5n, 3n) == 8n
BigInt.add(-2n, 7n) == 5n
```
*/
external add: (bigint, bigint) => bigint = "%addbigint"

/**
`sub(a, b)` calculates the difference of two bigints.

## Examples

```rescript
BigInt.sub(8n, 3n) == 5n
BigInt.sub(2n, 7n) == -5n
```
*/
external sub: (bigint, bigint) => bigint = "%subbigint"

/**
`mul(a, b)` calculates the product of two bigints.

## Examples

```rescript
BigInt.mul(5n, 3n) == 15n
BigInt.mul(-2n, 7n) == -14n
```
*/
external mul: (bigint, bigint) => bigint = "%mulbigint"

/**
`div(a, b)` calculates the quotient of two bigints.

## Examples

```rescript
BigInt.div(15n, 3n) == 5n
BigInt.div(14n, 3n) == 4n
```
*/
external div: (bigint, bigint) => bigint = "%divbigint"

/**
`mod(a, b)` calculates the remainder of dividing two bigints.

## Examples

```rescript
BigInt.mod(15n, 4n) == 3n
BigInt.mod(14n, 3n) == 2n
```
*/
external mod: (bigint, bigint) => bigint = "%modbigint"

/**
`bitwiseAnd(a, b)` calculates the bitwise AND of two bigints.

## Examples

```rescript
BigInt.bitwiseAnd(7n, 4n) == 4n
BigInt.bitwiseAnd(15n, 8n) == 8n
```
*/
external bitwiseAnd: (bigint, bigint) => bigint = "%andbigint"

/**
`bitwiseOr(a, b)` calculates the bitwise OR of two bigints.

## Examples

```rescript
BigInt.bitwiseOr(7n, 4n) == 7n
BigInt.bitwiseOr(8n, 4n) == 12n
```
*/
external bitwiseOr: (bigint, bigint) => bigint = "%orbigint"

/**
`bitwiseXor(a, b)` calculates the bitwise XOR of two bigints.

## Examples

```rescript
BigInt.bitwiseXor(7n, 4n) == 3n
BigInt.bitwiseXor(15n, 8n) == 7n
```
*/
external bitwiseXor: (bigint, bigint) => bigint = "%xorbigint"

/**
`bitwiseNot(bigint)` calculates the bitwise NOT of a bigint.

## Examples

```rescript
BigInt.bitwiseNot(2n) == -3n
BigInt.bitwiseNot(-1n) == 0n
```
*/
external bitwiseNot: bigint => bigint = "%bitnot_bigint"

/**
`shiftLeft(bigint, amount)` calculates the shifted value of a bigint by `amount` bits to the left.

## Examples

```rescript
BigInt.shiftLeft(4n, 1n) == 8n
BigInt.shiftLeft(1n, 3n) == 8n
```
*/
external shiftLeft: (bigint, bigint) => bigint = "%lslbigint"

/**
`shiftRight(bigint, amount)` calculates the shifted value of a bigint by `amount` bits to the right.

## Examples

```rescript
BigInt.shiftRight(8n, 1n) == 4n
BigInt.shiftRight(16n, 2n) == 4n
```
*/
external shiftRight: (bigint, bigint) => bigint = "%asrbigint"

/**
  `ignore(bigint)` ignores the provided bigint and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: bigint => unit = "%ignore"

/**
`land(a, b)` calculates the bitwise AND of two bigints.

**Deprecated:** Use `&&&` operator or `bitwiseAnd` instead.

## Examples

```rescript
BigInt.land(7n, 4n) == 4n
```
*/
@deprecated({
  reason: "Use `&&&` operator or `bitwiseAnd` instead.",
  migrate: %insert.unlabelledArgument(0) &&& %insert.unlabelledArgument(1),
  migrateInPipeChain: BigInt.bitwiseAnd(),
})
external land: (bigint, bigint) => bigint = "%andbigint"

/**
`lor(a, b)` calculates the bitwise OR of two bigints.

**Deprecated:** Use `|||` operator or `bitwiseOr` instead.

## Examples

```rescript
BigInt.lor(7n, 4n) == 7n
```
*/
@deprecated({
  reason: "Use `|||` operator or `bitwiseOr` instead.",
  migrate: %insert.unlabelledArgument(0) ||| %insert.unlabelledArgument(1),
  migrateInPipeChain: BigInt.bitwiseOr(),
})
external lor: (bigint, bigint) => bigint = "%orbigint"

/**
`lxor(a, b)` calculates the bitwise XOR of two bigints.

**Deprecated:** Use `^^^` operator or `bitwiseXor` instead.

## Examples

```rescript
BigInt.lxor(7n, 4n) == 3n
```
*/
@deprecated({
  reason: "Use `^^^` operator or `bitwiseXor` instead.",
  migrate: %insert.unlabelledArgument(0) ^^^ %insert.unlabelledArgument(1),
  migrateInPipeChain: BigInt.bitwiseXor(),
})
external lxor: (bigint, bigint) => bigint = "%xorbigint"

/**
`lnot(bigint)` calculates the bitwise NOT of a bigint.

**Deprecated:** Use `~~~` operator or `bitwiseNot` instead.

## Examples

```rescript
BigInt.lnot(2n) == -3n
```
*/
@deprecated({
  reason: "Use `~~~` operator or `bitwiseNot` instead.",
  migrate: ~~~(%insert.unlabelledArgument(0)),
  migrateInPipeChain: BigInt.bitwiseNot(),
})
external lnot: bigint => bigint = "%bitnot_bigint"

/**
`lsl(bigint, amount)` calculates the shifted value of a bigint by `amount` bits to the left.

**Deprecated:** Use `<<` operator or `shiftLeft` instead.

## Examples

```rescript
BigInt.lsl(4n, 1n) == 8n
```
*/
@deprecated({
  reason: "Use `<<` operator or `shiftLeft` instead.",
  migrate: %insert.unlabelledArgument(0) << %insert.unlabelledArgument(1),
  migrateInPipeChain: BigInt.shiftLeft(),
})
external lsl: (bigint, bigint) => bigint = "%lslbigint"

/**
`asr(bigint, amount)` calculates the shifted value of a bigint by `amount` bits to the right.

**Deprecated:** Use `>>` operator or `shiftRight` instead.

## Examples

```rescript
BigInt.asr(8n, 1n) == 4n
```
*/
@deprecated({
  reason: "Use `>>` operator or `shiftRight` instead.",
  migrate: %insert.unlabelledArgument(0) >> %insert.unlabelledArgument(1),
  migrateInPipeChain: BigInt.shiftRight(),
})
external asr: (bigint, bigint) => bigint = "%asrbigint"

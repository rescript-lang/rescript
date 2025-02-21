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
/* returns 123n */
BigInt.fromStringExn("123")

/* returns 0n */
BigInt.fromStringExn("")

/* returns 17n */
BigInt.fromStringExn("0x11")

/* returns 3n */
BigInt.fromStringExn("0b11")

/* returns 9n */
BigInt.fromStringExn("0o11")

/* catch exception */
try {
  BigInt.fromStringExn("a")
} catch {
| Exn.Error(_error) => 0n
}
```
*/
external fromStringExn: string => bigint = "BigInt"
@val external fromInt: int => bigint = "BigInt"
@val external fromFloat: float => bigint = "BigInt"

@send
/**
Formats a `bigint` as a string. Return a `string` representing the given value.
See [`toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) on MDN.

## Examples

```rescript
/* prints "123" */
BigInt.toString(123n)->Console.log
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
/* prints "123" */
BigInt.toString(123n)->Console.log
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

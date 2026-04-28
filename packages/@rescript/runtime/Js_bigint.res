/*** JavaScript BigInt API */

/**
Parses the given `string` into a `bigint` using JavaScript semantics. Return the
number as a `bigint` if successfully parsed. Uncaught syntax exception otherwise.

## Examples

```rescript
/* returns 123n */
Js.BigInt.fromStringExn("123")

/* returns 0n */
Js.BigInt.fromStringExn("")

/* returns 17n */
Js.BigInt.fromStringExn("0x11")

/* returns 3n */
Js.BigInt.fromStringExn("0b11")

/* returns 9n */
Js.BigInt.fromStringExn("0o11")

/* catch exception */
try {
  Js.BigInt.fromStringExn("a")
} catch {
| _ => Console.error("Error parsing bigint")
}
```
*/
@deprecated({
  reason: "Use `fromStringOrThrow` instead",
  migrate: BigInt.fromStringOrThrow(),
})
@val
external fromStringExn: string => bigint = "BigInt"

// Operations

external \"~-": bigint => bigint = "%negbigint"
external \"~+": bigint => bigint = "%identity"
external \"+": (bigint, bigint) => bigint = "%addbigint"
external \"-": (bigint, bigint) => bigint = "%subbigint"
external \"*": (bigint, bigint) => bigint = "%mulbigint"
external \"/": (bigint, bigint) => bigint = "%divbigint"
external mod: (bigint, bigint) => bigint = "%modbigint"
external \"**": (bigint, bigint) => bigint = "%powbigint"

@deprecated({
  reason: "Use `&&&` operator or `BigInt.bitwiseAnd` instead.",
  migrate: %insert.unlabelledArgument(0) &&& %insert.unlabelledArgument(1),
  migrateInPipeChain: BigInt.bitwiseAnd(),
})
external land: (bigint, bigint) => bigint = "%andbigint"

@deprecated({
  reason: "Use `|||` operator or `BigInt.bitwiseOr` instead.",
  migrate: %insert.unlabelledArgument(0) ||| %insert.unlabelledArgument(1),
  migrateInPipeChain: BigInt.bitwiseOr(),
})
external lor: (bigint, bigint) => bigint = "%orbigint"

@deprecated({
  reason: "Use `^^^` operator or `BigInt.bitwiseXor` instead.",
  migrate: %insert.unlabelledArgument(0) ^^^ %insert.unlabelledArgument(1),
  migrateInPipeChain: BigInt.bitwiseXor(),
})
external lxor: (bigint, bigint) => bigint = "%xorbigint"

@deprecated({
  reason: "Use `~~~` operator or `BigInt.bitwiseNot` instead.",
  migrate: ~~~(%insert.unlabelledArgument(0)),
  migrateInPipeChain: BigInt.bitwiseNot(),
})
let lnot = x => lxor(x, -1n)

@deprecated({
  reason: "Use `<<` operator or `BigInt.shiftLeft` instead.",
  migrate: %insert.unlabelledArgument(0) << %insert.unlabelledArgument(1),
  migrateInPipeChain: BigInt.shiftLeft(),
})
external lsl: (bigint, bigint) => bigint = "%lslbigint"

@deprecated({
  reason: "Use `>>` operator or `BigInt.shiftRight` instead.",
  migrate: %insert.unlabelledArgument(0) >> %insert.unlabelledArgument(1),
  migrateInPipeChain: BigInt.shiftRight(),
})
external asr: (bigint, bigint) => bigint = "%asrbigint"

/**
Formats a `bigint` as a string. Return a `string` representing the given value.
See [`toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) on MDN.

## Examples

```rescript
/* prints "123" */
Js.BigInt.toString(123n)->Js.log
```
*/
@deprecated({
  reason: "Use `BigInt.toString` instead.",
  migrate: BigInt.toString(),
})
@send
external toString: bigint => string = "toString"

/**
Returns a string with a language-sensitive representation of this BigInt value.

## Examples

```rescript
/* prints "123" */
Js.BigInt.toString(123n)->Js.log
```
*/
@deprecated({
  reason: "Use `BigInt.toLocaleString` instead.",
  migrate: BigInt.toLocaleString(),
})
@send
external toLocaleString: bigint => string = "toLocaleString"

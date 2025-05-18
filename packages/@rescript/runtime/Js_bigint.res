/*** JavaScript BigInt API */

@val
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
| _ => ...
}
```
*/
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

external land: (bigint, bigint) => bigint = "%andbigint"
external lor: (bigint, bigint) => bigint = "%orbigint"
external lxor: (bigint, bigint) => bigint = "%xorbigint"

let lnot = x => lxor(x, -1n)

external lsl: (bigint, bigint) => bigint = "%lslbigint"
external asr: (bigint, bigint) => bigint = "%asrbigint"

@send
/**
Formats a `bigint` as a string. Return a `string` representing the given value.
See [`toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) on MDN.

## Examples

```rescript
/* prints "123" */
Js.BigInt.toString(123n)->Js.log
```
*/
external toString: bigint => string = "toString"

@send
/**
Returns a string with a language-sensitive representation of this BigInt value.

## Examples

```rescript
/* prints "123" */
Js.BigInt.toString(123n)->Js.log
```
*/
external toLocaleString: bigint => string = "toLocaleString"

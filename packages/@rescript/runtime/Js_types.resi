/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/*** Provide utilities for manipulating JS types. */

/** Js symbol type (only available in ES6) */
@deprecated({
  reason: "Use `Symbol.t` instead.",
  migrate: %replace.type(: Symbol.t),
})
type symbol = Stdlib_Symbol.t

@deprecated({
  reason: "Use `Type.Classify.object` instead.",
  migrate: %replace.type(: Type.Classify.object),
})
type obj_val = Stdlib_Type.Classify.object

/** This type has only one value `undefined` */
@deprecated(
  "This has been deprecated and will be removed in v13. Use functions and types from the `Type` module instead."
)
type undefined_val

/** This type has only one value `null` */
@deprecated(
  "This has been deprecated and will be removed in v13. Use functions and types from the `Type` module instead."
)
type null_val

@deprecated({
  reason: "Use `Type.Classify.function` instead.",
  migrate: %replace.type(: Type.Classify.function),
})
type function_val = Stdlib_Type.Classify.function

@deprecated(
  "This has been deprecated and will be removed in v13. Use functions and types from the `Type` module instead."
)
type rec t<_> =
  | Undefined: t<undefined_val>
  | Null: t<null_val>
  | Boolean: t<bool>
  | Number: t<float>
  | String: t<string>
  | Function: t<function_val>
  | Object: t<obj_val>
  | Symbol: t<symbol>
  | BigInt: t<bigint>

/**
`test(value, t)` returns `true` if `value` is `typeof t`, otherwise `false`.
This is useful for doing runtime reflection on any given value.

## Examples

```rescript
test("test", String) == true
test(() => true, Function) == true
test("test", Boolean) == false
```
*/
@deprecated(
  "This has been deprecated and will be removed in v13. Use functions and types from the `Type` module instead."
)
let test: ('a, t<'b>) => bool

@deprecated(
  "This has been deprecated and will be removed in v13. Use functions and types from the `Type` module instead."
)
type tagged_t =
  | JSFalse
  | JSTrue
  | JSNull
  | JSUndefined
  | JSNumber(float)
  | JSString(string)
  | JSFunction(function_val)
  | JSObject(obj_val)
  | JSSymbol(symbol)
  | JSBigInt(bigint)

@deprecated(
  "This has been deprecated and will be removed in v13. Use functions and types from the `Type` module instead."
)
let classify: 'a => tagged_t

/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

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

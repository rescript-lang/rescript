/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/***
Contains functionality for dealing with values that can be both `null` and `undefined`
*/

@deprecated({
  reason: "Use `Nullable.t` instead.",
  migrate: %replace.type(: Nullable.t),
})
@unboxed
type t<+'a> = Primitive_js_extern.nullable<'a> =
  Value('a) | @as(null) Null | @as(undefined) Undefined

/** Constructs a value of `Js.null_undefined<'a>` containing a value of `'a`. */
@deprecated({
  reason: "Use `Nullable.make` instead.",
  migrate: Nullable.make(),
})
external return: 'a => t<'a> = "%identity"

/** Returns `true` if the given value is null or undefined, `false` otherwise. */
@deprecated({
  reason: "Use `Nullable.isNullable` instead.",
  migrate: Nullable.isNullable(),
})
external isNullable: t<'a> => bool = "%is_nullable"

/** The null value of type `Js.null_undefined<'a>`. */
@deprecated({
  reason: "Use `Nullable.null` instead.",
  migrate: Nullable.null,
})
external null: t<'a> = "%null"

/** The undefined value of type `Js.null_undefined<'a>`. */
@deprecated({
  reason: "Use `Nullable.undefined` instead.",
  migrate: Nullable.undefined,
})
external undefined: t<'a> = "%undefined"

/**
Maps the contained value using the given function.

If `Js.null_undefined<'a>` contains a value, that value is unwrapped, mapped to
a `'b` using the given function `a' => 'b`, then wrapped back up and returned
as `Js.null_undefined<'b>`.

## Examples

```rescript
let maybeGreetWorld = (maybeGreeting: Js.null_undefined<string>) =>
  Js.Null_undefined.bind(maybeGreeting, greeting => greeting ++ " world!")
```
*/
@deprecated({
  reason: "Use `Nullable.map` instead.",
  migrate: Nullable.map(),
})
let bind: (t<'a>, 'a => 'b) => t<'b>

/**
Iterates over the contained value with the given function.
If `Js.null_undefined<'a>` contains a value, that value is unwrapped and applied to the given function.

## Examples

```rescript
let maybeSay = (maybeMessage: Js.null_undefined<string>) =>
  Js.Null_undefined.iter(maybeMessage, message => Js.log(message))
```
*/
@deprecated({
  reason: "Use `Nullable.forEach` instead.",
  migrate: Nullable.forEach(),
})
let iter: (t<'a>, 'a => unit) => unit

/**
Maps `option<'a>` to `Js.null_undefined<'a>`.
`Some(a)` => `a`
`None` => `undefined`
*/
@deprecated({
  reason: "Use `Nullable.fromOption` instead.",
  migrate: Nullable.fromOption(),
})
let fromOption: option<'a> => t<'a>

@deprecated({
  reason: "Use `Nullable.fromOption` instead.",
  migrate: Nullable.fromOption(),
})
let from_opt: option<'a> => t<'a>

/**
Maps `Js.null_undefined<'a>` to `option<'a>`.
`a` => `Some(a)`
`undefined` => `None`
`null` => `None`
*/
@deprecated({
  reason: "Use `Nullable.toOption` instead.",
  migrate: Nullable.toOption(),
})
external toOption: t<'a> => option<'a> = "%nullable_to_opt"

@deprecated({
  reason: "Use `Nullable.toOption` instead.",
  migrate: Nullable.toOption(),
})
external to_opt: t<'a> => option<'a> = "%nullable_to_opt"

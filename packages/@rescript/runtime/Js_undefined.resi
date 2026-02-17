/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/*** Provides functionality for dealing with the `Js.undefined<'a>` type */

/** Local alias for `Js.undefined<'a>` */
@deprecated({
  reason: "Use `undefined` directly instead.",
  migrate: %replace.type(: undefined),
})
type t<+'a> = Primitive_js_extern.undefined<'a>

/** Constructs a value of `Js.undefined<'a>` containing a value of `'a`. */
@deprecated({
  reason: "Use `Nullable.make` or `option` directly instead.",
  migrate: Nullable.make(),
})
external return: 'a => t<'a> = "%identity"

/** Returns `true` if the given value is empty (undefined), `false` otherwise. */
@deprecated
let test: t<'a> => bool

/**
Returns `true` if the given value is empty (undefined).

**since 1.6.1**
*/
@deprecated
let testAny: 'a => bool

/** The empty value, `undefined` */
@deprecated({
  reason: "Use `Nullable.undefined` instead.",
  migrate: Nullable.undefined,
})
external empty: t<'a> = "%undefined"

@deprecated({
  reason: "Use `Nullable.getUnsafe` instead.",
  migrate: Nullable.getUnsafe(),
})
external getUnsafe: t<'a> => 'a = "%identity"

@deprecated({
  reason: "Use `Nullable.getOrThrow` instead.",
  migrate: Nullable.getOrThrow(),
})
let getExn: t<'a> => 'a

/**
Maps the contained value using the given function.
If `Js.undefined<'a>` contains a value, that value is unwrapped, mapped to a
`'b` using the given function `a' => 'b`, then wrapped back up and returned as
`Js.undefined<'b>`.

## Examples

```rescript
let maybeGreetWorld = (maybeGreeting: Js.undefined<string>) =>
  Js.Undefined.bind(maybeGreeting, greeting => greeting ++ " world!")
```
*/
@deprecated({
  reason: "Use `Nullable.map` instead.",
  migrate: Nullable.map(),
})
let bind: (t<'a>, 'a => 'b) => t<'b>

/**
Iterates over the contained value with the given function. If
`Js.undefined<'a>` contains a value, that value is unwrapped and applied to the
given function.

## Examples

```rescript
let maybeSay = (maybeMessage: Js.undefined<string>) =>
  Js.Undefined.iter(maybeMessage, message => Js.log(message))
```
*/
@deprecated({
  reason: "Use `Nullable.forEach` instead.",
  migrate: Nullable.forEach(),
})
let iter: (t<'a>, 'a => unit) => unit

/**
Maps `option<'a>` to `Js.undefined<'a>`.
`Some(a)` => `a`
`None` => `empty`
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
Maps `Js.undefined<'a>` to `option<'a>`
`a` => `Some(a)`
`empty` => `None`
*/
@deprecated({
  reason: "Use `Nullable.toOption` instead.",
  migrate: Nullable.toOption(),
})
let toOption: t<'a> => option<'a>

@deprecated({
  reason: "Use `Nullable.toOption` instead.",
  migrate: Nullable.toOption(),
})
let to_opt: t<'a> => option<'a>

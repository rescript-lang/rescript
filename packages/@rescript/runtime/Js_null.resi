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

/*** Provides functionality for dealing with the `Js.null<'a>` type */

@deprecated({
  reason: "Use `Null.t` instead.",
  migrate: %replace.type(: Null.t),
})
@unboxed
type t<+'a> = Primitive_js_extern.null<'a> = Value('a) | @as(null) Null

/** Constructs a value of `Js.null<'a>` containing a value of `'a`. */
@deprecated({
  reason: "Use `Null.make` instead.",
  migrate: Null.make(),
})
external return: 'a => t<'a> = "%identity"

/** Returns `true` if the given value is empty (`null`), `false` otherwise. */
@deprecated({
  reason: "Use `== Js.null` directly.",
  migrate: %insert.unlabelledArgument(0) === Null.null,
  migrateInPipeChain: Null.equal(Null, (a, b) => a === b),
})
let test: t<'a> => bool

/** The empty value, `null` */
@deprecated({
  reason: "Use `Null.null` instead.",
  migrate: Null.null,
})
external empty: t<'a> = "%null"

@deprecated({
  reason: "Use `Null.getUnsafe` instead.",
  migrate: Null.getUnsafe(),
})
external getUnsafe: t<'a> => 'a = "%identity"

@deprecated({
  reason: "Use `Null.getOrThrow` instead.",
  migrate: Null.getOrThrow(),
})
let getExn: t<'a> => 'a

/**
Maps the contained value using the given function.

If `Js.null<'a>` contains a value, that value is unwrapped, mapped to a `'b`
using the given function `'a => 'b`, then wrapped back up and returned as
`Js.null<'b>`.

## Examples

```rescript
let maybeGreetWorld = (maybeGreeting: Js.null<string>) =>
  Js.Null.bind(maybeGreeting, greeting => greeting ++ " world!")
```
*/
@deprecated({
  reason: "Use `Null.map` instead.",
  migrate: Null.map(),
})
let bind: (t<'a>, 'a => 'b) => t<'b>

/**
Iterates over the contained value with the given function.
If `Js.null<'a>` contains a value, that value is unwrapped and applied to the given function.

## Examples

```rescript
let maybeSay = (maybeMessage: Js.null<string>) =>
  Js.Null.iter(maybeMessage, message => Js.log(message))
```
*/
@deprecated({
  reason: "Use `Null.forEach` instead.",
  migrate: Null.forEach(),
})
let iter: (t<'a>, 'a => unit) => unit

/**
Maps `option<'a>` to `Js.null<'a>`.
`Some(a)` => `a`
`None` => `empty`
*/
@deprecated({
  reason: "Use `Null.fromOption` instead.",
  migrate: Null.fromOption(),
})
let fromOption: option<'a> => t<'a>

@deprecated({
  reason: "Use `Null.fromOption` instead.",
  migrate: Null.fromOption(),
})
let from_opt: option<'a> => t<'a>

/**
Maps `Js.null<'a>` to `option<'a>`.
`a` => `Some(a)`
`empty` => `None`
*/
@deprecated({
  reason: "Use `Null.toOption` instead.",
  migrate: Null.toOption(),
})
external toOption: t<'a> => option<'a> = "%null_to_opt"

@deprecated({
  reason: "Use `Null.toOption` instead.",
  migrate: Null.toOption(),
})
external to_opt: t<'a> => option<'a> = "%null_to_opt"

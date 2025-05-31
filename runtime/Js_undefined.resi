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

/*** Provides functionality for dealing with the `Js.undefined<'a>` type */

/** Local alias for `Js.undefined<'a>` */
type t<+'a> = Primitive_js_extern.undefined<'a>

/** Constructs a value of `Js.undefined<'a>` containing a value of `'a`. */
external return: 'a => t<'a> = "%identity"

/** Returns `true` if the given value is empty (undefined), `false` otherwise. */
@deprecated("Use = Js.undefined directly")
let test: t<'a> => bool

/**
Returns `true` if the given value is empty (undefined).

**since 1.6.1**
*/
let testAny: 'a => bool

/** The empty value, `undefined` */
external empty: t<'a> = "%undefined"

external getUnsafe: t<'a> => 'a = "%identity"
let getExn: t<'a> => 'a

/**
Maps the contained value using the given function.
If `Js.undefined<'a>` contains a value, that value is unwrapped, mapped to a
`'b` using the given function `a' => 'b`, then wrapped back up and returned as
`Js.undefined<'b>`.

## Examples

```rescript
let maybeGreetWorld = (maybeGreeting: Js.undefined<string>) =>
  Js.Undefined.bind(maybeGreeting, (. greeting) => greeting ++ " world!")
```
*/
let bind: (t<'a>, 'a => 'b) => t<'b>

/**
Iterates over the contained value with the given function. If
`Js.undefined<'a>` contains a value, that value is unwrapped and applied to the
given function.

## Examples

```rescript
let maybeSay = (maybeMessage: Js.undefined<string>) =>
  Js.Undefined.iter(maybeMessage, (. message) => Js.log(message))
```
*/
let iter: (t<'a>, 'a => unit) => unit

/**
Maps `option<'a>` to `Js.undefined<'a>`.
`Some(a)` => `a`
`None` => `empty`
*/
let fromOption: option<'a> => t<'a>

@deprecated("Use fromOption instead") let from_opt: option<'a> => t<'a>

/**
Maps `Js.undefined<'a>` to `option<'a>`
`a` => `Some(a)`
`empty` => `None`
*/
external toOption: t<'a> => option<'a> = "%undefined_to_opt"

@deprecated("use toOption instead") external to_opt: t<'a> => option<'a> = "%undefined_to_opt"

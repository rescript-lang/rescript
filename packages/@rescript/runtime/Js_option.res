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

/*** Provide utilities for handling `option`. */

/**
`Js.Option.t` is an alias for `option`
*/
type t<'a> = option<'a>

/**
Wraps the given value in `Some()`

## Examples

```rescript
Js.Option.some(1066) == Some(1066)
```
*/
let some = x => Some(x)

/**
Returns `true` if the argument is `Some(value)`; `false` if the argument is
`None`.
*/
let isSome = x =>
  switch x {
  | None => false
  | Some(_) => true
  }

/**
The first argument to `isSomeValue` is an uncurried function `eq()` that
takes two arguments and returns `true` if they are considered to be equal. It
is used to compare a plain value `v1`(the second argument) with an `option`
value. If the `option` value is `None`, `isSomeValue()` returns `false`; if
the third argument is `Some(v2)`, `isSomeValue()` returns the result of
calling `eq(v1, v2)`.

## Examples

```rescript
let clockEqual = (. a, b) => mod(a, 12) == mod(b, 12)
Js.Option.isSomeValue(clockEqual, 3, Some(15)) == true
Js.Option.isSomeValue(clockEqual, 3, Some(4)) == false
Js.Option.isSomeValue(clockEqual, 3, None) == false
```
*/
let isSomeValue = (eq, v, x) =>
  switch x {
  | None => false
  | Some(x) => eq(v, x)
  }

/** Returns `true` if the argument is `None`; `false` otherwise. */
let isNone = x =>
  switch x {
  | None => true
  | Some(_) => false
  }

/**
If the argument to `getExn()` is of the form `Some(value)`, returns `value`.
If given `None`, it throws a `getExn` exception.
*/
let getExn = x =>
  switch x {
  | None => Stdlib_Exn.raiseError("getExn")
  | Some(x) => x
  }

/**
The first argument to `equal` is an uncurried function `eq()` that takes two
arguments and returns `true` if they are considered to be equal. The second
and third arguments are `option` values.

If the second and third arguments are of the form:

* `Some(v1)` and `Some(v2)`: returns `eq(v1, v2)`
* `Some(v1)` and `None`: returns `false`
* `None` and `Some(v2)`: returns `false`
* `None` and `None`: returns `true`

## Examples

```rescript
let clockEqual = (. a, b) => mod(a, 12) == mod(b, 12)
Js.Option.equal(clockEqual, Some(3), Some(15)) == true
Js.Option.equal(clockEqual, Some(3), Some(16)) == false
Js.Option.equal(clockEqual, Some(3), None) == false
Js.Option.equal(clockEqual, None, Some(15)) == false
Js.Option.equal(clockEqual, None, None) == true
```
*/
let equal = (eq, a, b) =>
  switch a {
  | None => b == None
  | Some(x) =>
    switch b {
    | None => false
    | Some(y) => eq(x, y)
    }
  }

/**
The first argument to `andThen()` is an uncurried function `f()` that takes a
plain value and returns an `option` result. The second argument is an
`option` value. If the second argument is `None`, the return value is `None`.
If the second argument is `Some(v)`, the return value is `f(v)`.

## Examples

```rescript
let reciprocal = (. x) => x == 0 ? None : Some(1.0 /. Belt.Int.toFloat(x))
Js.Option.andThen(reciprocal, Some(5)) == Some(0.2)
Js.Option.andThen(reciprocal, Some(0)) == None
Js.Option.andThen(reciprocal, None) == None
```
*/
let andThen = (f, x) =>
  switch x {
  | None => None
  | Some(x) => f(x)
  }

/**
The first argument to `map()` is an uncurried function `f()` that takes a
plain value and returns a plain result. The second argument is an `option`
value. If it is of the form `Some(v)`, `map()` returns `Some(f(v))`; if it is
`None`, the return value is `None`, and function `f()` is not called.

## Examples

```rescript
let square = (. x) => x * x
Js.Option.map(square, Some(3)) == Some(9)
Js.Option.map(square, None) == None
```
*/
let map = (f, x) =>
  switch x {
  | None => None
  | Some(x) => Some(f(x))
  }

/**
The first argument to `getWithDefault()` is a default value. If the second
argument is of the form `Some(v)`, `getWithDefault()` returns `v`; if the
second argument is `None`, the return value is the default value.

## Examples

```rescript
Js.Option.getWithDefault(1066, Some(15)) == 15
Js.Option.getWithDefault(1066, None) == 1066
```
*/
let getWithDefault = (a, x) =>
  switch x {
  | None => a
  | Some(x) => x
  }

/** **See:** [getWithDefault](#getWithDefault) */
let default = getWithDefault

/**
The first argument to `filter()` is an uncurried function that takes a plain
value and returns a boolean. The second argument is an `option` value.

If the second argument is of the form `Some(v)` and `f(v)` is `true`,
the return value is `Some(v)`. Otherwise, the return value is `None`.

## Examples

```rescript
let isEven = (. x) => mod(x, 2) == 0
Js.Option.filter(isEven, Some(2)) == Some(2)
Js.Option.filter(isEven, Some(3)) == None
Js.Option.filter(isEven, None) == None
```
*/
let filter = (f, x) =>
  switch x {
  | None => None
  | Some(x) =>
    if f(x) {
      Some(x)
    } else {
      None
    }
  }

/**
The `firstSome()` function takes two `option` values; if the first is of the form `Some(v1)`, that is the return value. Otherwise, `firstSome()` returns the second value.

## Examples

```rescript
Js.Option.firstSome(Some("one"), Some("two")) == Some("one")
Js.Option.firstSome(Some("one"), None) == Some("one")
Js.Option.firstSome(None, Some("two")) == Some("two")
Js.Option.firstSome(None, None) == None
```
*/
let firstSome = (a, b) =>
  switch (a, b) {
  | (Some(_), _) => a
  | (None, Some(_)) => b
  | (None, None) => None
  }

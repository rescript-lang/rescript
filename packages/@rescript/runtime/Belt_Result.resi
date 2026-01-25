/* Copyright (C) 2017 Authors of ReScript
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

/***
Result types are really useful to describe the result of a certain operation
without relying on exceptions or `option` types.

This module gives you useful utilities to create and combine `Result` data.
*/

type t<'a, 'b> = result<'a, 'b> =
  | Ok('a)
  | Error('b)

/**
`getExn(res)`: when `res` is `Ok(n)`, returns `n` when `res` is `Error(m)`, throw an exception

## Examples

```rescript
Belt.Result.Ok(42)->Belt.Result.getExn == 42

switch Belt.Result.getExn(Belt.Result.Error("Invalid data")) {
// throw a exception
| exception _ => assert(true)
| _ => assert(false)
}
```
*/
let getExn: t<'a, 'b> => 'a

/**
`getOrThrow(res)`: when `res` is `Ok(n)`, returns `n` when `res` is `Error(m)`, throw an exception

## Examples

```rescript
Belt.Result.Ok(42)->Belt.Result.getOrThrow == 42

switch Belt.Result.getOrThrow(Belt.Result.Error("Invalid data")) {
// throw a exception
| exception _ => assert(true)
| _ => assert(false)
}
```
*/
let getOrThrow: t<'a, 'b> => 'a

@deprecated("Use `mapWithDefault` instead")
let mapWithDefaultU: (t<'a, 'c>, 'b, 'a => 'b) => 'b
/**
`mapWithDefault(res, default, f)`: When res is `Ok(n)`, returns `f(n)`,
otherwise `default`.

## Examples

```rescript
let ok = Belt.Result.Ok(42)
Belt.Result.mapWithDefault(ok, 0, x => x / 2) == 21

let error = Belt.Result.Error("Invalid data")
Belt.Result.mapWithDefault(error, 0, x => x / 2) == 0
```
*/
let mapWithDefault: (t<'a, 'c>, 'b, 'a => 'b) => 'b

@deprecated("Use `map` instead")
let mapU: (t<'a, 'c>, 'a => 'b) => t<'b, 'c>
/**
`map(res, f)`: When res is `Ok(n)`, returns `Ok(f(n))`. Otherwise returns res
unchanged. Function `f` takes a value of the same type as `n` and returns an
ordinary value.

## Examples

```rescript
let f = x => sqrt(Belt.Int.toFloat(x))

Belt.Result.map(Ok(64), f) == Ok(8.0)

Belt.Result.map(Error("Invalid data"), f) == Error("Invalid data")
```
*/
let map: (t<'a, 'c>, 'a => 'b) => t<'b, 'c>

@deprecated("Use `flatMap` instead")
let flatMapU: (t<'a, 'c>, 'a => t<'b, 'c>) => t<'b, 'c>
/**
`flatMap(res, f)`: When res is `Ok(n)`, returns `f(n)`. Otherwise, returns res
unchanged. Function `f` takes a value of the same type as `n` and returns a
`Belt.Result`.

## Examples

```rescript
let recip = x =>
  if x !== 0.0 {
    Belt.Result.Ok(1.0 /. x)
  } else {
    Belt.Result.Error("Divide by zero")
  }

Belt.Result.flatMap(Ok(2.0), recip) == Ok(0.5)

Belt.Result.flatMap(Ok(0.0), recip) == Error("Divide by zero")

Belt.Result.flatMap(Error("Already bad"), recip) == Error("Already bad")
```
*/
let flatMap: (t<'a, 'c>, 'a => t<'b, 'c>) => t<'b, 'c>

/**
`getWithDefault(res, defaultValue)`: If `res` is `Ok(n)`, returns `n`,
otherwise `default`

## Examples

```rescript
Belt.Result.getWithDefault(Ok(42), 0) == 42

Belt.Result.getWithDefault(Error("Invalid Data"), 0) == 0
```
*/
let getWithDefault: (t<'a, 'b>, 'a) => 'a

/**
`isOk(res)`: Returns `true` if `res` is of the form `Ok(n)`, `false` if it is
the `Error(e)` variant.
*/
let isOk: t<'a, 'b> => bool

/**
`isError(res)`: Returns `true` if `res` is of the form `Error(e)`, `false` if
it is the `Ok(n)` variant.
*/
let isError: t<'a, 'b> => bool

@deprecated("Use `eq` instead")
let eqU: (t<'a, 'c>, t<'b, 'd>, ('a, 'b) => bool) => bool
/**
`eq(res1, res2, f)`: Determine if two `Belt.Result` variables are equal with
respect to an equality function. If `res1` and `res2` are of the form `Ok(n)`
and `Ok(m)`, return the result of `f(n, m)`. If one of `res1` and `res2` are of
the form `Error(e)`, return false If both `res1` and `res2` are of the form
`Error(e)`, return true

## Examples

```rescript
let good1 = Belt.Result.Ok(42)

let good2 = Belt.Result.Ok(32)

let bad1 = Belt.Result.Error("invalid")

let bad2 = Belt.Result.Error("really invalid")

let mod10equal = (a, b) => mod(a, 10) === mod(b, 10)

Belt.Result.eq(good1, good2, mod10equal) == true

Belt.Result.eq(good1, bad1, mod10equal) == false

Belt.Result.eq(bad2, good2, mod10equal) == false

Belt.Result.eq(bad1, bad2, mod10equal) == true
```
*/
let eq: (t<'a, 'c>, t<'b, 'd>, ('a, 'b) => bool) => bool

@deprecated("Use `cmp` instead")
let cmpU: (t<'a, 'c>, t<'b, 'd>, ('a, 'b) => int) => int
/**
`cmp(res1, res2, f)`: Compare two `Belt.Result` variables with respect to a
comparison function. The comparison function returns -1 if the first variable
is "less than" the second, 0 if the two variables are equal, and 1 if the first
is "greater than" the second.

If `res1` and `res2` are of the form `Ok(n)` and `Ok(m)`, return the result of
`f(n, m)`. If `res1` is of the form `Error(e)` and `res2` of the form `Ok(n)`,
return -1 (nothing is less than something) If `res1` is of the form `Ok(n)` and
`res2` of the form `Error(e)`, return 1 (something is greater than nothing) If
both `res1` and `res2` are of the form `Error(e)`, return 0 (equal)

## Examples

```rescript
let good1 = Belt.Result.Ok(59)

let good2 = Belt.Result.Ok(37)

let bad1 = Belt.Result.Error("invalid")

let bad2 = Belt.Result.Error("really invalid")

let mod10cmp = (a, b) => Pervasives.compare(mod(a, 10), mod(b, 10))

Belt.Result.cmp(Ok(39), Ok(57), mod10cmp) == 1

Belt.Result.cmp(Ok(57), Ok(39), mod10cmp) == -1

Belt.Result.cmp(Ok(39), Error("y"), mod10cmp) == 1

Belt.Result.cmp(Error("x"), Ok(57), mod10cmp) == -1

Belt.Result.cmp(Error("x"), Error("y"), mod10cmp) == 0
```
*/
let cmp: (t<'a, 'c>, t<'b, 'd>, ('a, 'b) => int) => int

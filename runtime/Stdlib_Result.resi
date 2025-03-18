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

/**
  Result types are really useful to describe the result of a certain operation
  without relying on exceptions or `option` types.

  This module gives you useful utilities to create and combine `Result` data.
*/
/**
  The type `Result.t(result, err)` describes a variant of two states:
  `Ok(someResult)` represents a successful operation, whereby
  ``Error(someError)` signals an erroneous operation.

  In this concrete example, we are defining our own `Result` type to reflect an HTTP like
  query operation:

  ```res example
  type responseError = NotAvailable | NotFound
  type queryResult = result<string, responseError>

  let failQueryUser = (username: string): queryResult => {
    Error(NotAvailable)
  }
```
*/
type t<'res, 'err> = result<'res, 'err> = Ok('res) | Error('err)

/**
  `getExn(res)`: when `res` is `Ok(n)`, returns `n` when `res` is `Error(m)`, raise an exception

  ```res example
  Result.getExn(Result.Ok(42)) == 42

  switch Result.getExn(Error("Invalid data")) {
  | exception Not_found => assert(true)
  | _ => assert(false)
  }
  ```
*/
let getExn: result<'a, 'b> => 'a

/**
`mapOr(res, default, f)`: When res is `Ok(n)`, returns `f(n)`, otherwise `default`.

## Examples

```rescript
let ok = Ok(42)
Result.mapOr(ok, 0, (x) => x / 2) == 21

let error = Error("Invalid data")
Result.mapOr(error, 0, (x) => x / 2) == 0
```
*/
let mapOr: (result<'a, 'c>, 'b, 'a => 'b) => 'b

@deprecated("Use mapOr instead")
let mapWithDefault: (result<'a, 'c>, 'b, 'a => 'b) => 'b

/**
`map(res, f)`: When res is `Ok(n)`, returns `Ok(f(n))`. Otherwise returns res
unchanged. Function `f` takes a value of the same type as `n` and returns an
ordinary value.

## Examples

```rescript
let f = (x) => sqrt(Int.toFloat(x))

Result.map(Ok(64), f) == Ok(8.0)

Result.map(Error("Invalid data"), f) == Error("Invalid data")
```
*/
let map: (result<'a, 'c>, 'a => 'b) => result<'b, 'c>

/**
`flatMap(res, f)`: When res is `Ok(n)`, returns `f(n)`. Otherwise, returns res
unchanged. Function `f` takes a value of the same type as `n` and returns a
`Result`.

## Examples

```rescript
let recip = (x) =>
  if (x !== 0.0) {
    Ok(1.0 /. x)
  } else {
    Error("Divide by zero")
  }

Result.flatMap(Ok(2.0), recip) == Ok(0.5)

Result.flatMap(Ok(0.0), recip) == Error("Divide by zero")

Result.flatMap(Error("Already bad"), recip) == Error("Already bad")
```
*/
let flatMap: (result<'a, 'c>, 'a => result<'b, 'c>) => result<'b, 'c>

/**
`getOr(res, defaultValue)`: If `res` is `Ok(n)`, returns `n`, otherwise `default`

## Examples

```rescript
Result.getOr(Ok(42), 0) == 42

Result.getOr(Error("Invalid Data"), 0) == 0
```
*/
let getOr: (result<'a, 'b>, 'a) => 'a

@deprecated("Use getOr instead")
let getWithDefault: (result<'a, 'b>, 'a) => 'a

/**
`isOk(res)`: Returns `true` if `res` is of the form `Ok(n)`, `false` if it is the `Error(e)` variant.
*/
let isOk: result<'a, 'b> => bool

/**
`isError(res)`: Returns `true` if `res` is of the form `Error(e)`, `false` if it is the `Ok(n)` variant.
*/
let isError: result<'a, 'b> => bool

/**
`equal(res1, res2, f)`: Determine if two `Result` variables are equal with
respect to an equality function. If `res1` and `res2` are of the form `Ok(n)`
and `Ok(m)`, return the result of `f(n, m)`. If one of `res1` and `res2` are of
the form `Error(e)`, return false If both `res1` and `res2` are of the form
`Error(e)`, return true

## Examples

```rescript
let good1 = Ok(42)

let good2 = Ok(32)

let bad1 = Error("invalid")

let bad2 = Error("really invalid")

let mod10equal = (a, b) => mod(a, 10) === mod(b, 10)

Result.equal(good1, good2, mod10equal) == true

Result.equal(good1, bad1, mod10equal) == false

Result.equal(bad2, good2, mod10equal) == false

Result.equal(bad1, bad2, mod10equal) == true
```
*/
let equal: (result<'a, 'c>, result<'b, 'd>, ('a, 'b) => bool) => bool

/**
`compare(res1, res2, f)`: Compare two `Result` variables with respect to a
comparison function. The comparison function returns -1. if the first variable
is "less than" the second, 0. if the two variables are equal, and 1. if the first
is "greater than" the second.

If `res1` and `res2` are of the form `Ok(n)` and `Ok(m)`, return the result of
`f(n, m)`. If `res1` is of the form `Error(e)` and `res2` of the form `Ok(n)`,
return -1. (nothing is less than something) If `res1` is of the form `Ok(n)` and
`res2` of the form `Error(e)`, return 1. (something is greater than nothing) If
both `res1` and `res2` are of the form `Error(e)`, return 0. (equal)

## Examples

```rescript
let good1 = Ok(59)

let good2 = Ok(37)

let bad1 = Error("invalid")

let bad2 = Error("really invalid")

let mod10cmp = (a, b) => Int.compare(mod(a, 10), mod(b, 10))

Result.compare(Ok(39), Ok(57), mod10cmp) == 1.

Result.compare(Ok(57), Ok(39), mod10cmp) == (-1.)

Result.compare(Ok(39), Error("y"), mod10cmp) == 1.

Result.compare(Error("x"), Ok(57), mod10cmp) == (-1.)

Result.compare(Error("x"), Error("y"), mod10cmp) == 0.
```
*/
let compare: (result<'a, 'c>, result<'b, 'd>, ('a, 'b) => Stdlib_Ordering.t) => Stdlib_Ordering.t

/**
`forEach(res, f)` runs the provided function `f` on the `Ok` value. If `res` is `Error`, nothing happens.

## Examples

```rescript
Result.forEach(Ok(3), Console.log) // Logs "3", returns ()
Result.forEach(Error("x"), Console.log) // Does nothing, returns ()
```
*/
let forEach: (result<'a, 'b>, 'a => unit) => unit

/**
`mapError(r, f)` generates a new `result` by applying the function `f` to the `Error` value. If the source is `Ok`, return it as-is.

## Examples

```rescript
let format = n => `Error code: ${n->Int.toString}`
Result.mapError(Error(14), format) // Error("Error code: 14")
Result.mapError(Ok("abc"), format) // Ok("abc")
```
*/
let mapError: (result<'a, 'b>, 'b => 'c) => result<'a, 'c>

/**
`all(results)` returns a result of array if all options are Ok, otherwise returns Error.
## Examples
```rescript
Result.all([Ok(1), Ok(2), Ok(3)]) // Ok([1, 2, 3])
Result.all([Ok(1), Error(1)]) // Error(1)
```
*/
let all: array<result<'a, 'b>> => result<array<'a>, 'b>

/**
`all2((r1, r2))`. Like `all()`, but with a fixed size tuple of 2
*/
let all2: ((result<'r1, 'e>, result<'r2, 'e>)) => result<('r1, 'r2), 'e>

/**
`all3((r1, r2, r3))`. Like `all()`, but with a fixed size tuple of 2
*/
let all3: ((result<'r1, 'e>, result<'r2, 'e>, result<'r3, 'e>)) => result<('r1, 'r2, 'r3), 'e>

/**
`all4((r1, r2, r3, r4))`. Like `all()`, but with a fixed size tuple of 2
*/
let all4: ((result<'r1, 'e>, result<'r2, 'e>, result<'r3, 'e>, result<'r4, 'e>)) => result<
  ('r1, 'r2, 'r3, 'r4),
  'e,
>

/**
`all5((r1, r2, r3, r4, r5))`. Like `all()`, but with a fixed size tuple of 2
*/
let all5: (
  (result<'r1, 'e>, result<'r2, 'e>, result<'r3, 'e>, result<'r4, 'e>, result<'r5, 'e>)
) => result<('r1, 'r2, 'r3, 'r4, 'r5), 'e>

/**
`all6((r1, r2, r3, r4, r5, r6))`. Like `all()`, but with a fixed size tuple of 2
*/
let all6: (
  (
    result<'r1, 'e>,
    result<'r2, 'e>,
    result<'r3, 'e>,
    result<'r4, 'e>,
    result<'r5, 'e>,
    result<'r6, 'e>,
  )
) => result<('r1, 'r2, 'r3, 'r4, 'r5, 'r6), 'e>

/**
  `ignore(result)` ignores the provided result and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: result<'res, 'err> => unit = "%ignore"

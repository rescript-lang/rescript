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

/***
Provide utilities for handling `int`.
*/

/*
If we use number, we need coerce to int32 by adding `|0`,
otherwise `+0` can be wrong.
Most JS API is float oriented, it may overflow int32 or
comes with `NAN`
*/

/* + conversion */

@send
/**
Formats an `int` using exponential (scientific) notation.
Returns a `string` representing the given value in exponential notation.
Raises `RangeError` if digits is not in the range \[0, 20\] (inclusive).

See [`toExponential`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential) on MDN.

## Examples

```rescript
/* prints "7.7e+1" */
Js.log(Js.Int.toExponential(77))
```
*/
external toExponential: int => string = "toExponential"

@send
/**
Formats an `int` using exponential (scientific) notation.
`digits` specifies how many digits should appear after the decimal point. The value must be in the range \[0, 20\] (inclusive).

Returns a `string` representing the given value in exponential notation.

The output will be rounded or padded with zeroes if necessary.
Raises `RangeError` if `digits` is not in the range \[0, 20\] (inclusive).

See [`toExponential`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential) on MDN.

## Examples

```rescript
/* prints "7.70e+1" */
Js.log(Js.Int.toExponentialWithPrecision(77, ~digits=2))

/* prints "5.68e+3" */
Js.log(Js.Int.toExponentialWithPrecision(5678, ~digits=2))
```
*/
external toExponentialWithPrecision: (int, ~digits: int) => string = "toExponential"

@send
/**
Formats an `int` using some fairly arbitrary rules.
Returns a `string` representing the given value in fixed-point (usually).

`toPrecision` differs from `toFixed` in that the former will format the number with full precision, while the latter will not output any digits after the decimal point.
Raises `RangeError` if `digits` is not in the range accepted by this function.

See [`toPrecision`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision) on MDN.

## Examples

```rescript
/* prints "123456789" */
Js.log(Js.Int.toPrecision(123456789))
```
*/
external toPrecision: int => string = "toPrecision"

@send
/**
Formats an `int` using some fairly arbitrary rules.
`digits` specifies how many digits should appear in total. The value must between 0 and some arbitrary number that's hopefully at least larger than 20 (for Node it's 21. Why? Who knows).

Returns a `string` representing the given value in fixed-point or scientific notation.

The output will be rounded or padded with zeroes if necessary.

`toPrecisionWithPrecision` differs from `toFixedWithPrecision` in that the former will count all digits against the precision, while the latter will count only the digits after the decimal point.
`toPrecisionWithPrecision` will also use scientific notation if the specified precision is less than the number of digits before the decimal point.
Raises `RangeError` if `digits` is not in the range accepted by this function.


See [`toPrecision`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision) on MDN.

## Examples

```rescript
/* prints "1.2e+8" */
Js.log(Js.Int.toPrecisionWithPrecision(123456789, ~digits=2))

/* prints "0.0" */
Js.log(Js.Int.toPrecisionWithPrecision(0, ~digits=2))
```
*/
external toPrecisionWithPrecision: (int, ~digits: int) => string = "toPrecision"

@send
/**
Formats an `int` as a `string`. Returns a `string` representing the given value
in fixed-point (usually).

See [`toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) on MDN.

## Examples

```rescript
/* prints "123456789" */
Js.log(Js.Int.toString(123456789))
```
*/
external toString: int => string = "toString"

@send
/**
Formats an `int` as a `string`. `radix` specifies the radix base to use for the
formatted number. The value must be in the range \[2, 36\] (inclusive). Returns
a `string` representing the given value in fixed-point (usually). Raises
`RangeError` if `radix` is not in the range \[2, 36\] (inclusive).


See [`toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) on MDN.

## Examples

```rescript
/* prints "110" */
Js.log(Js.Int.toStringWithRadix(6, ~radix=2))

/* prints "deadbeef" */
Js.log(Js.Int.toStringWithRadix(3735928559, ~radix=16))

/* prints "2n9c" */
Js.log(Js.Int.toStringWithRadix(123456, ~radix=36))
```
*/
external toStringWithRadix: (int, ~radix: int) => string = "toString"

external toFloat: int => float = "%floatofint"

let equal = (x: int, y) => x == y
let max: int = 2147483647
let min: int = -2147483648

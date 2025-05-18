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
Provide utilities for JS float.
*/

@val
/**
The special value "Not a Number". See [`NaN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN) on MDN.
*/
external _NaN: float = "NaN"

@val
@scope("Number")
/**
Tests if the given value is `_NaN`

Note that both `_NaN = _NaN` and `_NaN == _NaN` will return `false`. `isNaN` is
therefore necessary to test for `_NaN`. Return `true` if the given value is
`_NaN`, `false` otherwise. See [`isNaN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isNaN) on MDN.
*/
external isNaN: float => bool = "isNaN"

@val
@scope("Number")
/**
Tests if the given value is finite. Return `true` if the given value is a finite
number, `false` otherwise. See [`isFinite`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/isFinite) on MDN.

## Examples

```rescript
/* returns [false] */
Js.Float.isFinite(infinity)

/* returns [false] */
Js.Float.isFinite(neg_infinity)

/* returns [false] */
Js.Float.isFinite(Js.Float._NaN)

/* returns [true] */
Js.Float.isFinite(1234.)
```
*/
external isFinite: float => bool = "isFinite"

@send
/**
Formats a `float` using exponential (scientific) notation. Return a
`string` representing the given value in exponential notation. Raise
RangeError if digits is not in the range [0, 20] (inclusive). See [`toExponential`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential) on MDN.

## Examples

```rescript
/* prints "7.71234e+1" */
Js.Float.toExponential(77.1234)->Js.log

/* prints "7.7e+1" */
Js.Float.toExponential(77.)->Js.log
```
*/
external toExponential: float => string = "toExponential"

@send
/**
Formats a `float` using exponential (scientific) notation. `digits` specifies
how many digits should appear after the decimal point. The value must be in
the range [0, 20] (inclusive). Return a `string` representing the given value
in exponential notation. The output will be rounded or padded with zeroes if
necessary. Raise RangeError if `digits` is not in the range [0, 20] (inclusive).
See [`toExponential`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential) on MDN.

## Examples

```rescript
/* prints "7.71e+1" */
Js.Float.toExponentialWithPrecision(77.1234, ~digits=2)->Js.log
```
*/
external toExponentialWithPrecision: (float, ~digits: int) => string = "toExponential"

@send
/**
Formats a `float` using fixed point notation. Return a `string` representing the
given value in fixed-point notation (usually). Raise RangeError if digits is not
in the range [0, 20] (inclusive). See [`toFixed`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed) on MDN.

## Examples

```rescript
/* prints "12346" (note the rounding) */
Js.Float.toFixed(12345.6789)->Js.log

/* print "1.2e+21" */
Js.Float.toFixed(1.2e21)->Js.log
```
*/
external toFixed: float => string = "toFixed"

@send
/**
Formats a `float` using fixed point notation. `digits` specifies how many digits
should appear after the decimal point. The value must be in the range [0, 20]
(inclusive). Defaults to `0`. Return a `string` representing the given value in
fixed-point notation (usually). See [`toFixed`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed) on MDN.

The output will be rounded or padded with zeroes if necessary.

Raise RangeError if digits is not in the range [0, 20] (inclusive)

## Examples

```rescript
/* prints "12345.7" (note the rounding) */
Js.Float.toFixedWithPrecision(12345.6789, ~digits=1)->Js.log

/* prints "0.00" (note the added zeroes) */
Js.Float.toFixedWithPrecision(0., ~digits=2)->Js.log
```
*/
external toFixedWithPrecision: (float, ~digits: int) => string = "toFixed"

@send
/**
Formats a `float` using some fairly arbitrary rules. Return a `string`
representing the given value in fixed-point (usually). `toPrecision` differs
from `Js.Float.toFixed` in that the former will format the number with full
precision, while the latter will not output any digits after the decimal point.
See [`toPrecision`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision) on MDN.

Raise RangeError if digits is not in the range accepted by this function (what do you mean "vague"?)

## Examples

```rescript
/* prints "12345.6789" */
Js.Float.toPrecision(12345.6789)->Js.log

/* print "1.2e+21" */
Js.Float.toPrecision(1.2e21)->Js.log
```
*/
external toPrecision: float => string = "toPrecision"

/* equivalent to `toString` I think */

@send
/**
Formats a `float` using some fairly arbitrary rules. `digits` specifies how many
digits should appear in total. The value must between 0 and some arbitrary number
that's hopefully at least larger than 20 (for Node it's 21. Why? Who knows).
See [`toPrecision`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision) on MDN.

Return a `string` representing the given value in fixed-point or scientific
notation. The output will be rounded or padded with zeroes if necessary.

`toPrecisionWithPrecision` differs from `toFixedWithPrecision` in that the former
will count all digits against the precision, while the latter will count only
the digits after the decimal point. `toPrecisionWithPrecision` will also use
scientific notation if the specified precision is less than the number for digits
before the decimal point.

Raise RangeError if digits is not in the range accepted by this function (what do you mean "vague"?)

## Examples

```rescript
/* prints "1e+4" */
Js.Float.toPrecisionWithPrecision(12345.6789, ~digits=1)->Js.log

/* prints "0.0" */
Js.Float.toPrecisionWithPrecision(0., ~digits=2)->Js.log
```
*/
external toPrecisionWithPrecision: (float, ~digits: int) => string = "toPrecision"

@send
/**
Formats a `float` as a string. Return a `string` representing the given value in
fixed-point (usually). See [`toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) on MDN.

## Examples

```rescript
/* prints "12345.6789" */
Js.Float.toString(12345.6789)->Js.log
```
*/
external toString: float => string = "toString"

@send
/**
Formats a `float` as a string. `radix` specifies the radix base to use for the
formatted number. The value must be in the range [2, 36] (inclusive). Return a
`string` representing the given value in fixed-point (usually). See [`toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) on MDN.

Raise RangeError if radix is not in the range [2, 36] (inclusive)

## Examples

```rescript
/* prints "110" */
Js.Float.toStringWithRadix(6., ~radix=2)->Js.log

/* prints "11.001000111101011100001010001111010111000010100011111" */
Js.Float.toStringWithRadix(3.14, ~radix=2)->Js.log

/* prints "deadbeef" */
Js.Float.toStringWithRadix(3735928559., ~radix=16)->Js.log

/* prints "3f.gez4w97ry0a18ymf6qadcxr" */
Js.Float.toStringWithRadix(123.456, ~radix=36)->Js.log
```
*/
external toStringWithRadix: (float, ~radix: int) => string = "toString"

@val
/**
Parses the given `string` into a `float` using JavaScript semantics. Return the
number as a `float` if successfully parsed, `_NaN` otherwise.

## Examples

```rescript
/* returns 123 */
Js.Float.fromString("123")

/* returns 12.3 */
Js.Float.fromString("12.3")

/* returns 0 */
Js.Float.fromString("")

/* returns 17 */
Js.Float.fromString("0x11")

/* returns 3 */
Js.Float.fromString("0b11")

/* returns 9 */
Js.Float.fromString("0o11")

/* returns [_NaN] */
Js.Float.fromString("hello")

/* returns [_NaN] */
Js.Float.fromString("100a")
```
*/
external fromString: string => float = "Number"

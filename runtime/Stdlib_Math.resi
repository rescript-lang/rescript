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
Functions for interacting with JavaScript Math.
See: [`Math`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math).
*/

/**
Mathematical Constants
*/
module Constants: {
  /**
  `Math.Constants.e` returns Euler's number, ≈ 2.718281828459045.
  See [`Math.E`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/E) on MDN.

  ## Examples

  ```rescript
  Math.Constants.e
  ```
  */
  @val
  external e: float = "Math.E"

  /**
  `Math.Constants.ln2` returns Natural logarithm of 2, ≈ 0.6931471805599453.
  See [`Math.LN2`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/LN2) on MDN.

  ## Examples

  ```rescript
  Math.Constants.ln2
  ```
  */
  @val
  external ln2: float = "Math.LN2"

  /**
  `Math.Constants.ln10` returns Natural logarithm of 10, ≈ 2.302585092994046.
  See [`Math.LN10`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/LN10) on MDN.

  ## Examples

  ```rescript
  Math.Constants.ln10
  ```
  */
  @val
  external ln10: float = "Math.LN10"

  /**
  `Math.Constants.log2e` returns Base 2 logarithm of E, ≈ 1.4426950408889634.
  See [`Math.LOG2E`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/LOG2E) on MDN.

  ## Examples

  ```rescript
  Math.Constants.log2e
  ```
  */
  @val
  external log2e: float = "Math.LOG2E"

  /**
  `Math.Constants.log10e` returns Base 10 logarithm of E, ≈ 0.4342944819032518.
  See [`Math.LOG10E`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/LOG10E) on MDN.

  ## Examples

  ```rescript
  Math.Constants.log10e
  ```
  */
  @val
  external log10e: float = "Math.LOG10E"
  /**
  `Math.Constants.pi` returns Pi - ratio of the circumference to the diameter
  of a circle, ≈ 3.141592653589793.
  See [`Math.PI`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/PI) on MDN.

  ## Examples

  ```rescript
  Math.Constants.pi
  ```
  */
  @val
  external pi: float = "Math.PI"
  /**
  `Math.Constants.sqrt1_2` returns Square root of 1/2, ≈ 0.7071067811865476.
  See [`Math.SQRT1_2`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/SQRT1_2) on MDN.

  ## Examples

  ```rescript
  Math.Constants.sqrt1_2
  ```
  */
  @val
  external sqrt1_2: float = "Math.SQRT1_2"
  /**
  `Math.Constants.e` returns Absolute value for integer argument.
  See [`Math.abs`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/abs) on MDN.

  ## Examples

  ```rescript
  Math.Constants.sqrt2
  ```
  */
  @val
  external sqrt2: float = "Math.SQRT2"
}

/**
Provide Math utilities for `int`
*/
module Int: {
  /**
  `abs(v)` returns absolute value of `v`.
  See [`Math.abs`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/abs) on MDN.

  ## Examples

  ```rescript
  Math.Int.abs(-2) // 2
  Math.Int.abs(3) // 3
  ```
  */
  @val
  external abs: int => int = "Math.abs"

  /**
  `clz32(v)` returns the number of leading zero bits of the argument's 32 bit
  int representation.
  See [`Math.clz32`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/clz32) on MDN.

  ## Examples

  ```rescript
  // 00000000000000000000000000000001
  Math.Int.clz32(1) // 31
  // 00000000000000000000000000000100
  Math.Int.clz32(4) // 29
  ```
  */
  @val
  external clz32: int => int = "Math.clz32"

  /**
  `imul(a, b)` returns 32-bit integer multiplication. Use this only when you
  need to optimize performance of multiplication of numbers stored as 32-bit
  integers.
  See [`Math.imul`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/imul) on MDN.

  ## Examples

  ```rescript
  Math.Int.imul(3, 4) // 12
  Math.Int.imul(-5, 12) // 60
  ```
  */
  @val
  external imul: (int, int) => int = "Math.imul"

  /**
  `min(a, b)` returns the minimum of its two integer arguments.
  See [`Math.min`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/min) on MDN.

  ## Examples

  ```rescript
  Math.Int.min(1, 2) // 1
  Math.Int.min(-1, -2) // -2
  ```
  */
  @val
  external min: (int, int) => int = "Math.min"

  /**
  `minMany(arr)` returns the minimum of the integers in the given array `arr`.
  Returns `Infinity` if `arr` is empty.
  See [`Math.min`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/min) on MDN.

  ## Examples

  ```rescript
  Math.Int.minMany([1, 2]) // 1
  Math.Int.minMany([-1, -2]) // -2
  Math.Int.minMany([])->Int.toFloat->Float.isFinite // false
  ```
  */
  @variadic
  @val
  external minMany: array<int> => int = "Math.min"

  /**
  `max(a, b)` returns the maximum of its two integer arguments.
  See [`Math.max`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/max) on MDN.

  ## Examples

  ```rescript
  Math.Int.max(1, 2) // 2
  Math.Int.max(-1, -2) // -1
  ```
  */
  @val
  external max: (int, int) => int = "Math.max"

  /**
  `maxMany(arr)` returns the maximum of the integers in the given array `arr`.
  Returns `Infinity` if `arr` is empty.
  See [`Math.max`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/max) on MDN.

  ## Examples

  ```rescript
  Math.Int.maxMany([1, 2]) // 2
  Math.Int.maxMany([-1, -2]) // -1
  Math.Int.maxMany([])->Int.toFloat->Float.isFinite // false
  ```
  */
  @variadic
  @val
  external maxMany: array<int> => int = "Math.max"

  /**
  `pow(a, ~exp)` raises the given base `a` to the given exponent `exp`.
  See [`Math.pow`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/pow) on MDN.

  ## Examples

  ```rescript
  Math.Int.pow(2, ~exp=4) // 16
  Math.Int.pow(3, ~exp=4) // 81
  ```
  */
  @val
  external pow: (int, ~exp: int) => int = "Math.pow"

  /**
  `sign(v)` returns the sign of its integer argument: `-1` if negative, `0` if
  zero, `1` if positive.
  See [`Math.sign`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sign) on MDN.

  ## Examples

  ```rescript
  Math.Int.sign(3) // 1
  Math.Int.sign(-3) // -1
  Math.Int.sign(0) // 0
  ```
  */
  @val
  external sign: int => int = "Math.sign"

  /**
  floor(v) returns the largest `int` less than or equal to the argument; 
  See [`Math.floor`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
  on MDN.

  ## Examples

  ```rescript
  Math.Int.floor(3.7) == 3
  Math.Int.floor(3.0) == 3
  Math.Int.floor(-3.1) == -4
  ```
  */
  let floor: float => int

  /**
  ceil(v) returns the smallest `int` greater than or equal to the argument;
  See [`Math.floor`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
  on MDN.

  ## Examples

  ```rescript
  Math.Int.ceil(3.7) == 4
  Math.Int.ceil(3.0) == 3
  Math.Int.ceil(-3.1) == -3
  ```
  */
  let ceil: float => int

  /**
  `random(minVal, maxVal)` returns a random integer number in the half-closed interval [minVal, maxVal).
   See [`Math.random`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
  on MDN.

  ## Examples

  ```rescript
  Math.Int.random(2, 5) == 4
  Math.Int.random(505, 2000) == 1276
  Math.Int.random(-7, -2) == -4
  ```
  */
  let random: (int, int) => int
}

/**
`abs(v)` returns absolute value of `v`.
See [`Math.abs`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/abs) on MDN.

## Examples

```rescript
Math.abs(-2.0) // 2.0
Math.abs(3.0) // 3.0
```
  */
@val
external abs: float => float = "Math.abs"

/**
`acos(v)` returns arccosine (in radians) of argument `v`, returns `NaN` if the
argument is outside the range [-1.0, 1.0].
See [`Math.acos`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/acos) on MDN.

## Examples

```rescript
Math.acos(-1.0) // 3.141592653589793
Math.acos(-3.0)->Float.isNaN // true
```
*/
@val
external acos: float => float = "Math.acos"

/**
`acosh(v)` returns the inverse hyperbolic arccosine (in radians) of argument `v`,
returns `NaN` if the argument is less than `1.0`.
See [`Math.acosh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/acosh) on MDN.

## Examples

```rescript
Math.acosh(1.0) // 0.0
Math.acosh(0.5)->Float.isNaN // true
```
*/
@val
external acosh: float => float = "Math.acosh"

/**
`asin(v)` returns the inverse sine (in radians) of argument `v`, returns `NaN`
if the argument `v` is outside the range [-1.0, 1.0].
See [`Math.asin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/asin) on MDN.

## Examples

```rescript
Math.asin(-1.0) // -1.5707963267948966
Math.asin(-2.0)->Float.isNaN // true
```
*/
@val
external asin: float => float = "Math.asin"

/**
`asinh(v)` returns the inverse hyperbolic sine of argument `v`.
See [`Math.asinh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/asinh) on MDN.

## Examples

```rescript
Math.asinh(-1.0) // -0.881373587019543
Math.asinh(-0.0) // -0.0
```
*/
@val
external asinh: float => float = "Math.asinh"

/**
`atan(v)` returns the inverse tangent (in radians) of argument `v`.
See [`Math.atan`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/atan) on MDN.

## Examples

```rescript
Math.atan(-0.0) // -0.0
Math.atan(0.0) // 0.0
Math.atan(1.0) // 0.7853981633974483
```
*/
@val
external atan: float => float = "Math.atan"

/**
`atanh(v)` returns the invert hyperbolic tangent of argument `v`. Returns `NaN`
if the argument `v` is is outside the range [-1.0, 1.0] and `Infinity` if `v`
is `-1.0` or `1.0`.
See [`Math.atanh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/atanh) on MDN.

## Examples

```rescript
Math.atanh(-2.0)->Float.isNaN // true
Math.atanh(-1.0)->Float.isFinite // false
Math.atanh(-0.0) // -0.0
Math.atanh(0.0) // 0.0
Math.atanh(0.5) // 0.5493061443340548
```
*/
@val
external atanh: float => float = "Math.atanh"

/**
`atan2(~y, ~x)` returns the angle (in radians) of the quotient `y /. x`. It is
also the angle between the *x*-axis and point (*x*, *y*).
See [`Math.atan2`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/atan2) on MDN.

## Examples

```rescript
Math.atan2(~y=0.0, ~x=10.0) == 0.0
Math.atan2(~x=5.0, ~y=5.0) == Math.Constants.pi /. 4.0
Math.atan2(~x=90.0, ~y=15.0) // 1.4056476493802699
Math.atan2(~x=15.0, ~y=90.0) // 0.16514867741462683
```
*/
@val
external atan2: (~y: float, ~x: float) => float = "Math.atan2"

/**
`cbrt(v)` returns the cube root of argument `v`.
See [`Math.cbrt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cbrt) on MDN.

## Examples

```rescript
Math.cbrt(-1.0) // -1.0
Math.cbrt(-0.0) // -0.0
Math.cbrt(0.0) // 0.0
```
*/
@val
external cbrt: float => float = "Math.cbrt"

/**
`ceil(v)` returns the smallest integral value greater than or equal to the
argument `v`. The result is a `float` and is not restricted to the `int` data
type range.
See [`Math.ceil`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil) on MDN.

## Examples

```rescript
Math.ceil(3.1) == 4.0
Math.ceil(3.0) == 3.0
Math.ceil(-3.1) == -3.0
Math.ceil(2_150_000_000.3) == 2_150_000_001.0
```
*/
@val
external ceil: float => float = "Math.ceil"

/**
`cos(v)` returns the cosine of argument `v`, which must be specified in radians.
See [`Math.cos`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cos) on MDN.

## Examples

```rescript
Math.cos(-0.0) // 1.0
Math.cos(0.0) // 1.0
Math.cos(1.0) // 0.5403023058681398
```
*/
@val
external cos: float => float = "Math.cos"

/**
`cosh(v)` returns the hyperbolic cosine of argument `v`, which must be specified
in radians.
See [`Math.cosh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/cosh) on MDN.

## Examples

```rescript
Math.cosh(-1.0) // 1.5430806348152437
Math.cosh(-0.0) // 1.0
Math.cosh(0.0) // 1.0
```
*/
@val
external cosh: float => float = "Math.cosh"

/**
`exp(v)` returns natural exponentional, returns *e* (the base of natural logarithms)
to the power of the given argument `v`.
See [`Math.exp`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/exp) on MDN.

## Examples

```rescript
Math.exp(-1.0) // 0.36787944117144233
Math.exp(0.0) // 1.0
```
*/
@val
external exp: float => float = "Math.exp"

/**
`expm1(v)` returns *e* (the base of natural logarithms) to the power of the given
argument `v` minus 1.
See [`Math.expm1`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/expm1) on MDN.

## Examples

```rescript
Math.expm1(-1.0) // -0.6321205588285577
Math.expm1(-0.0) // -0
```
*/
@val
external expm1: float => float = "Math.expm1"

/**
`floor(v)` returns the largest integral value less than or equal to the argument
`v`. The result is a `float` and is not restricted to the `int` data type range.
See [`Math.floor`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor) on MDN.

## Examples

```rescript
Math.floor(-45.95) // -46.0
Math.floor(-45.05) // -46.0
Math.floor(-0.0) // -0.0
```
*/
@val
external floor: float => float = "Math.floor"

/**
`fround(v)` returns the nearest single precision float.
See [`Math.fround`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/fround) on MDN.

## Examples

```rescript
Math.fround(5.5) == 5.5
Math.fround(5.05) == 5.050000190734863
```
*/
@val
external fround: float => float = "Math.fround"

/**
`hypot(a, b)` returns the square root of the sum of squares of its two arguments
(the Pythagorean formula).
See [`Math.hypot`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/hypot) on MDN.

## Examples

```rescript
Math.hypot(3.0, 4.0) // 5.0
Math.hypot(3.0, 5.0) // 5.8309518948453
```
*/
@val
external hypot: (float, float) => float = "Math.hypot"

/**
`hypotMany(arr)` returns the square root of the sum of squares of the numbers in
the array argument (generalized Pythagorean equation). Using an array allows you
to have more than two items. If `arr` is an empty array then returns `0.0`.
See [`Math.hypot`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/hypot) on MDN.

## Examples

```rescript
Math.hypotMany([3.0, 4.0, 5.0]) // 7.0710678118654755
Math.hypotMany([]) // 0.0
```
*/
@variadic
@val
external hypotMany: array<float> => float = "Math.hypot"

/**
`log(v)` returns the natural logarithm of argument `v`, this is the number *x*
such that `e^x` equals the argument. Returns `NaN` for negative arguments and
`Infinity` for `0.0` or `-0.0`.
See [`Math.log`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log) on MDN.

## Examples

```rescript
Math.log(-1.0)->Float.isNaN // true
Math.log(-0.0)->Float.isFinite // false
Math.log(0.0)->Float.isFinite // false
Math.log(1.0) // 0
```
*/
@val
external log: float => float = "Math.log"

/**
`log1p(v)` returns the natural logarithm of one plus the argument `v`.
Returns `NaN` for arguments less than `-1` and `Infinity` if `v` is `-1.0`.
See [`Math.log1p`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log1p) on MDN.

## Examples

```rescript
Math.log1p(-2.0)->Float.isNaN // true
Math.log1p(-1.0)->Float.isFinite // false
Math.log1p(-0.0) // -0
```
*/
@val
external log1p: float => float = "Math.log1p"

/**
`log10(v)` returns the base 10 logarithm of argument `v`. Returns `NaN` for
negative `v`. If `v` is `-0.0` or `0.0` returns `Infinity`.
See [`Math.log10`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log10) on MDN.

## Examples

```rescript
Math.log10(-2.0)->Float.isNaN // true
Math.log10(-0.0)->Float.isFinite // false
Math.log10(0.0)->Float.isFinite // false
Math.log10(1.0) // 0
```
*/
@val
external log10: float => float = "Math.log10"

/**
`log2(v)` returns the base 2 logarithm of argument `v`. Returns `NaN` for
negative `v` and `Infinity` if `v` is `-0.0` or `0.0`.
See [`Math.log2`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/log2) on MDN.

## Examples

```rescript
Math.log2(-2.0)->Float.isNaN // true
Math.log2(-0.0)->Float.isFinite // false
Math.log2(0.0)->Float.isFinite // false
Math.log2(1.0) // 0.0
```
*/
@val
external log2: float => float = "Math.log2"

/**
`min(a, b)` returns the minimum of its two float arguments.
See [`Math.min`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/min) on MDN.

## Examples

```rescript
Math.min(1.0, 2.0) // 1.0
Math.min(-1.0, -2.0) // -2.0
```
*/
@val
external min: (float, float) => float = "Math.min"

/**
`minMany(arr)` returns the minimum of the float in the given array `arr`.
Returns `Infinity` if `arr` is empty.
See [`Math.min`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/min) on MDN.

## Examples

```rescript
Math.minMany([1.0, 2.0]) // 1.0
Math.minMany([-1.0, -2.0]) // -2.0
Math.minMany([])->Float.isFinite // false
```
*/
@variadic
@val
external minMany: array<float> => float = "Math.min"

/**
`max(a, b)` returns the maximum of its two float arguments.
See [`Math.max`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/max) on MDN.

## Examples

```rescript
Math.max(1.0, 2.0) // 2.0
Math.max(-1.0, -2.0) // -1.0
```
*/
@val
external max: (float, float) => float = "Math.max"

/**
`maxMany(arr)` returns the maximum of the float in the given array `arr`.
Returns `Infinity` if `arr` is empty.
See [`Math.max`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/max) on MDN.

## Examples

```rescript
Math.maxMany([1.0, 2.0]) // 2.0
Math.maxMany([-1.0, -2.0]) // -1.0
Math.maxMany([])->Float.isFinite // false
```
*/
@variadic
@val
external maxMany: array<float> => float = "Math.max"

/**
`pow(a, ~exp)` raises the given base `a` to the given exponent `exp`.
See [`Math.pow`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/pow) on MDN.

## Examples

```rescript
Math.pow(2.0, ~exp=4.0) // 16.0
Math.pow(3.0, ~exp=4.0) // 81.0
```
*/
@val
external pow: (float, ~exp: float) => float = "Math.pow"

/**
`random()` returns a random number in the half-closed interval [0,1].
See [`Math.random`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random) on MDN.

## Examples

```rescript
Math.random()
```
*/
@val
external random: unit => float = "Math.random"

/**
`round(v)` returns then value of `v` rounded to nearest integral value
(expressed as a float). If the fractional portion of the argument `v` is greater
than `0.5`, the argument `v` is rounded to the float with the next higher
absolute value.
See [`Math.round`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round) on MDN.

## Examples

```rescript
Math.round(-20.5) // -20.0
Math.round(-0.1) // -0.0
Math.round(0.0) // 0.0
Math.round(-0.0) // -0.0
```
*/
@val
external round: float => float = "Math.round"

/**
`sign(v)` returns the sign of its foat argument: `-1` if negative, `0` if
zero, `1` if positive.
See [`Math.sign`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sign) on MDN.

## Examples

```rescript
Math.sign(3.0) // 1.0
Math.sign(-3.0) // 1.0
Math.sign(0.0) // 0.0
```
*/
@val
external sign: float => float = "Math.sign"

/**
`sin(v)` returns the sine of argument `v`, which must be specified in radians.
See [`Math.sin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sin) on MDN.

## Examples

```rescript
Math.sin(-0.0) // -0.0
Math.sin(0.0) // 0.0
Math.sin(1.0) // 0.8414709848078965
```
*/
@val
external sin: float => float = "Math.sin"

/**
`sinh(v)` returns then hyperbolic sine of argument `v`, which must be specified
in radians.
See [`Math.sinh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sinh) on MDN.

## Examples

```rescript
Math.sinh(-0.0) // -0.0
Math.sinh(0.0) // 0.0
Math.sinh(1.0) // 1.1752011936438014
```
*/
@val
external sinh: float => float = "Math.sinh"

/**
`sqrt(v)` returns the square root of `v`. If `v` is negative returns `NaN`.
See [`Math.sqrt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/sqrt) on MDN.

## Examples

```rescript
Math.sqrt(-1.0)->Float.isNaN // true
Math.sqrt(-0.0) // -0.0
Math.sqrt(0.0) // 0.0
Math.sqrt(1.0) // 1.0
Math.sqrt(9.0) // 3.0
```
*/
@val
external sqrt: float => float = "Math.sqrt"

/**
`tan(v)` returns the tangent of argument `v`, which must be specified in
radians. Returns `NaN` if `v` is positive `Infinity` or negative `Infinity`.
See [`Math.tan`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/tan) on MDN.

## Examples

```rescript
Math.tan(-0.0) // -0.0
Math.tan(0.0) // 0.0
Math.tan(1.0) // 1.5574077246549023
```
*/
@val
external tan: float => float = "Math.tan"

/**
`tanh(v)` returns the hyperbolic tangent of argument `v`, which must be
specified in radians.
See [`Math.tanh`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/tanh) on MDN.

## Examples

```rescript
Math.tanh(-0.0) // -0.0
Math.tanh(0.0) // 0.0
Math.tanh(1.0) // 0.7615941559557649
```
*/
@val
external tanh: float => float = "Math.tanh"

/**
`trunc(v)` truncates the argument `v`, i.e., removes fractional digits.
See [`Math.trunc`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/trunc) on MDN.

## Examples

```rescript
Math.trunc(0.123) // 0.0
Math.trunc(1.999) // 1.0
Math.trunc(13.37) // 13.0
Math.trunc(42.84) // 42.0
```
*/
@val
external trunc: float => float = "Math.trunc"

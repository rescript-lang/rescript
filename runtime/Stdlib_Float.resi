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
Functions for interacting with float.
*/

/**
Type representing a float.
*/
type t = float

/**
Float constants.
*/
module Constants: {
  /**
  The special value "Not a Number"
  See [`NaN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN) on MDN.

  ## Examples

  ```rescript
  Float.Constants.nan
  ```
  */
  @val
  external nan: float = "NaN"

  /**
  Represents the difference between 1 and the smallest floating point number greater than 1.
  See [`Number.EPSILON`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/EPSILON) on MDN.

  ## Examples

  ```rescript
  Float.Constants.epsilon
  ```
  */
  @val
  external epsilon: float = "Number.EPSILON"

  /**
  The positive Infinity value
  See [`Number.POSITIVE_INFINITY`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/POSITIVE_INFINITY) on MDN.

  ## Examples

  ```rescript
  Float.Constants.positiveInfinity
  ```
  */
  @val
  external positiveInfinity: float = "Number.POSITIVE_INFINITY"

  /**
  The negative Infinity value
  See [`Number.NEGATIVE_INFINITY`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/NEGATIVE_INFINITY) on MDN.

  ## Examples

  ```rescript
  Float.Constants.negativeInfinity
  ```
  */
  @val
  external negativeInfinity: float = "Number.NEGATIVE_INFINITY"

  /**
  The smallest positive numeric value representable in JavaScript.
  See [`Number.MIN_VALUE`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MIN_VALUE) on MDN.

  ## Examples

  ```rescript
  Float.Constants.minValue
  ```
  */
  @val
  external minValue: float = "Number.MIN_VALUE"

  /**
  The maximum positive numeric value representable in JavaScript.
  See [`Number.MAX_VALUE`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_VALUE) on MDN.

  ## Examples

  ```rescript
  Float.Constants.minValue
  ```
  */
  @val
  external maxValue: float = "Number.MAX_VALUE"
}

external equal: (float, float) => bool = "%equal"

external compare: (float, float) => Stdlib_Ordering.t = "%compare"

/**
`isNaN(v)` tests if the given `v` is `NaN`.
See [`NaN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN) on MDN.

## Examples

```rescript
Float.isNaN(3.0) // false
Float.isNaN(Float.Constants.nan) // true
```
*/
@val
external isNaN: float => bool = "isNaN"

/**
`isFinite(v)` tests if the given `v` is finite.
See [`isFinite`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/isFinite) on MDN.

## Examples

```rescript
Float.isFinite(1.0) // true
Float.isFinite(Float.Constants.nan) // false
Float.isFinite(Float.Constants.positiveInfinity) // false
```
*/
@val
external isFinite: float => bool = "isFinite"

/**
`parseFloat(v)` parse the given `v` and returns a float. Leading whitespace in
`v` is ignored. Returns `NaN` if `v` can't be parsed. Use [`fromString`] to
ensure it returns a valid float and not `NaN`.
See [`parseFloat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseFloat) on MDN.

## Examples

```rescript
Float.parseFloat("1.0") // 1.0
Float.parseFloat("  3.14   ") // 3.14
Float.parseFloat("3.0") // 3.0
Float.parseFloat("3.14some non-digit characters") // 3.14
Float.parseFloat("error")->Float.isNaN // true
```
*/
@val
external parseFloat: string => float = "parseFloat"

/**
`parseInt(v, ~radix=?)` parse the given `v` and returns a float. Leading
whitespace in this argument `v`is ignored. `radix` specifies the radix base to
use for the formatted number. The value must be in the range [2, 36] (inclusive).
Returns `NaN` if `v` can't be parsed and `radix` is smaller than 2 or bigger
than 36.
See [`parseInt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseInt) on MDN.

## Examples

```rescript
Float.parseInt("1.0") // 1.0
Float.parseInt("  3.14   ") // 3.0
Float.parseInt(3) // 3.0
Float.parseInt("3.14some non-digit characters") // 3.0
Float.parseInt("error")->Float.isNaN // true
Float.parseInt("10.0", ~radix=2) // 2.0
Float.parseInt("15 * 3", ~radix=10) // 15.0
Float.parseInt("12", ~radix=13) // 15.0
Float.parseInt("17", ~radix=40)->Float.isNaN // true
```
*/
@val
external parseInt: ('a, ~radix: int=?) => float = "parseInt"

/**
`parseIntWithRadix(v, ~radix)` parse the given `v` and returns a float. Leading
whitespace in this argument `v`is ignored. `radix` specifies the radix base to
use for the formatted number. The value must be in the range [2, 36] (inclusive).
Returns `NaN` if `v` can't be parsed and `radix` is smaller than 2 or bigger
than 36.
See [`parseInt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseInt) on MDN.

## Examples

```rescript
Float.parseIntWithRadix("10.0", ~radix=2) // 2.0
Float.parseIntWithRadix("15 * 3", ~radix=10) // 15.0
Float.parseIntWithRadix("12", ~radix=13) // 15.0
Float.parseIntWithRadix("17", ~radix=40)->Float.isNaN // true
```
*/
@deprecated("Use `parseInt` instead")
@val
external parseIntWithRadix: ('a, ~radix: int) => float = "parseInt"

/**
`toExponential(v, ~digits=?)` return a `string` representing the given value in
exponential notation. `digits` specifies how many digits should appear after
the decimal point.
See [`Number.toExponential`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential) on MDN.

## Examples

```rescript
Float.toExponential(1000.0) // "1e+3"
Float.toExponential(-1000.0) // "-1e+3"
Float.toExponential(77.0, ~digits=2) // "7.70e+1"
Float.toExponential(5678.0, ~digits=2) // "5.68e+3"
```

## Exceptions

- `RangeError`: If `digits` less than 0 or greater than 10.
*/
@send
external toExponential: (float, ~digits: int=?) => string = "toExponential"

/**
`toExponential(v, ~digits)` return a `string` representing the given value in
exponential notation. `digits` specifies how many digits should appear after
the decimal point.
See [`Number.toExponential`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential) on MDN.

## Examples

```rescript
Float.toExponentialWithPrecision(77.0, ~digits=2) // "7.70e+1"
Float.toExponentialWithPrecision(5678.0, ~digits=2) // "5.68e+3"
```

## Exceptions

- `RangeError`: If `digits` less than 0 or greater than 10.
*/
@deprecated("Use `toExponential` instead")
@send
external toExponentialWithPrecision: (float, ~digits: int) => string = "toExponential"

/**
`toFixed(v, ~digits=?)` return a `string` representing the given
value using fixed-point notation. `digits` specifies how many digits should
appear after the decimal point.
See [`Number.toFixed`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed) on MDN.

## Examples

```rescript
Float.toFixed(123456.0) // "123456.00"
Float.toFixed(10.0) // "10.00"
Float.toFixed(300.0, ~digits=4) // "300.0000"
Float.toFixed(300.0, ~digits=1) // "300.0"
```

## Exceptions

- `RangeError`: If `digits` is less than 0 or larger than 100.
*/
@send
external toFixed: (float, ~digits: int=?) => string = "toFixed"

/**
`toFixedWithPrecision(v, ~digits)` return a `string` representing the given
value using fixed-point notation. `digits` specifies how many digits should
appear after the decimal point.
See [`Number.toFixed`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed) on MDN.

## Examples

```rescript
Float.toFixedWithPrecision(300.0, ~digits=4) // "300.0000"
Float.toFixedWithPrecision(300.0, ~digits=1) // "300.0"
```

## Exceptions

- `RangeError`: If `digits` is less than 0 or larger than 100.
*/
@deprecated("Use `toFixed` instead")
@send
external toFixedWithPrecision: (float, ~digits: int) => string = "toFixed"

/**
`toPrecision(v, ~digits=?)` return a `string` representing the giver value with
precision. `digits` specifies the number of significant digits.
See [`Number.toPrecision`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision) on MDN.

## Examples

```rescript
Float.toPrecision(100.0) // "100"
Float.toPrecision(1.0) // "1"
Float.toPrecision(100.0, ~digits=2) // "1.0e+2"
Float.toPrecision(1.0, ~digits=1) // "1"
```

## Exceptions

- `RangeError`: If `digits` is not between 1 and 100 (inclusive).
Implementations are allowed to support larger and smaller values as well.
ECMA-262 only requires a precision of up to 21 significant digits.
*/
@send
external toPrecision: (float, ~digits: int=?) => string = "toPrecision"

/**
`toPrecisionWithPrecision(v, ~digits)` return a `string` representing the giver value with
precision. `digits` specifies the number of significant digits.
See [`Number.toPrecision`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision) on MDN.

## Examples

```rescript
Float.toPrecisionWithPrecision(100.0, ~digits=2) // "1.0e+2"
Float.toPrecisionWithPrecision(1.0, ~digits=1) // "1"
```

## Exceptions

- `RangeError`: If `digits` is not between 1 and 100 (inclusive).
Implementations are allowed to support larger and smaller values as well.
ECMA-262 only requires a precision of up to 21 significant digits.

*/
@deprecated("Use `toPrecision` instead")
@send
external toPrecisionWithPrecision: (float, ~digits: int) => string = "toPrecision"

/**
`toString(v)` return a `string` representing the given value.
See [`Number.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) on MDN.

## Examples

```rescript
Float.toString(1000.0) // "1000"
Float.toString(-1000.0) // "-1000"
```
*/
@send
external toString: (float, ~radix: int=?) => string = "toString"

/**
`toStringWithRadix(v, ~radix)` return a `string` representing the given value.
`~radix` specifies the radix base to use for the formatted number.
See [`Number.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString) on MDN.

## Examples

```rescript
Float.toStringWithRadix(6.0, ~radix=2) // "110"
Float.toStringWithRadix(3735928559.0, ~radix=16) // "deadbeef"
Float.toStringWithRadix(123456.0, ~radix=36) // "2n9c"
```

## Exceptions

`RangeError`: if `radix` is less than 2 or greater than 36.
*/
@deprecated("Use `toString` with `~radix` instead")
@send
external toStringWithRadix: (float, ~radix: int) => string = "toString"

/**
`toLocaleString(v)` return a `string` with language-sensitive representing the
given value.
See [`Number.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toLocaleString) on MDN.

## Examples

```rescript
// If the application uses English as the default language
Float.toLocaleString(1000.0) // "1,000"

// If the application uses Portuguese Brazil as the default language
Float.toLocaleString(1000.0) // "1.000"
```
*/
@send
external toLocaleString: float => string = "toLocaleString"

/**
`fromString(str)` return an `option<int>` representing the given value `str`.

## Examples

```rescript
Float.fromString("0") == Some(0.0)
Float.fromString("NaN") == None
Float.fromString("6") == Some(6.0)
```
*/
let fromString: string => option<float>

/**
`toInt(v)` returns an int to given float `v`.

## Examples

```rescript
Float.toInt(2.0) == 2
Float.toInt(1.0) == 1
Float.toInt(1.1) == 1
Float.toInt(1.6) == 1
```
*/
external toInt: float => int = "%intoffloat"

/**
`fromInt(v)` returns a float to given int `v`.

## Examples

```rescript
Float.fromInt(2) == 2.0
Float.fromInt(1) == 1.0
```
*/
external fromInt: int => float = "%identity"

/**
`mod(n1, n2)` calculates the modulo (remainder after division) of two floats.

## Examples

```rescript
Float.mod(7.0, 4.0) == 3.0
```
*/
external mod: (float, float) => float = "%modfloat"

/**
`clamp(~min=?, ~max=?, value)` returns `value`, optionally bounded by `min` and `max`.

if `max` < `min` returns `min`.

## Examples

```rescript
Float.clamp(4.2) == 4.2
Float.clamp(4.2, ~min=4.3) == 4.3
Float.clamp(4.2, ~max=4.1) == 4.1
Float.clamp(4.2, ~min=4.3, ~max=4.1) == 4.3
```
*/
let clamp: (~min: float=?, ~max: float=?, float) => float

/**
  `ignore(float)` ignores the provided float and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: float => unit = "%ignore"

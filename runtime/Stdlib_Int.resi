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
Functions for interacting with JavaScript Number.
See: [`Number`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number).
*/

/**
Type representing an int.
*/
type t = int

module Constants: {
  /**
  The smallest positive number represented in JavaScript.
  See [`Number.MIN_VALUE`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MIN_VALUE)
  on MDN.

  ## Examples

  ```rescript
  Console.log(Int.Constants.minValue)
  ```
  */
  @inline
  let minValue: int
  /**
  The largest positive number represented in JavaScript.
  See [`Number.MAX_VALUE`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/MAX_VALUE)
  on MDN.

  ## Examples

  ```rescript
  Console.log(Int.Constants.maxValue)
  ```
  */
  @inline
  let maxValue: int
}

external equal: (int, int) => bool = "%equal"

external compare: (int, int) => Stdlib_Ordering.t = "%compare"

/**
`toExponential(n, ~digits=?)` return a `string` representing the given value in
exponential notation. `digits` specifies how many digits should appear after
the decimal point. See [`Number.toExponential`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential)

## Examples

```rescript
Int.toExponential(1000) // "1e+3"
Int.toExponential(-1000) // "-1e+3"
Int.toExponential(77, ~digits=2) // "7.70e+1"
Int.toExponential(5678, ~digits=2) // "5.68e+3"
```

## Exceptions

- `RangeError`: If `digits` less than 0 or greater than 10.
*/
@send
external toExponential: (int, ~digits: int=?) => string = "toExponential"

/**
`toExponential(n, ~digits)` return a `string` representing the given value in
exponential notation. `digits` specifies how many digits should appear after
the decimal point. See [`Number.toExponential`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toExponential)
on MDN.

## Examples

```rescript
Int.toExponentialWithPrecision(77, ~digits=2) // "7.70e+1"
Int.toExponentialWithPrecision(5678, ~digits=2) // "5.68e+3"
```

## Exceptions

- `RangeError`: If `digits` less than 0 or greater than 10.
*/
@deprecated("Use `toExponential` instead")
@send
external toExponentialWithPrecision: (int, ~digits: int) => string = "toExponential"

/**
`toFixed(n, ~digits=?)` return a `string` representing the given
value using fixed-point notation. `digits` specifies how many digits should
appear after the decimal point. See [`Number.toFixed`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
on MDN.

## Examples

```rescript
Int.toFixed(123456) // "123456.00"
Int.toFixed(10) // "10.00"
Int.toFixed(300, ~digits=4) // "300.0000"
Int.toFixed(300, ~digits=1) // "300.0"
```

## Exceptions

- `RangeError`: If `digits` is less than 0 or larger than 100.
*/
@send
external toFixed: (int, ~digits: int=?) => string = "toFixed"

/**
`toFixedWithPrecision(n, ~digits)` return a `string` representing the given
value using fixed-point notation. `digits` specifies how many digits should
appear after the decimal point. See [`Number.toFixed`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
on MDN.

## Examples

```rescript
Int.toFixedWithPrecision(300, ~digits=4) // "300.0000"
Int.toFixedWithPrecision(300, ~digits=1) // "300.0"
```

## Exceptions

- `RangeError`: If `digits` is less than 0 or larger than 100.
*/
@deprecated("Use `toFixed` instead")
@send
external toFixedWithPrecision: (int, ~digits: int) => string = "toFixed"

/**
`toPrecision(n, ~digits=?)` return a `string` representing the giver value with
precision. `digits` specifies the number of significant digits. See [`Number.toPrecision`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision) on MDN.

## Examples

```rescript
Int.toPrecision(100) // "100"
Int.toPrecision(1) // "1"
Int.toPrecision(100, ~digits=2) // "1.0e+2"
Int.toPrecision(1, ~digits=2) // "1.0"
```

## Exceptions

- `RangeError`: If `digits` is not between 1 and 100 (inclusive).
Implementations are allowed to support larger and smaller values as well.
ECMA-262 only requires a precision of up to 21 significant digits.
*/
@send
external toPrecision: (int, ~digits: int=?) => string = "toPrecision"

/**
`toPrecisionWithPrecision(n, ~digits)` return a `string` representing the giver value with
precision. `digits` specifies the number of significant digits. See [`Number.toPrecision`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toPrecision) on MDN.

## Examples

```rescript
Int.toPrecisionWithPrecision(100, ~digits=2) // "1.0e+2"
Int.toPrecisionWithPrecision(1, ~digits=2) // "1.0"
```

## Exceptions

- `RangeError`: If `digits` is not between 1 and 100 (inclusive).
Implementations are allowed to support larger and smaller values as well.
ECMA-262 only requires a precision of up to 21 significant digits.

*/
@send
@deprecated("Use `toPrecision` instead")
external toPrecisionWithPrecision: (int, ~digits: int) => string = "toPrecision"

/**
`toString(n, ~radix=?)` return a `string` representing the given value.
`~radix` specifies the radix base to use for the formatted number.
See [`Number.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString)
on MDN.

## Examples

```rescript
Int.toString(1000) // "1000"
Int.toString(-1000) // "-1000"
Int.toString(6, ~radix=2) // "110"
Int.toString(373592855, ~radix=16) // "16449317"
Int.toString(123456, ~radix=36) // "2n9c"
```

## Exceptions

`RangeError`: if `radix` is less than 2 or greater than 36.
*/
@send
external toString: (int, ~radix: int=?) => string = "toString"

/**
`toStringWithRadix(n, ~radix)` return a `string` representing the given value.
`~radix` specifies the radix base to use for the formatted number.
See [`Number.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toString)
on MDN.

## Examples

```rescript
Int.toStringWithRadix(6, ~radix=2) // "110"
Int.toStringWithRadix(373592855, ~radix=16) // "16449317"
Int.toStringWithRadix(123456, ~radix=36) // "2n9c"
```

## Exceptions

`RangeError`: if `radix` is less than 2 or greater than 36.
*/
@deprecated("Use `toString` instead")
@send
external toStringWithRadix: (int, ~radix: int) => string = "toString"

/**
`toLocaleString(n)` return a `string` with language-sensitive representing the
given value. See [`Number.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toLocaleString) on MDN.

## Examples

```rescript
// If the application uses English as the default language
Int.toLocaleString(1000) // "1,000"

// If the application uses Portuguese Brazil as the default language
Int.toLocaleString(1000) // "1.000"
```
*/
@send
external toLocaleString: int => string = "toLocaleString"

/**
`toFloat(n)` return a `float` representing the given value.

## Examples

```rescript
Int.toFloat(100) == 100.0
Int.toFloat(2) == 2.0
```
*/
external toFloat: int => float = "%identity"

/**
`fromFloat(n)` return an `int` representing the given value. The conversion is
done by truncating the decimal part.

## Examples

```rescript
Int.fromFloat(2.0) == 2
Int.fromFloat(1.999) == 1
Int.fromFloat(1.5) == 1
Int.fromFloat(0.9999) == 0
```
*/
external fromFloat: float => int = "%intoffloat"

/**
`fromString(str, ~radix=?)` return an `option<int>` representing the given value
`str`. `~radix` specifies the radix base to use for the formatted number.

## Examples

```rescript
Int.fromString("0") == Some(0)
Int.fromString("NaN") == None
Int.fromString("6", ~radix=2) == None
```
*/
let fromString: (string, ~radix: int=?) => option<int>

/**
`mod(n1, n2)` calculates the modulo (remainder after division) of two integers.

## Examples

```rescript
Int.mod(7, 4) == 3
```
*/
external mod: (int, int) => int = "%modint"

/**
The options for `range`.
*/
type rangeOptions = {step?: int, inclusive?: bool}

/**
`range(start, end, ~options=?)` returns an int array of the sequence of integers in the
range `[start, end)`. That is, including `start` but excluding `end`.

If `step` is not set and `start < end`, the sequence will be increasing in steps of 1.

If `step` is not set and `start > end`, the sequence will be decreasing in steps of -1.

If `step` is set, the sequence will increase or decrease by that amount for each
step. If `start < end` and `step` is negative, or vice versa, an empty array is
returned since the sequence would otherwise never reach or exceed the end value
and hence be infinite. If `step` is `0` and `start !=` end, a `RangeError` is
raised as the sequence would never reach or exceed the end value and hence be
infinite.

If `inclusive` is set to `true`, the sequence will include `end` if `step` is
set such that the sequence includes it.

## Examples

```rescript
Int.range(3, 6) == [3, 4, 5]
Int.range(-3, -1) == [-3, -2]
Int.range(3, 1) == [3, 2]
Int.range(3, 7, ~options={step: 2}) == [3, 5]
Int.range(3, 7, ~options={step: 2, inclusive: true}) == [3, 5, 7]
Int.range(3, 6, ~options={step: -2}) // RangeError
```

## Exceptions

- Raises `RangeError` if `step == 0 && start != end`.
*/
let range: (int, int, ~options: rangeOptions=?) => array<int>

/**
`rangeWithOptions(start, end, options)` is like `range`, but with `step` and
`inclusive` options configurable.

If `step` is set, the sequence will increase or decrease by that amount for each
step. If `start < end` and `step` is negative, or vice versa, an empty array is
returned since the sequence would otherwise never reach or exceed the end value
and hence be infinite. If `step` is `0` and `start !=` end, a `RangeError` is
raised as the sequence would never reach or exceed the end value and hence be
infinite.

If `inclusive` is set to `true`, the sequence will include `end` if `step` is
set such that the sequence includes it.

## Examples

```rescript
Int.rangeWithOptions(3, 7, {step: 2}) == [3, 5]
Int.rangeWithOptions(3, 7, {step: 2, inclusive: true}) == [3, 5, 7]
Int.rangeWithOptions(3, 6, {step: -2}) // RangeError
```

## Exceptions

- Raises `RangeError` if `step == 0 && start != end`.
*/
@deprecated("Use `range` instead")
let rangeWithOptions: (int, int, rangeOptions) => array<int>

/**
`clamp(~min=?, ~max=?, value)` returns `value`, optionally bounded by `min` and `max`.

if `max` < `min` returns `min`.

## Examples

```rescript
Int.clamp(42) == 42
Int.clamp(42, ~min=50) == 50
Int.clamp(42, ~max=40) == 40
Int.clamp(42, ~min=50, ~max=40) == 50
```
*/
let clamp: (~min: int=?, ~max: int=?, int) => int

module Bitwise: {
  /**
  `land(n1, n2)` calculates the bitwise logical AND of two integers.

  ## Examples

  ```rescript
  Int.Bitwise.land(7, 4) == 4
  ```
  */
  external land: (int, int) => int = "%andint"

  /**
  `lor(n1, n2)` calculates the bitwise logical OR of two integers.

  ## Examples

  ```rescript
  Int.Bitwise.lor(7, 4) == 7
  ```
  */
  external lor: (int, int) => int = "%orint"

  /**
  `lxor(n1, n2)` calculates the bitwise logical XOR of two integers.

  ## Examples

  ```rescript
  Int.Bitwise.lxor(7, 4) == 3
  ```
  */
  external lxor: (int, int) => int = "%xorint"

  /**
  `lnot(n)` calculates the bitwise logical NOT of an integer.

  ## Examples

  ```rescript
  Int.Bitwise.lnot(2) == -3
  ```
  */
  let lnot: int => int

  /**
  `lsl(n, length)` calculates the bitwise logical left shift of an integer `n` by `length`.

  ## Examples

  ```rescript
  Int.Bitwise.lsl(4, 1) == 8
  ```
  */
  external lsl: (int, int) => int = "%lslint"

  /**
  `lsr(n, length)` calculates the bitwise logical right shift of an integer `n` by `length`.

  ## Examples

  ```rescript
  Int.Bitwise.lsr(8, 1) == 4
  ```
  */
  external lsr: (int, int) => int = "%lsrint"

  /**
  `asr(n, length)` calculates the bitwise arithmetic right shift of an integer `n` by `length`.

  ## Examples

  ```rescript
  Int.Bitwise.asr(4, 1) == 2
  ```
  */
  external asr: (int, int) => int = "%asrint"
}

/**
  `ignore(int)` ignores the provided int and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: int => unit = "%ignore"

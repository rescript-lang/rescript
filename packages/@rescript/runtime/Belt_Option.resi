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
In Belt we represent the existence and nonexistence of a value by wrapping it
with the `option` type.  In order to make it a bit more convenient to work with
option-types, Belt provides utility-functions for it.

The `option` type is a part of the ReScript standard library which is defined like this:

## Examples

```rescript
type option<'a> = None | Some('a)
```

```rescript
let someString: option<string> = Some("hello")
```
*/

/** Uncurried version of `keep` */
@deprecated("Use `keep` instead")
let keepU: (option<'a>, 'a => bool) => option<'a>

/**
If `optionValue` is `Some(value)` and `p(value) = true`, it returns `Some(value)`; otherwise returns `None`

## Examples

```rescript
Belt.Option.keep(Some(10), x => x > 5) == Some(10)
Belt.Option.keep(Some(4), x => x > 5) == None
Belt.Option.keep(None, x => x > 5) == None
```
*/
let keep: (option<'a>, 'a => bool) => option<'a>

/** Uncurried version of `forEach` */
@deprecated("Use `forEach` instead")
let forEachU: (option<'a>, 'a => unit) => unit

/**
If `optionValue` is `Some(value`), it calls `f(value)`; otherwise returns `()`

## Examples

```rescript
Belt.Option.forEach(Some("thing"), x => Js.log(x)) /* logs "thing" */
Belt.Option.forEach(None, x => Js.log(x)) /* returns () */
```
*/
let forEach: (option<'a>, 'a => unit) => unit

/**
Throws an Error in case `None` is provided. Use with care.

## Examples

```rescript
Some(3)->Belt.Option.getExn == 3

switch Belt.Option.getExn(None) {
// Throws an exception
| exception _ => assert(true)
| _ => assert(false)
}
```
*/
let getExn: option<'a> => 'a

/**
Throws an Error in case `None` is provided. Use with care.

## Examples

```rescript
Some(3)->Belt.Option.getOrThrow == 3

switch Belt.Option.getOrThrow(None) {
// Throws an exception
| exception _ => assert(true)
| _ => assert(false)
}
```
*/
let getOrThrow: option<'a> => 'a

/**
`getUnsafe(x)` returns `x`

This is an unsafe operation, it assumes `x` is neither `None`
nor `Some(None(...)))`
*/
external getUnsafe: option<'a> => 'a = "%identity"

/** Uncurried version of `mapWithDefault` */
@deprecated("Use `mapWithDefault` instead")
let mapWithDefaultU: (option<'a>, 'b, 'a => 'b) => 'b

/**
If `optionValue` is of `Some(value)`,
this function returns that value applied with `f`, in other words `f(value)`.

If `optionValue` is `None`, the default is returned.

## Examples

```rescript
let someValue = Some(3)
someValue->Belt.Option.mapWithDefault(0, x => x + 5) == 8

let noneValue = None
noneValue->Belt.Option.mapWithDefault(0, x => x + 5) == 0
```
*/
let mapWithDefault: (option<'a>, 'b, 'a => 'b) => 'b

/** Uncurried version of `map` */
@deprecated("Use `map` instead")
let mapU: (option<'a>, 'a => 'b) => option<'b>

/**
If `optionValue` is `Some(value)` this returns `f(value)`, otherwise it returns `None`.

## Examples

```rescript
Belt.Option.map(Some(3), x => x * x) == Some(9)

Belt.Option.map(None, x => x * x) == None
```
*/
let map: (option<'a>, 'a => 'b) => option<'b>

/** Uncurried version of `flatMap` */
@deprecated("Use `flatMap` instead")
let flatMapU: (option<'a>, 'a => option<'b>) => option<'b>

/**
If `optionValue` is `Some(value)`, returns `f(value)`, otherwise returns
`None`.<br/>
The function `f` must have a return type of `option<'b>`.

## Examples

```rescript
let addIfAboveOne = value =>
  if value > 1 {
    Some(value + 1)
  } else {
    None
  }

Belt.Option.flatMap(Some(2), addIfAboveOne) == Some(3)

Belt.Option.flatMap(Some(-4), addIfAboveOne) == None

Belt.Option.flatMap(None, addIfAboveOne) == None
```
*/
let flatMap: (option<'a>, 'a => option<'b>) => option<'b>

/**
If `optionalValue` is `Some(value)`, returns `value`, otherwise default.

## Examples

```rescript
Belt.Option.getWithDefault(None, "Banana") == "Banana"

Belt.Option.getWithDefault(Some("Apple"), "Banana") == "Apple"
```

```rescript
let greet = (firstName: option<string>) =>
  "Greetings " ++ firstName->Belt.Option.getWithDefault("Anonymous")

Some("Jane")->greet == "Greetings Jane"

None->greet == "Greetings Anonymous"
```
*/
let getWithDefault: (option<'a>, 'a) => 'a

/**
`orElse(optionalValue, otherOptional)` if `optionalValue` is `Some(value)`,
returns `Some(value)`, otherwise `otherOptional`

## Examples

```rescript
Belt.Option.orElse(Some(1812), Some(1066)) == Some(1812)
Belt.Option.orElse(None, Some(1066)) == Some(1066)
Belt.Option.orElse(None, None) == None
```
*/
let orElse: (option<'a>, option<'a>) => option<'a>

/**
Returns `true` if the argument is `Some(value)`, `false` otherwise.

## Examples

```rescript
Belt.Option.isSome(None) == false

Belt.Option.isSome(Some(1)) == true
```
*/
let isSome: option<'a> => bool

/**
Returns `true` if the argument is `None`, `false` otherwise.

## Examples

```rescript
Belt.Option.isNone(None) == true

Belt.Option.isNone(Some(1)) == false
```
*/
let isNone: option<'a> => bool

/**
Uncurried version of `eq`
*/
@deprecated("Use `eq` instead")
let eqU: (option<'a>, option<'b>, ('a, 'b) => bool) => bool

/**
Evaluates two optional values for equality with respect to a predicate
function. If both `optValue1` and `optValue2` are `None`, returns `true`.
If one of the arguments is `Some(value)` and the other is `None`, returns
`false`.

If arguments are `Some(value1)` and `Some(value2)`, returns the result of
`predicate(value1, value2)`; the predicate function must return a bool.

## Examples

```rescript
let clockEqual = (a, b) => mod(a, 12) == mod(b, 12)

open Belt.Option

eq(Some(3), Some(15), clockEqual) == true

eq(Some(3), None, clockEqual) == false

eq(None, Some(3), clockEqual) == false

eq(None, None, clockEqual) == true
```
*/
let eq: (option<'a>, option<'b>, ('a, 'b) => bool) => bool

/**
Uncurried version of `cmp`
*/
@deprecated("Use `cmp` instead")
let cmpU: (option<'a>, option<'b>, ('a, 'b) => int) => int

/**
`cmp(optValue1, optValue2, comparisonFunction)` compares two optional values
with respect to given `comparisonFunction`.

If both `optValue1` and `optValue2` are `None`, it returns `0`.

If the first argument is `Some(value1)` and the second is `None`, returns `1`
(something is greater than nothing).

If the first argument is `None` and the second is `Some(value2)`, returns `-1`
(nothing is less than something).

If the arguments are `Some(value1)` and `Some(value2)`, returns the result of
`comparisonFunction(value1, value2)`; comparisonFunction takes two arguments
and returns `-1` if the first argument is less than the second, `0` if the
arguments are equal, and `1` if the first argument is greater than the second.

## Examples

```rescript
let clockCompare = (a, b) => compare(mod(a, 12), mod(b, 12))

open Belt.Option

cmp(Some(3), Some(15), clockCompare) == 0

cmp(Some(3), Some(14), clockCompare) == 1

cmp(Some(2), Some(15), clockCompare) == -1

cmp(None, Some(15), clockCompare) == -1

cmp(Some(14), None, clockCompare) == 1

cmp(None, None, clockCompare) == 0
```
*/
let cmp: (option<'a>, option<'b>, ('a, 'b) => int) => int

/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/***
This module includes convenience methods for handling `int` types.
*/

/**
Converts a given `int` to a `float`.

## Examples

```rescript
Belt.Int.toFloat(1) == 1.0
```
*/
external toFloat: int => float = "%identity"

/**
Converts a given `float` to an `int`.

## Examples

```rescript
Belt.Int.fromFloat(1.0) == 1
```
*/
external fromFloat: float => int = "%intoffloat"

/**
Converts a given `string` to an `int`. Returns `Some(int)` when the input is a number, `None` otherwise.

## Examples

```rescript
Belt.Int.fromString("1") == Some(1)
```
*/
let fromString: string => option<int>

/**
Converts a given `int` to a `string`. Uses the JavaScript `String` constructor under the hood.

## Examples

```rescript
Belt.Int.toString(1) == "1"
```
*/
@val
external toString: int => string = "String"

/**
Addition of two `int` values. Same as the addition from `Pervasives`.

## Examples

```rescript
open Belt.Int
2 + 2 == 4
```
*/
external \"+": (int, int) => int = "%addint"

/**
Subtraction of two `int` values. Same as the subtraction from `Pervasives`.

## Examples

```rescript
open Belt.Int
2 - 1 == 1
```
*/
external \"-": (int, int) => int = "%subint"

/**
Multiplication of two `int` values. Same as the multiplication from `Pervasives`.

## Examples

```rescript
open Belt.Int
2 * 2 == 4
```
*/
external \"*": (int, int) => int = "%mulint"

/**
Division of two `int` values. Same as the division from `Pervasives`.

## Examples

```rescript
open Belt.Int
4 / 2 == 2
```
*/
external \"/": (int, int) => int = "%divint"

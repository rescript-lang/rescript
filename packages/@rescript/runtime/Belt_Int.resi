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

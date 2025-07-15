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

/*** This module includes convenience methods for handling `float` types. */

/**
Converts a given `float` to an `int`.

## Examples

```rescript
Belt.Float.toInt(1.0) == 1
```
*/
external toInt: float => int = "%intoffloat"

/** 
Converts a given `int` to a `float`.

## Examples

```rescript
Belt.Float.fromInt(1) == 1.0
```
*/
external fromInt: int => float = "%identity"

/** 
Converts a given `string` to a `float`. Returns `Some(float)` when the input is a number, `None` otherwise.

## Examples

```rescript
Belt.Float.fromString("1.0") == Some(1.0)
```
*/
let fromString: string => option<float>

/**
Converts a given `float` to a `string`. Uses the JavaScript `String` constructor under the hood.

## Examples

```rescript
Belt.Float.toString(1.0) == "1"
```
*/
@val
external toString: float => string = "String"

/**
Addition of two `float` values.
Can be opened in a module to avoid dot-notation (`+.`), however this yields a shadow warning (Warning number 44) in the default configuration.

## Examples

```rescript
open Belt.Float
2.0 + 2.0 == 4.0
```
*/
external \"+": (float, float) => float = "%addfloat"

/**
Subtraction of two `float` values.
Can be opened in a module to avoid dot-notation (`-.`), however this yields a shadow warning (Warning number 44) in the default configuration.

## Examples

```rescript
open Belt.Float
2.0 - 1.0 == 1.0
```
*/
external \"-": (float, float) => float = "%subfloat"

/**
Multiplication of two `float` values.
Can be opened in a module to avoid dot-notation (`*.`), however this yields a shadow warning (Warning number 44) in the default configuration.

## Examples

```rescript
open Belt.Float
2.0 * 2.0 == 4.0
```
*/
external \"*": (float, float) => float = "%mulfloat"

/** 
Division of two `float` values.
Can be opened in a module to avoid dot-notation (`/.`), however this yields a shadow warning (Warning number 44) in the default configuration.

## Examples

```rescript
open Belt.Float
4.0 / 2.0 == 2.0
```
*/
external \"/": (float, float) => float = "%divfloat"

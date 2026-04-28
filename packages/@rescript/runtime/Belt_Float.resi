/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

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

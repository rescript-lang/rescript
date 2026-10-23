/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/*** [`Belt.Int`]()
    Utilities for Int
*/

@val external isNaN: int => bool = "isNaN"

external toFloat: int => float = "%identity"

external fromFloat: float => int = "%intoffloat"

@val external fromString: (string, @as(10) _) => int = "parseInt"

let fromString = i =>
  switch fromString(i) {
  | i if isNaN(i) => None
  | i => Some(i)
  }

@val external toString: int => string = "String"

external \"+": (int, int) => int = "%addint"

external \"-": (int, int) => int = "%subint"

external \"*": (int, int) => int = "%mulint"

external \"/": (int, int) => int = "%divint"

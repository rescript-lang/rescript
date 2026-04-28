/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/*** [`Belt.Float`]()
    Utilities for Float
*/

@val external isNaN: float => bool = "isNaN"

external toInt: float => int = "%intoffloat"

external fromInt: int => float = "%identity"

@val external fromString: string => float = "parseFloat"

let fromString = i =>
  switch fromString(i) {
  | i if isNaN(i) => None
  | i => Some(i)
  }

@val external toString: float => string = "String"

external \"+": (float, float) => float = "%addfloat"

external \"-": (float, float) => float = "%subfloat"

external \"*": (float, float) => float = "%mulfloat"

external \"/": (float, float) => float = "%divfloat"

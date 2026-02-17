/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

external length: string => int = "%string_length"

@send external getChar: (string, int) => option<char> = "codePointAt"

@send external getCharUnsafe: (string, int) => char = "codePointAt"

@scope("String")
external fromChar: char => string = "fromCodePoint"

@send external repeat: (string, int) => string = "repeat"

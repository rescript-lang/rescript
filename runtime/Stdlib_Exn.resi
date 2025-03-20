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
Provide utilities for dealing with JS exceptions.
*/

/** Represents a JS exception */
type t

type exn += private Error(t)

let asJsExn: exn => option<t>

@get external stack: t => option<string> = "stack"
@get external message: t => option<string> = "message"
@get external name: t => option<string> = "name"
@get external fileName: t => option<string> = "fileName"

/**
`anyToExnInternal(obj)` will take any value `obj` and wrap it
in a Exn.Error if given value is not an exn already. If
`obj` is an exn, it will return `obj` without any changes.

This function is mostly useful for cases where you want to unify a type of a value
that potentially is either exn, a JS error, or any other JS value really (e.g. for
a value passed to a Promise.catch callback)

**IMPORTANT**: This is an internal API and may be changed / removed any time in the future.
*/
external anyToExnInternal: 'a => exn = "%wrap_exn"

/** Raise Js exception Error object with stacktrace */
let raiseError: string => 'a
let raiseEvalError: string => 'a
let raiseRangeError: string => 'a
let raiseReferenceError: string => 'a
let raiseSyntaxError: string => 'a
let raiseTypeError: string => 'a
let raiseUriError: string => 'a

/**
  `ignore(exn)` ignores the provided exn and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

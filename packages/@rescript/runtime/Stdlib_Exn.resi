/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/***
Provide utilities for dealing with JS exceptions.
*/

/** Represents a JS exception */
@deprecated({
  reason: "Use `JsExn.t` instead",
  migrate: %replace.type(: JsExn.t),
})
type t

type exn +=
  private
  | @deprecated({
    reason: "Use `JsExn` instead",
    migrate: %replace.constructor(JsExn),
  })
  Error(t)

@deprecated({
  reason: "Use `JsExn.fromException` instead",
  migrate: JsExn.fromException(),
})
let asJsExn: exn => option<t>

@deprecated({
  reason: "Use `JsExn.stack` instead",
  migrate: JsExn.stack(),
})
@get
external stack: t => option<string> = "stack"
@deprecated({
  reason: "Use `JsExn.message` instead",
  migrate: JsExn.message(),
})
@get
external message: t => option<string> = "message"
@deprecated({
  reason: "Use `JsExn.name` instead",
  migrate: JsExn.name(),
})
@get
external name: t => option<string> = "name"
@deprecated({
  reason: "Use `JsExn.fileName` instead",
  migrate: JsExn.fileName(),
})
@get
external fileName: t => option<string> = "fileName"

/**
`anyToExnInternal(obj)` will take any value `obj` and wrap it
in a Exn.Error if given value is not an exn already. If
`obj` is an exn, it will return `obj` without any changes.

This function is mostly useful for cases where you want to unify a type of a value
that potentially is either exn, a JS error, or any other JS value really (e.g. for
a value passed to a Promise.catch callback)

**IMPORTANT**: This is an internal API and may be changed / removed any time in the future.
*/
@deprecated({
  reason: "Use `JsExn.anyToExnInternal` instead",
  migrate: JsExn.anyToExnInternal(),
})
external anyToExnInternal: 'a => exn = "%wrap_exn"

/** Raise Js exception Error object with stacktrace */
@deprecated({
  reason: "Use `JsError.throwWithMessage` instead",
  migrate: JsError.throwWithMessage(),
})
let raiseError: string => 'a
@deprecated({
  reason: "Use `JsError.EvalError.throwWithMessage` instead",
  migrate: JsError.EvalError.throwWithMessage(),
})
let raiseEvalError: string => 'a
@deprecated({
  reason: "Use `JsError.RangeError.throwWithMessage` instead",
  migrate: JsError.RangeError.throwWithMessage(),
})
let raiseRangeError: string => 'a
@deprecated({
  reason: "Use `JsError.ReferenceError.throwWithMessage` instead",
  migrate: JsError.ReferenceError.throwWithMessage(),
})
let raiseReferenceError: string => 'a
@deprecated({
  reason: "Use `JsError.SyntaxError.throwWithMessage` instead",
  migrate: JsError.SyntaxError.throwWithMessage(),
})
let raiseSyntaxError: string => 'a
@deprecated({
  reason: "Use `JsError.TypeError.throwWithMessage` instead",
  migrate: JsError.TypeError.throwWithMessage(),
})
let raiseTypeError: string => 'a
@deprecated({
  reason: "Use `JsError.URIError.throwWithMessage` instead",
  migrate: JsError.URIError.throwWithMessage(),
})
let raiseUriError: string => 'a

/**
  `ignore(exn)` ignores the provided exn and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
@deprecated({
  reason: "Use `JsExn.ignore` instead",
  migrate: JsExn.ignore(),
})
external ignore: t => unit = "%ignore"

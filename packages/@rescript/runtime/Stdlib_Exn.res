/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

type t = unknown

@@warning("-38") /* unused extension constructor */
exception Error = JsExn

let asJsExn: exn => option<t> = exn =>
  switch Obj.magic(exn) {
  | Error(t) => Some(t)
  | _ => None
  }

@get external stack: t => option<string> = "stack"
@get external message: t => option<string> = "message"
@get external name: t => option<string> = "name"
@get external fileName: t => option<string> = "fileName"

type error
@new external makeError: string => error = "Error"

external anyToExnInternal: 'a => exn = "%wrap_exn"

let raiseError = str => throw((Obj.magic((makeError(str): error)): exn))

type eval_error
@new external makeEvalError: string => eval_error = "EvalError"

let raiseEvalError = str => throw((Obj.magic((makeEvalError(str): eval_error)): exn))

type range_error
@new external makeRangeError: string => range_error = "RangeError"

let raiseRangeError = str => throw((Obj.magic((makeRangeError(str): range_error)): exn))

type reference_error

@new external makeReferenceError: string => reference_error = "ReferenceError"

let raiseReferenceError = str => throw(Obj.magic(makeReferenceError(str)))

type syntax_error
@new external makeSyntaxError: string => syntax_error = "SyntaxError"

let raiseSyntaxError = str => throw(Obj.magic(makeSyntaxError(str)))

type type_error
@new external makeTypeError: string => type_error = "TypeError"

let raiseTypeError = str => throw(Obj.magic(makeTypeError(str)))

type uri_error
@new external makeURIError: string => uri_error = "URIError"

let raiseUriError = str => throw(Obj.magic(makeURIError(str)))

/* TODO add predicate to tell which error is which " */

/*
exception EvalError of error
exception RangeError of error
exception ReferenceError of error
exception SyntaxError of error
exception TypeError of error

 The URIError object represents an error when a global URI handling function was used in a wrong way. 
exception URIError of error    
*/

external ignore: t => unit = "%ignore"

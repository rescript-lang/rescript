/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/*** Provides functionality for dealing with the `'a Js.null` type */

@unboxed
type t<+'a> = Primitive_js_extern.null<'a> = Value('a) | @as(null) Null

external to_opt: t<'a> => option<'a> = "%null_to_opt"
external toOption: t<'a> => option<'a> = "%null_to_opt"
external return: 'a => t<'a> = "%identity"
let test: t<'a> => bool = x => x == Js_extern.null
external empty: t<'a> = "%null"
external getUnsafe: t<'a> => 'a = "%identity"

let getExn = f =>
  switch toOption(f) {
  | None => Stdlib_Exn.raiseError("Js.Null.getExn")
  | Some(x) => x
  }

let bind = (x, f) =>
  switch toOption(x) {
  | None => empty
  | Some(x) => return(f(x))
  }

let iter = (x, f) =>
  switch toOption(x) {
  | None => ()
  | Some(x) => f(x)
  }

let fromOption = x =>
  switch x {
  | None => empty
  | Some(x) => return(x)
  }

let from_opt = fromOption

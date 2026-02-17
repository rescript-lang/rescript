/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/*** Provides functionality for dealing with the `'a Js.undefined` type */

type t<+'a> = Primitive_js_extern.undefined<'a>

let to_opt: t<'a> => option<'a> = Primitive_option.fromUndefined
let toOption: t<'a> => option<'a> = Primitive_option.fromUndefined

external return: 'a => t<'a> = "%identity"

external empty: t<'a> = "%undefined"
let test: t<'a> => bool = x => x == empty
let testAny: 'a => bool = x => Obj.magic(x) == empty
external getUnsafe: t<'a> => 'a = "%identity"

let getExn = f =>
  switch toOption(f) {
  | None => Stdlib_Exn.raiseError("Js.Undefined.getExn")
  | Some(x) => x
  }

let bind = (x, f) =>
  switch to_opt(x) {
  | None => empty
  | Some(x) => return(f(x))
  }

let iter = (x, f) =>
  switch to_opt(x) {
  | None => ()
  | Some(x) => f(x)
  }

let fromOption = x =>
  switch x {
  | None => empty
  | Some(x) => return(x)
  }

let from_opt = fromOption

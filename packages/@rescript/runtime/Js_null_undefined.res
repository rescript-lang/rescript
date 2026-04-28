/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/*** Contains functionality for dealing with values that can be both `null` and `undefined` */

@unboxed
type t<+'a> = Primitive_js_extern.nullable<'a> =
  Value('a) | @as(null) Null | @as(undefined) Undefined

external toOption: t<'a> => option<'a> = "%nullable_to_opt"
external to_opt: t<'a> => option<'a> = "%nullable_to_opt"
external return: 'a => t<'a> = "%identity"
external isNullable: t<'a> => bool = "%is_nullable"
external null: t<'a> = "%null"
external undefined: t<'a> = "%undefined"

let bind = (x, f) =>
  switch to_opt(x) {
  | None => (Obj.magic((x: t<'a>)): t<'b>)
  | Some(x) => return(f(x))
  }

let iter = (x, f) =>
  switch to_opt(x) {
  | None => ()
  | Some(x) => f(x)
  }

let fromOption = x =>
  switch x {
  | None => undefined
  | Some(x) => return(x)
  }

let from_opt = fromOption

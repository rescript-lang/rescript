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

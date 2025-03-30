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

/*** Provides functionality for dealing with the `'a Js.undefined` type */

type t<+'a> = Primitive_js_extern.undefined<'a>

external to_opt: t<'a> => option<'a> = "%undefined_to_opt"
external toOption: t<'a> => option<'a> = "%undefined_to_opt"
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

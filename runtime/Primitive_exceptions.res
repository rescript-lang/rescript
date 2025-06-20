/* Copyright (C) 2015- Hongbo Zhang, Authors of ReScript
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

module Obj = Primitive_object_extern
module Js = Primitive_js_extern

type t = {@as("RE_EXN_ID") id: string}

exception Error = JsExn
type js_error = {cause: exn}

/**
   This function should never throw
   It could be either customized exception or built in exception 
   Note due to that in OCaml extensible variants have the same 
   runtime representation as exception, so we can not 
   really tell the difference. 

   However, if we make a false alarm, classified extensible variant 
   as exception, it will be OKAY for nested pattern match

   {[
     match toExn x : exn option with 
     | Some _ 
       -> Js.log "Could be an OCaml exception or an open variant"
     (* If it is an Open variant, it will never pattern match, 
        This is Okay, since exception could never have exhaustive pattern match

     *)
     | None -> Js.log "Not an OCaml exception for sure"
   ]}

   However, there is still something wrong, since if user write such code
   {[
     match toExn x with 
     | Some _ -> (* assert it is indeed an exception *)
       (* This assertion is wrong, since it could be an open variant *)
     | None -> (* assert it is not an exception *)
   ]}

   This is not a problem in `try .. with` since the logic above is not expressible, see more design in [destruct_exn.md]
*/
let isExtension = (type a, e: a): bool =>
  if Js.testAny(e) {
    false
  } else {
    Js.typeof((Obj.magic(e): t).id) == "string"
  }

/**   
   This function has to be in this module Since 
   [Error] is defined here 
*/
let internalToException = (e: unknown) =>
  if isExtension(e) {
    (Obj.magic(e): exn)
  } else {
    JsExn(e)
  }

module Dict = {
  @obj
  external empty: unit => dict<'a> = ""

  @set_index
  external set: (dict<'a>, string, 'a) => unit = ""

  /**
    It's the same as `Js.Dict.get` but it doesn't have runtime overhead to check if the key exists.
   */
  @get_index
  external dangerouslyGetNonOption: (dict<'a>, string) => option<'a> = ""
}

/**
  Needs to have unique extension ids when used with functors.
  See discussion in https://github.com/rescript-lang/rescript-compiler/pull/6570
*/
let idMap = Dict.empty()

let create = (str: string): string => {
  switch idMap->Dict.dangerouslyGetNonOption(str) {
  | Some(v) => {
      let id = v + 1
      idMap->Dict.set(str, id)
      str ++ ("/" ++ (Obj.magic((id: int)): string))
    }
  | None => {
      idMap->Dict.set(str, 1)
      str
    }
  }
}

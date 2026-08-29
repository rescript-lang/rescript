/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

module Obj = Primitive_object_extern

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
       -> Console.log "Could be an OCaml exception or an open variant"
     (* If it is an Open variant, it will never pattern match, 
        This is Okay, since exception could never have exhaustive pattern match

     *)
     | None -> Console.log "Not an OCaml exception for sure"
   ]}

   However, there is still something wrong, since if user write such code
   {[
     match toExn x with 
     | Some _ -> (* assert it is indeed an exception *)
       (* This assertion is wrong, since it could be an open variant *)
     | None -> (* assert it is not an exception *)
   ]}

   This is not a problem in `try .. with`: a handler only asks whether the
   caught value matches an exception branch, never whether an arbitrary value
   is an exception - the question a general exception-destruction operator
   would force, and which cannot be answered soundly while open variants
   share the exception representation.
*/
let isExtension = (type a, e: a): bool =>
  if Primitive_js_extern.testAny(e) {
    false
  } else {
    Primitive_js_extern.typeof((Obj.magic(e): t).id) == "string"
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
    It's the same as `Dict.get` but it doesn't have runtime overhead to check if the key exists.
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

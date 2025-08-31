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

/*** Provides a simple key-value dictionary abstraction over native JavaScript objects */

/** The dict type */
type t<'a> = dict<'a>

/** The key type, an alias of string */
type key = string

/**
  `unsafeGet dict key` returns the value associated with `key` in `dict`

  This function will return an invalid value (`undefined`) if `key` does not exist in `dict`. It
  will not throw an error.
*/
@get_index
external unsafeGet: (t<'a>, key) => 'a = ""
let \".!()" = unsafeGet

/** `get dict key` returns the value associated with `key` in `dict` */
let get = (type u, dict: t<u>, k: key): option<u> =>
  if %raw(`k in dict`) {
    Some(\".!()"(dict, k))
  } else {
    None
  }

/** `set dict key value` sets the value of `key` in `dict` to `value` */
@set_index
external set: (t<'a>, key, 'a) => unit = ""

/** `keys dict` returns an array of all the keys in `dict` */
@val
external keys: t<'a> => array<key> = "Object.keys"

/** `empty ()` creates an empty dictionary */
@obj
external empty: unit => t<'a> = ""

let unsafeDeleteKey: (t<string>, string) => unit = %raw(` function (dict,key){
      delete dict[key];
     }
  `)

@new external unsafeCreate: int => array<'a> = "Array"
/* external entries : 'a t -> (key * 'a) array = "Object.entries" [@@val] (* ES2017 *) */
let entries = dict => {
  let keys = keys(dict)
  let l = Js_array2.length(keys)
  let values = unsafeCreate(l)
  for i in 0 to l - 1 {
    let key = Js_array2.unsafe_get(keys, i)
    Js_array2.unsafe_set(values, i, (key, \".!()"(dict, key)))
  }
  values
}

/* external values : 'a t -> 'a array = "Object.values" [@@val] (* ES2017 *) */
let values = dict => {
  let keys = keys(dict)
  let l = Js_array2.length(keys)
  let values = unsafeCreate(l)
  for i in 0 to l - 1 {
    Js_array2.unsafe_set(values, i, \".!()"(dict, Js_array2.unsafe_get(keys, i)))
  }
  values
}

let fromList = entries => {
  let dict = empty()
  let rec loop = x =>
    switch x {
    | list{} => dict
    | list{(key, value), ...rest} =>
      set(dict, key, value)
      loop(rest)
    }

  loop(entries)
}

let fromArray = entries => {
  let dict = empty()
  let l = Js_array2.length(entries)
  for i in 0 to l - 1 {
    let (key, value) = Js_array2.unsafe_get(entries, i)
    set(dict, key, value)
  }
  dict
}

let map = (f, source) => {
  let target = empty()
  let keys = keys(source)
  let l = Js_array2.length(keys)
  for i in 0 to l - 1 {
    let key = Js_array2.unsafe_get(keys, i)
    set(target, key, f(unsafeGet(source, key)))
  }
  target
}

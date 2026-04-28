/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

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

/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

let keep = (opt, p) =>
  switch opt {
  | Some(x) as some if p(x) => some
  | _ => None
  }

let forEach = (opt, f) =>
  switch opt {
  | Some(x) => f(x)
  | None => ()
  }

let getOrThrow = x =>
  switch x {
  | Some(x) => x
  | None => throw(Not_found)
  }

let getExn = getOrThrow

external getUnsafe: option<'a> => 'a = "%identity"

let mapWithDefault = (opt, default, f) =>
  switch opt {
  | Some(x) => f(x)
  | None => default
  }

let map = (opt, f) =>
  switch opt {
  | Some(x) => Some(f(x))
  | None => None
  }

let flatMap = (opt, f) =>
  switch opt {
  | Some(x) => f(x)
  | None => None
  }

let getWithDefault = (opt, default) =>
  switch opt {
  | Some(x) => x
  | None => default
  }

let orElse = (opt, other) =>
  switch opt {
  | Some(_) as some => some
  | None => other
  }

let isSome = x =>
  switch x {
  | Some(_) => true
  | None => false
  }

let isNone = x => x == None

let eq = (a, b, f) =>
  switch a {
  | Some(a) =>
    switch b {
    | None => false
    | Some(b) => f(a, b)
    }
  | None => b == None
  }

let cmp = (a, b, f) =>
  switch (a, b) {
  | (Some(a), Some(b)) => f(a, b)
  | (None, Some(_)) => -1
  | (Some(_), None) => 1
  | (None, None) => 0
  }

let cmpU = cmp
let eqU = eq
let flatMapU = flatMap
let forEachU = forEach
let keepU = keep
let mapU = map
let mapWithDefaultU = mapWithDefault

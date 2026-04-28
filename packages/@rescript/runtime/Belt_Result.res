/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

type t<'a, 'b> = result<'a, 'b> =
  | Ok('a)
  | Error('b)

let getOrThrow = x =>
  switch x {
  | Ok(x) => x
  | Error(_) => throw(Not_found)
  }

let getExn = getOrThrow

let mapWithDefault = (opt, default, f) =>
  switch opt {
  | Ok(x) => f(x)
  | Error(_) => default
  }

let map = (opt, f) =>
  switch opt {
  | Ok(x) => Ok(f(x))
  | Error(y) => Error(y)
  }

let flatMap = (opt, f) =>
  switch opt {
  | Ok(x) => f(x)
  | Error(y) => Error(y)
  }

let getWithDefault = (opt, default) =>
  switch opt {
  | Ok(x) => x
  | Error(_) => default
  }

let isOk = x =>
  switch x {
  | Ok(_) => true
  | Error(_) => false
  }

let isError = x =>
  switch x {
  | Ok(_) => false
  | Error(_) => true
  }

let eq = (a, b, f) =>
  switch (a, b) {
  | (Ok(a), Ok(b)) => f(a, b)
  | (Error(_), Ok(_))
  | (Ok(_), Error(_)) => false
  | (Error(_), Error(_)) => true
  }

let cmp = (a, b, f) =>
  switch (a, b) {
  | (Ok(a), Ok(b)) => f(a, b)
  | (Error(_), Ok(_)) => -1
  | (Ok(_), Error(_)) => 1
  | (Error(_), Error(_)) => 0
  }

let cmpU = cmp
let eqU = eq
let flatMapU = flatMap
let mapU = map
let mapWithDefaultU = mapWithDefault

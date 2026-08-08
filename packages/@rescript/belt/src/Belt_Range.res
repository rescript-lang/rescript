/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

let forEach = (s, f, action) =>
  for i in s to f {
    (action(i): unit)
  }

let rec every = (s, f, p) =>
  if s > f {
    true
  } else {
    p(s) && every(s + 1, f, p)
  }

let rec everyByAux = (s, f, ~step, p) =>
  if s > f {
    true
  } else {
    p(s) && everyByAux(s + step, f, ~step, p)
  }

let everyBy = (s, f, ~step, p) =>
  if step > 0 {
    everyByAux(s, f, ~step, p)
  } else {
    true
  } /* return empty range `true` */

let rec some = (s, f, p) =>
  if s > f {
    false
  } else {
    p(s) || some(s + 1, f, p)
  }

let rec someByAux = (s, f, ~step, p) =>
  if s > f {
    false
  } else {
    p(s) || someByAux(s + step, f, ~step, p)
  }

let someBy = (s, f, ~step, p) =>
  if step > 0 {
    someByAux(s, f, ~step, p)
  } else {
    false
  } /* return empty range, `false` */

let everyByU = everyBy
let everyU = every
let forEachU = forEach
let someByU = someBy
let someU = some

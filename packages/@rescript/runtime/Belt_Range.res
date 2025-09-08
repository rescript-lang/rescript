/* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

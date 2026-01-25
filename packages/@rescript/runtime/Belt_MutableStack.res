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

type rec t<'a> = {mutable root: opt_cell<'a>}
and opt_cell<'a> = option<cell<'a>>
and cell<'a> = {
  head: 'a,
  tail: opt_cell<'a>,
}

let make = () => {root: None}

let clear = s => s.root = None

let copy = (s: t<_>): t<_> => {root: s.root}

let push = (s, x) => s.root = Some({head: x, tail: s.root})

let topUndefined = (s: t<'a>) =>
  switch s.root {
  | None => Js.undefined
  | Some(x) => Js.Undefined.return(x.head)
  }

let top = s =>
  switch s.root {
  | None => None
  | Some(x) => Some(x.head)
  }

let isEmpty = s => s.root == None

let popUndefined = s =>
  switch s.root {
  | None => Js.undefined
  | Some(x) =>
    s.root = x.tail
    Js.Undefined.return(x.head)
  }

let pop = s =>
  switch s.root {
  | None => None
  | Some(x) =>
    s.root = x.tail
    Some(x.head)
  }

let rec lengthAux = (x: cell<_>, acc) =>
  switch x.tail {
  | None => acc + 1
  | Some(x) => lengthAux(x, acc + 1)
  }

let size = s =>
  switch s.root {
  | None => 0
  | Some(x) => lengthAux(x, 0)
  }

let rec iterAux = (s: opt_cell<_>, f) =>
  switch s {
  | None => ()
  | Some(x) =>
    f(x.head)
    iterAux(x.tail, f)
  }

let forEach = (s, f) => iterAux(s.root, f)

let rec dynamicPopIter = (s, f) =>
  switch s.root {
  | Some({tail, head}) =>
    s.root = tail
    f(head)
    dynamicPopIter(s, f) /* using root, `f` may change it */
  | None => ()
  }

let dynamicPopIterU = dynamicPopIter
let forEachU = forEach

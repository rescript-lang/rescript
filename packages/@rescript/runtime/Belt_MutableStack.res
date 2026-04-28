/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

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

let top = s =>
  switch s.root {
  | None => None
  | Some(x) => Some(x.head)
  }

let isEmpty = s => s.root == None

let pop = s =>
  switch s.root {
  | None => None
  | Some(x) =>
    s.root = x.tail
    Some(x.head)
  }

let popOrThrow = s =>
  switch s.root {
  | None => throw(Not_found)
  | Some(x) =>
    s.root = x.tail
    x.head
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

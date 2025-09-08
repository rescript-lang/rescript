/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Francois Pottier, projet Cristal, INRIA Rocquencourt */
/* Jeremie Dimino, Jane Street Europe */
/*  */
/* Copyright 2002 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */
/* Adapted significantly by ReScript Authors */
module A = Belt_Array

type rec node<'a> = {
  content: 'a,
  mutable next: cell<'a>,
}
and cell<'a> = option<node<'a>>
and t<'a> = {
  mutable length: int,
  mutable first: cell<'a>,
  mutable last: cell<'a>,
}

let make = () => {
  length: 0,
  first: None,
  last: None,
}

let clear = q => {
  q.length = 0
  q.first = None
  q.last = None
}

let add = (q, x) => {
  let cell = Some({
    content: x,
    next: None,
  })
  switch q.last {
  | None =>
    /* TODO: better names for intermediate var */
    q.length = 1
    q.first = cell
    q.last = cell
  | Some(last) =>
    q.length = q.length + 1
    last.next = cell
    q.last = cell
  }
}

let peek = q =>
  switch q.first {
  /* same here could be v */
  | None => None
  | Some(v) => Some(v.content)
  }

let peekUndefined = q =>
  switch q.first {
  | None => Js.undefined
  | Some(v) => Js.Undefined.return(v.content)
  }

let peekOrThrow = q =>
  switch q.first {
  | None => throw(Not_found)
  | Some(v) => v.content
  }

let peekExn = peekOrThrow

let pop = q =>
  switch q.first {
  | None => None
  | Some(x) =>
    let next = x.next
    if next == None {
      /* only one element */
      clear(q)
      Some(x.content)
    } else {
      q.length = q.length - 1
      q.first = next
      Some(x.content)
    }
  }

let popOrThrow = q =>
  /* TO fix */
  switch q.first {
  | None => throw(Not_found)
  | Some(x) =>
    let next = x.next
    if next == None {
      /* only one element */
      clear(q)
      x.content
    } else {
      q.length = q.length - 1
      q.first = next
      x.content
    }
  }

let popExn = popOrThrow

let popUndefined = q =>
  switch q.first {
  | None => Js.undefined
  | Some(x) =>
    let next = x.next
    if next == None {
      /* only one element */
      clear(q)
      Js.Undefined.return(x.content)
    } else {
      q.length = q.length - 1
      q.first = next
      Js.Undefined.return(x.content)
    }
  }

let rec copyAux = (qRes, prev, cell) =>
  switch cell {
  | None =>
    qRes.last = prev
    qRes
  | Some(x) =>
    let content = x.content
    let res = Some({content, next: None})
    switch prev {
    | None => qRes.first = res
    | Some(p) => p.next = res
    }
    copyAux(qRes, res, x.next)
  }

let copy = q => copyAux({length: q.length, first: None, last: None}, None, q.first)

let rec copyMapAux = (qRes, prev, cell, f) =>
  switch cell {
  | None =>
    qRes.last = prev
    qRes
  | Some(x) =>
    let content = f(x.content)
    let res = Some({content, next: None})
    switch prev {
    /* TODO: optimize to remove such check */
    | None => qRes.first = res
    | Some(p) => p.next = res
    }
    copyMapAux(qRes, res, x.next, f)
  }

let map = (q, f) => copyMapAux({length: q.length, first: None, last: None}, None, q.first, f)

let isEmpty = q => q.length == 0

let size = q => q.length

let rec iterAux = (cell, f) =>
  switch cell {
  | None => ()
  | Some(x) =>
    f(x.content)
    iterAux(x.next, f)
  }

let forEach = (q, f) => iterAux(q.first, f)

let rec foldAux = (f, accu, cell) =>
  switch cell {
  | None => accu
  | Some(x) =>
    let accu = f(accu, x.content)
    foldAux(f, accu, x.next)
  }

let reduce = (q, accu, f) => foldAux(f, accu, q.first)

let transfer = (q1, q2) =>
  if q1.length > 0 {
    switch q2.last {
    | None =>
      q2.length = q1.length
      q2.first = q1.first
      q2.last = q1.last
      clear(q1)
    | Some(l) =>
      q2.length = q2.length + q1.length
      l.next = q1.first
      q2.last = q1.last
      clear(q1)
    }
  }

let rec fillAux = (i, arr, cell) =>
  switch cell {
  | None => ()
  | Some(x) =>
    A.setUnsafe(arr, i, x.content)
    fillAux(i + 1, arr, x.next)
  }

let toArray = x => {
  let v = A.makeUninitializedUnsafe(x.length)
  fillAux(0, v, x.first)
  v
}

/* TODO: optimize */
let fromArray = arr => {
  let q = make()
  for i in 0 to A.length(arr) - 1 {
    add(q, A.getUnsafe(arr, i))
  }
  q
}

let forEachU = forEach
let mapU = map
let reduceU = reduce

/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Francois Pottier, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 2002 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the GNU Library General Public License, with */
/* the special exception on linking described in file ../LICENSE. */
/*  */
/* ********************************************************************* */

exception Empty

/* OCaml currently does not allow the components of a sum type to be
   mutable. Yet, for optimal space efficiency, we must have cons cells
   whose [next] field is mutable. This leads us to define a type of
   cyclic lists, so as to eliminate the [Nil] case and the sum
   type. */

type rec cell<'a> = {
  content: 'a,
  mutable next: cell<'a>,
}

/* A queue is a reference to either nothing or some cell of a cyclic
   list. By convention, that cell is to be viewed as the last cell in
   the queue. The first cell in the queue is then found in constant
   time: it is the next cell in the cyclic list. The queue's length is
   also recorded, so as to make [length] a constant-time operation.

   The [tail] field should really be of type ['a cell option], but
   then it would be [None] when [length] is 0 and [Some] otherwise,
   leading to redundant memory allocation and accesses. We avoid this
   overhead by filling [tail] with a dummy value when [length] is 0.
   Of course, this requires bending the type system's arm slightly,
   because it does not have dependent sums. */

type t<'a> = {
  mutable length: int,
  mutable tail: cell<'a>,
}

let create = () => {
  length: 0,
  tail: Obj.magic(None),
}

let clear = q => {
  q.length = 0
  q.tail = Obj.magic(None)
}

let add = (x, q) =>
  if q.length == 0 {
    let rec cell = {
      content: x,
      next: cell,
    }
    q.length = 1
    q.tail = cell
  } else {
    let tail = q.tail
    let head = tail.next
    let cell = {
      content: x,
      next: head,
    }
    q.length = q.length + 1
    tail.next = cell
    q.tail = cell
  }

let push = add

let peek = q =>
  if q.length == 0 {
    throw(Empty)
  } else {
    q.tail.next.content
  }

let top = peek

let take = q => {
  if q.length == 0 {
    throw(Empty)
  }
  q.length = q.length - 1
  let tail = q.tail
  let head = tail.next
  if head === tail {
    q.tail = Obj.magic(None)
  } else {
    tail.next = head.next
  }
  head.content
}

let pop = take

let copy = q =>
  if q.length == 0 {
    create()
  } else {
    let tail = q.tail

    let rec tail' = {
      content: tail.content,
      next: tail',
    }

    let rec copy = (prev, cell) =>
      if cell !== tail {
        let res = {
          content: cell.content,
          next: tail',
        }
        prev.next = res
        copy(res, cell.next)
      }

    copy(tail', tail.next)
    {
      length: q.length,
      tail: tail',
    }
  }

let is_empty = q => q.length == 0

let length = q => q.length

let iter = (f, q) =>
  if q.length > 0 {
    let tail = q.tail
    let rec iter = cell => {
      f(cell.content)
      if cell !== tail {
        iter(cell.next)
      }
    }
    iter(tail.next)
  }

let fold = (f, accu, q) =>
  if q.length == 0 {
    accu
  } else {
    let tail = q.tail
    let rec fold = (accu, cell) => {
      let accu = f(accu, cell.content)
      if cell === tail {
        accu
      } else {
        fold(accu, cell.next)
      }
    }
    fold(accu, tail.next)
  }

let transfer = (q1, q2) => {
  let length1 = q1.length
  if length1 > 0 {
    let tail1 = q1.tail
    clear(q1)
    if q2.length > 0 {
      let tail2 = q2.tail
      let head1 = tail1.next
      let head2 = tail2.next
      tail1.next = head2
      tail2.next = head1
    }
    q2.length = q2.length + length1
    q2.tail = tail1
  }
}

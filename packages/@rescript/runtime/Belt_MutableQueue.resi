/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */
/* Adapted significantly by ReScript Authors */

/***
A FIFO (first in first out) queue data structure.
*/

/**
The type of queues containing elements of `type('a)`.
*/
type t<'a>

/** 
Returns a new queue, initially empty.
*/
let make: unit => t<'a>

/**
Discard all elements from the queue.
*/
let clear: t<'a> => unit

/**
Returns `true` if the given queue is empty, `false` otherwise.
*/
let isEmpty: t<'a> => bool

/**
`fromArray` a is equivalent to `Array.forEach(a, add(q, a));`
*/
let fromArray: array<'a> => t<'a>

/**
`add(q, x)` adds the element `x` at the end of the queue `q`.
*/
let add: (t<'a>, 'a) => unit

/**
`peekOpt(q)` returns the first element in queue `q`, without removing it from the queue.
*/
let peek: t<'a> => option<'a>

/**
`peekUndefined(q)` returns `undefined` if not found.
*/
let peekUndefined: t<'a> => Js.undefined<'a>

/**
`peekExn(q)` throws an exception if `q` is empty.
*/
let peekExn: t<'a> => 'a

/**
`peekOrThrow(q)` throws an exception if `q` is empty.
*/
let peekOrThrow: t<'a> => 'a

/**
`pop(q)` removes and returns the first element in queue `q`.
*/
let pop: t<'a> => option<'a>

/**
`popUndefined(q)` removes and returns the first element in queue `q`. it will
return `undefined` if it is already empty.
*/
let popUndefined: t<'a> => Js.undefined<'a>

/**
`popExn(q)` throw an exception if q is empty.
*/
let popExn: t<'a> => 'a

/**
`popOrThrow(q)` throw an exception if q is empty.
*/
let popOrThrow: t<'a> => 'a

/**
`copy(q)` returns a fresh queue.
*/
let copy: t<'a> => t<'a>

/**
Returns the number of elements in a queue.
*/
let size: t<'a> => int

@deprecated("Use `map` instead")
let mapU: (t<'a>, 'a => 'b) => t<'b>
let map: (t<'a>, 'a => 'b) => t<'b>
@deprecated("Use `forEach` instead")
let forEachU: (t<'a>, 'a => unit) => unit

/**
`forEach(q, f) applies`f`in turn to all elements of`q`, from the least
recently entered to the most recently entered. The queue itself is unchanged.
*/
let forEach: (t<'a>, 'a => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t<'a>, 'b, ('b, 'a) => 'b) => 'b

/**
`reduce(q, accu, f)` is equivalent to `List.reduce(l, accu, f)`, where `l` is the
list of `q`'s elements. The queue remains unchanged.
*/
let reduce: (t<'a>, 'b, ('b, 'a) => 'b) => 'b

/**
`transfer(q1, q2)` adds all of `q1`'s elements at the end of the queue `q2`,
then clears `q1`. It is equivalent to the sequence `forEach((x) => add(x, q2), q1)`;
clear `q1`, but runs in constant time.
*/
let transfer: (t<'a>, t<'a>) => unit

/**
First added will be in the beginning of the array.
*/
let toArray: t<'a> => array<'a>

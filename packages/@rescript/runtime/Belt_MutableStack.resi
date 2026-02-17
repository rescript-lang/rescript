/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/***
First in last out stack. This module implements stacks, with in-place
modification.
*/

type t<'a>

/**
Returns a new stack, initially empty.
*/
let make: unit => t<'a>

/**
Discard all elements from the stack.
*/
let clear: t<'a> => unit

/**
`copy(x)` O(1) operation, return a new stack.
*/
let copy: t<'a> => t<'a>

let push: (t<'a>, 'a) => unit
let popUndefined: t<'a> => Js.undefined<'a>
let pop: t<'a> => option<'a>
let topUndefined: t<'a> => Js.undefined<'a>
let top: t<'a> => option<'a>
let isEmpty: t<'a> => bool
let size: t<'a> => int
@deprecated("Use `forEach` instead")
let forEachU: (t<'a>, 'a => unit) => unit
let forEach: (t<'a>, 'a => unit) => unit
@deprecated("Use `dynamicPopIter` instead")
let dynamicPopIterU: (t<'a>, 'a => unit) => unit

/**
`dynamicPopIter(s, f)` apply `f` to each element of `s`. The item is poped
before applying `f`, `s` will be empty after this opeartion. This function is
useful for worklist algorithm.
 */
let dynamicPopIter: (t<'a>, 'a => unit) => unit

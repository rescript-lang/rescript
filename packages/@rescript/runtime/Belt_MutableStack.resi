/* Copyright (C) 2017 Authors of ReScript
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

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

/*
This internal module contains methods which does not rely on ordering.
Such methods could be shared between `generic set/specalized set` whether mutable
or immutable depends on use cases
*/
type rec t<'value> = option<node<'value>>
and node<'value> = {
  @as("v") mutable value: 'value,
  @as("h") mutable height: int,
  @as("l") mutable left: t<'value>,
  @as("r") mutable right: t<'value>,
}

type cmp<'a, 'b> = Belt_Id.cmp<'a, 'b>

let copy: t<'a> => t<'a>
let create: (t<'a>, 'a, t<'a>) => t<'a>
let bal: (t<'a>, 'a, t<'a>) => t<'a>
let singleton: 'a => t<'a>

let minimum: t<'a> => option<'a>
let minUndefined: t<'a> => Js.undefined<'a>
let maximum: t<'a> => option<'a>
let maxUndefined: t<'a> => Js.undefined<'a>

let removeMinAuxWithRef: (node<'a>, ref<'a>) => t<'a>
/* `removeMinAuxWithRef n cell` return a new node with
 minimum removed and stored in cell */

let isEmpty: t<'a> => bool

let stackAllLeft: (t<'a>, list<node<'a>>) => list<node<'a>>

let forEach: (t<'a>, 'a => unit) => unit

let reduce: (t<'a>, 'b, ('b, 'a) => 'b) => 'b

let every: (t<'a>, 'a => bool) => bool

let some: (t<'a>, 'a => bool) => bool

let joinShared: (t<'a>, 'a, t<'a>) => t<'a>
let concatShared: (t<'a>, t<'a>) => t<'a>

let keepShared: (t<'a>, 'a => bool) => t<'a>

let keepCopy: (t<'a>, 'a => bool) => t<'a>

let partitionShared: (t<'a>, 'a => bool) => (t<'a>, t<'a>)

let partitionCopy: (t<'a>, 'a => bool) => (t<'a>, t<'a>)

let lengthNode: node<'a> => int
let size: t<'a> => int

let toList: t<'a> => list<'a>

/**
**throw** when invariant is not held
*/
let checkInvariantInternal: t<_> => unit

let fillArray: (node<'a>, int, array<'a>) => int
let toArray: t<'a> => array<'a>
let fromSortedArrayAux: (array<'a>, int, int) => t<'a>
let fromSortedArrayRevAux: (array<'a>, int, int) => t<'a>
let fromSortedArrayUnsafe: array<'a> => t<'a>
let has: (t<'a>, 'a, ~cmp: cmp<'a, 'b>) => bool
let cmp: (t<'a>, t<'a>, ~cmp: cmp<'a, 'b>) => int
let eq: (t<'a>, t<'a>, ~cmp: cmp<'a, 'b>) => bool
let subset: (t<'a>, t<'a>, ~cmp: cmp<'a, 'b>) => bool
let get: (t<'a>, 'a, ~cmp: cmp<'a, 'b>) => option<'a>
let getUndefined: (t<'a>, 'a, ~cmp: cmp<'a, 'b>) => Js.undefined<'a>
let getOrThrow: (t<'a>, 'a, ~cmp: cmp<'a, 'b>) => 'a

let fromArray: (array<'a>, ~cmp: cmp<'a, 'b>) => t<'a>

let addMutate: (~cmp: cmp<'a, 'b>, t<'a>, 'a) => t<'a>
let balMutate: node<'a> => node<'a>
/**
`removeMinAuxWithRootMutate(root, n)` remove the minimum of n in place and store
its value in the `key root`
*/
let removeMinAuxWithRootMutate: (node<'a>, node<'a>) => t<'a>

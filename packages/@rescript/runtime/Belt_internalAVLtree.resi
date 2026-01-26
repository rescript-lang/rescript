/* Copyright (C) 2018 Authors of ReScript
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

type rec t<'key, 'a> = option<node<'key, 'a>>

and node<'k, 'v> = {
  @as("k") mutable key: 'k,
  @as("v") mutable value: 'v,
  @as("h") mutable height: int,
  @as("l") mutable left: t<'k, 'v>,
  @as("r") mutable right: t<'k, 'v>,
}

type cmp<'k, 'id> = Belt_Id.cmp<'k, 'id>

let copy: t<'k, 'v> => t<'k, 'v>
let create: (t<'a, 'b>, 'a, 'b, t<'a, 'b>) => t<'a, 'b>
let bal: (t<'a, 'b>, 'a, 'b, t<'a, 'b>) => t<'a, 'b>

let singleton: ('a, 'b) => t<'a, 'b>

let updateValue: (node<'k, 'v>, 'v) => node<'k, 'v>

let minKey: t<'a, 'b> => option<'a>
let minKeyUndefined: t<'a, 'b> => Js.undefined<'a>

let maxKey: t<'a, 'b> => option<'a>
let maxKeyUndefined: t<'a, 'b> => Js.undefined<'a>

let minimum: t<'a, 'b> => option<('a, 'b)>
let minUndefined: t<'a, 'b> => Js.undefined<('a, 'b)>

let maximum: t<'a, 'b> => option<('a, 'b)>
let maxUndefined: t<'a, 'b> => Js.undefined<('a, 'b)>

let removeMinAuxWithRef: (node<'a, 'b>, ref<'a>, ref<'b>) => t<'a, 'b>

let isEmpty: t<_> => bool

let stackAllLeft: (t<'a, 'b>, list<node<'a, 'b>>) => list<node<'a, 'b>>

let findFirstBy: (t<'a, 'b>, ('a, 'b) => bool) => option<('a, 'b)>

let forEach: (t<'a, 'b>, ('a, 'b) => unit) => unit

let map: (t<'c, 'a>, 'a => 'b) => t<'c, 'b>

let mapWithKey: (t<'a, 'b>, ('a, 'b) => 'c) => t<'a, 'c>

let reduce: (t<'a, 'b>, 'c, ('c, 'a, 'b) => 'c) => 'c

let every: (t<'a, 'b>, ('a, 'b) => bool) => bool

let some: (t<'a, 'b>, ('a, 'b) => bool) => bool

let join: (t<'a, 'b>, 'a, 'b, t<'a, 'b>) => t<'a, 'b>

let concat: (t<'a, 'b>, t<'a, 'b>) => t<'a, 'b>

let concatOrJoin: (t<'a, 'b>, 'a, option<'b>, t<'a, 'b>) => t<'a, 'b>

let keepShared: (t<'a, 'b>, ('a, 'b) => bool) => t<'a, 'b>

let keepMap: (t<'a, 'b>, ('a, 'b) => option<'c>) => t<'a, 'c>

/* seems no sharing, could be shared with mutation */
let partitionShared: (t<'a, 'b>, ('a, 'b) => bool) => (t<'a, 'b>, t<'a, 'b>)

let lengthNode: node<'a, 'b> => int
let size: t<'a, 'b> => int

let toList: t<'a, 'b> => list<('a, 'b)>
/**
**throw** when invariant is not held
*/
let checkInvariantInternal: t<'a, 'b> => unit

let fillArray: (node<'a, 'b>, int, array<('a, 'b)>) => int

let toArray: t<'a, 'b> => array<('a, 'b)>
let keysToArray: t<'a, 'b> => array<'a>
let valuesToArray: t<'a, 'b> => array<'b>
let fromSortedArrayAux: (array<('a, 'b)>, int, int) => t<'a, 'b>
let fromSortedArrayRevAux: (array<('a, 'b)>, int, int) => t<'a, 'b>
let fromSortedArrayUnsafe: array<('a, 'b)> => t<'a, 'b>

let cmp: (t<'a, 'b>, t<'a, 'c>, ~kcmp: cmp<'a, _>, ~vcmp: ('b, 'c) => int) => int

let eq: (t<'a, 'b>, t<'a, 'c>, ~kcmp: cmp<'a, _>, ~veq: ('b, 'c) => bool) => bool

let get: (t<'a, 'b>, 'a, ~cmp: cmp<'a, _>) => option<'b>

let getUndefined: (t<'a, 'b>, 'a, ~cmp: cmp<'a, _>) => Js.undefined<'b>

let getWithDefault: (t<'a, 'b>, 'a, 'b, ~cmp: cmp<'a, _>) => 'b
let getOrThrow: (t<'a, 'b>, 'a, ~cmp: cmp<'a, _>) => 'b

let has: (t<'a, 'b>, 'a, ~cmp: cmp<'a, _>) => bool

let fromArray: (array<('a, 'b)>, ~cmp: cmp<'a, 'id>) => t<'a, 'b>

let updateMutate: (t<'a, 'b>, 'a, 'b, ~cmp: cmp<'a, 'id>) => t<'a, 'b>

let balMutate: node<'a, 'b> => node<'a, 'b>

let removeMinAuxWithRootMutate: (node<'a, 'b>, node<'a, 'b>) => t<'a, 'b>

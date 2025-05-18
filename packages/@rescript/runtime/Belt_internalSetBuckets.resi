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

module C = Belt_internalBucketsType

type rec bucket<'a> = {
  mutable key: 'a,
  mutable next: C.opt<bucket<'a>>,
}
and t<'hash, 'eq, 'a> = C.container<'hash, 'eq, bucket<'a>>

let copy: t<'hash, 'eq, 'a> => t<'hash, 'eq, 'a>

let forEach: (t<'hash, 'eq, 'a>, 'a => unit) => unit
let fillArray: (int, array<'a>, bucket<'a>) => int

let toArray: t<_, _, 'a> => array<'a>

let reduce: (t<_, _, 'a>, 'b, ('b, 'a) => 'b) => 'b

let logStats: t<_> => unit

let getBucketHistogram: t<_> => array<int>

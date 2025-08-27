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

module C = Belt_internalBucketsType

type rec bucket<'a, 'b> = {
  mutable key: 'a,
  mutable value: 'b,
  mutable next: C.opt<bucket<'a, 'b>>,
}
and t<'hash, 'eq, 'a, 'b> = C.container<'hash, 'eq, bucket<'a, 'b>>

let copy: t<'hash, 'eq, 'a, 'b> => t<'hash, 'eq, 'a, 'b>

let forEach: (t<_, _, 'a, 'b>, ('a, 'b) => 'c) => unit

let reduce: (t<_, _, 'a, 'b>, 'c, ('c, 'a, 'b) => 'c) => 'c

let logStats: t<_> => unit

let keepMapInPlace: (t<_, _, 'a, 'b>, ('a, 'b) => option<'b>) => unit

let fillArray: (int, array<('a, 'b)>, bucket<'a, 'b>) => int

let keysToArray: t<_, _, 'a, _> => array<'a>

let valuesToArray: t<_, _, _, 'b> => array<'b>

let toArray: t<_, _, 'a, 'b> => array<('a, 'b)>

let getBucketHistogram: t<_> => array<int>

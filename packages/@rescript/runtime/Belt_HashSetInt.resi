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
This module is [`Belt.HashSet`]() specialized with key type to be a primitive type.

It is more efficient in general, the  API is the same with [`Belt.HashSet`]() except its key type is fixed,
and identity is not needed(using the built-in one)

**See** [`Belt.HashSet`]()
*/

type key = int

type t

let make: (~hintSize: int) => t

let clear: t => unit

let isEmpty: t => bool

let add: (t, key) => unit

let copy: t => t

let has: (t, key) => bool

let remove: (t, key) => unit

@deprecated("Use `forEach` instead")
let forEachU: (t, key => unit) => unit
let forEach: (t, key => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t, 'c, ('c, key) => 'c) => 'c
let reduce: (t, 'c, ('c, key) => 'c) => 'c

let size: t => int

let logStats: t => unit

let toArray: t => array<key>

let fromArray: array<key> => t

let mergeMany: (t, array<key>) => unit

let getBucketHistogram: t => array<int>

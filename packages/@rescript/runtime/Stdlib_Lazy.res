/* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

type t<+'a>

/* Internals of forcing lazy values. */
type internal<'a> = {
  @as("LAZY_DONE") mutable tag: bool,
  /* Invariant: name */
  @as("VAL") mutable value: 'a,
  /* its type is ['a] or [unit -> 'a ] */
}

external fnToVal: (unit => 'a) => 'a = "%identity"
external valToFn: 'a => unit => 'a = "%identity"
external castToConcrete: t<'a> => internal<'a> = "%identity"
external castFromConcrete: internal<'a> => t<'a> = "%identity"

let is_val = (type a, l: t<a>): bool => castToConcrete(l).tag

exception Undefined

let forward_with_closure = (type a, blk: internal<a>, closure: unit => a): a => {
  let result = closure()
  blk.value = result
  blk.tag = true
  result
}

let raise_undefined = () => throw(Undefined)

/* Assume [blk] is a block with tag lazy */
let force_lazy_block = (type a, blk: internal<a>): a => {
  let closure = valToFn(blk.value)
  blk.value = fnToVal(raise_undefined)
  try forward_with_closure(blk, closure) catch {
  | e =>
    blk.value = fnToVal(() => throw(e))
    throw(e)
  }
}

/* Assume [blk] is a block with tag lazy */
let force_val_lazy_block = (type a, blk: internal<a>): a => {
  let closure = valToFn(blk.value)
  blk.value = fnToVal(raise_undefined)
  forward_with_closure(blk, closure)
}

let force = (type a, lzv: t<a>): a => {
  let lzv: internal<_> = castToConcrete(lzv)
  if lzv.tag {
    lzv.value
  } else {
    force_lazy_block(lzv)
  }
}

let force_val = (type a, lzv: t<a>): a => {
  let lzv: internal<_> = castToConcrete(lzv)
  if lzv.tag {
    lzv.value
  } else {
    force_val_lazy_block(lzv)
  }
}

let from_fun = (type a, closure: unit => a): t<a> => {
  let blk = {
    tag: false,
    value: fnToVal(closure),
  }
  castFromConcrete(blk)
}

let from_val = (type a, value: a): t<a> => {
  let blk = {
    tag: true,
    value,
  }
  castFromConcrete(blk)
}

let make = from_fun
let get = force
let isEvaluated = is_val

external ignore: t<'a> => unit = "%ignore"

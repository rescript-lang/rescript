/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

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

let isEvaluated = (type a, l: t<a>): bool => castToConcrete(l).tag

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

let get = (type a, lzv: t<a>): a => {
  let lzv: internal<_> = castToConcrete(lzv)
  if lzv.tag {
    lzv.value
  } else {
    force_lazy_block(lzv)
  }
}

let make = (type a, closure: unit => a): t<a> => {
  let blk = {
    tag: false,
    value: fnToVal(closure),
  }
  castFromConcrete(blk)
}

external ignore: t<'a> => unit = "%ignore"

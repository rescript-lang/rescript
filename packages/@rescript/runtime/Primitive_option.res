/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

module Obj = Primitive_object_extern

type nested = {@as("BS_PRIVATE_NESTED_SOME_NONE") depth: int}

/* INPUT: [x] should not be nullable */
let isNested = (x: Obj.t): bool => {
  Obj.repr((Obj.magic(x): nested).depth) !== Obj.repr(Primitive_js_extern.undefined)
}

external getUnsafe: (array<'a>, int) => 'a = "%array_unsafe_get"
external setUnsafe: (array<'a>, int, 'a) => unit = "%array_unsafe_set"
@scope("Object") external freeze: nested => nested = "freeze"

/* One frozen representation per depth, created on first use, so that
   wrapping and unwrapping a nested [None] does not allocate and
   [Some(None) === Some(None)] holds */
let markers: array<nested> = []

let marker = (depth: int): Obj.t => {
  let m = getUnsafe(markers, depth)
  if Obj.repr(m) !== Obj.repr(Primitive_js_extern.undefined) {
    Obj.repr(m)
  } else {
    let m = freeze({depth: depth})
    setUnsafe(markers, depth, m)
    Obj.repr(m)
  }
}

let some = (x: Obj.t): Obj.t =>
  if Obj.magic(x) == None {
    marker(0)
  } /* [x] is neither None nor null so it is safe to do property access */
  else if x !== Obj.repr(Primitive_js_extern.null) && isNested(x) {
    marker((Obj.magic(x): nested).depth + 1)
  } else {
    x
  }

let fromNullable = (type t, x: Primitive_js_extern.nullable<t>): option<t> =>
  if Primitive_js_extern.isNullable(x) {
    None
  } else {
    Obj.magic(some((Obj.magic(x): 'a)))
  }

let fromNull = (type t, x: Primitive_js_extern.null<t>): option<t> =>
  if Obj.magic(x) === Primitive_js_extern.null {
    None
  } else {
    Obj.magic(some((Obj.magic(x): 'a)))
  }

/* external valFromOption : 'a option -> 'a =
 "#val_from_option" */

/** Preserves `None`/`undefined` and unwraps one outer option layer.
    Nested `Some` values stay encoded one level deeper. */
let valFromOption = (x: Obj.t): Obj.t =>
  if x !== Obj.repr(Primitive_js_extern.null) && isNested(x) {
    let {depth}: nested = Obj.magic(x)
    if depth == 0 {
      Obj.magic(None)
    } else {
      marker(depth - 1)
    }
  } else {
    Obj.magic(x)
  }

type poly = {@as("VAL") value: Obj.t}

/** [input] is optional polymorphic variant */
let unwrapPolyVar = (x: option<poly>) =>
  switch x {
  | None => Obj.repr(x)
  | Some(x) => x.value
  }

/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

module Obj = Primitive_object_extern
module Js = Primitive_js_extern

type nested = {@as("BS_PRIVATE_NESTED_SOME_NONE") depth: int}

/* INPUT: [x] should not be nullable */
let isNested = (x: Obj.t): bool => {
  Obj.repr((Obj.magic(x): nested).depth) !== Obj.repr(Js.undefined)
}

let some = (x: Obj.t): Obj.t =>
  if Obj.magic(x) == None {
    Obj.repr({depth: 0})
  } /* [x] is neither None nor null so it is safe to do property access */
  else if x !== Obj.repr(Js.null) && isNested(x) {
    Obj.repr({depth: (Obj.magic(x): nested).depth + 1})
  } else {
    x
  }

let fromNullable = (type t, x: Js.nullable<t>): option<t> =>
  if Js.isNullable(x) {
    None
  } else {
    Obj.magic(some((Obj.magic(x): 'a)))
  }

let fromUndefined = (type t, x: Js.undefined<t>): option<t> =>
  if Obj.magic(x) === Js.undefined {
    None
  } else {
    Obj.magic(some((Obj.magic(x): 'a)))
  }

let fromNull = (type t, x: Js.null<t>): option<t> =>
  if Obj.magic(x) === Js.null {
    None
  } else {
    Obj.magic(some((Obj.magic(x): 'a)))
  }

/* external valFromOption : 'a option -> 'a =
 "#val_from_option" */

/** The input is already of [Some] form, [x] is not None, 
    make sure [x[0]] will not throw */
let valFromOption = (x: Obj.t): Obj.t =>
  if x !== Obj.repr(Js.null) && isNested(x) {
    let {depth}: nested = Obj.magic(x)
    if depth == 0 {
      Obj.magic(None)
    } else {
      Obj.repr({depth: depth - 1})
    }
  } else {
    Obj.magic(x)
  }

let toUndefined = (x: option<'a>) =>
  if x == None {
    Js.undefined
  } else {
    Obj.magic(valFromOption(Obj.repr(x)))
  }

type poly = {@as("VAL") value: Obj.t}

/** [input] is optional polymorphic variant */
let unwrapPolyVar = (x: option<poly>) =>
  switch x {
  | None => Obj.repr(x)
  | Some(x) => x.value
  }

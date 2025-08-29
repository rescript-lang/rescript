/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

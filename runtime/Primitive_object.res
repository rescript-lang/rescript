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

module Array = Primitive_array_extern
module Js = Primitive_js_extern

type t = Primitive_object_extern.t

// Note: this only works as intended as long as the runtime is compiled
// with -bs-cross-module-opt.
let repr = Primitive_object_extern.repr
let magic = Primitive_object_extern.magic
let tag = Primitive_object_extern.tag
let size = Primitive_object_extern.size

module O = {
  @val external isArray: 'a => bool = "Array.isArray"
  type key = string
  let for_in: (t, key => unit) => unit = %raw(`function(o,foo){
        for (var x in o) { foo(x) }}
      `)

  @scope(("Object", "prototype", "hasOwnProperty"))
  @val
  /**
     JS objects are not guaranteed to have `Object` in their prototype
     chain so calling `some_obj.hasOwnProperty(key)` can sometimes throw
     an exception when dealing with JS interop. This mainly occurs when
     objects are created via `Object.create(null)`. The only safe way
     to call this function is directly, e.g. `Object.prototype.hasOwnProperty.call(some_obj, key)`.
  */
  external hasOwnProperty: (t, key) => bool = "call"

  @get_index external get_value: (t, key) => t = ""
}

let updateDummy = Primitive_object_extern.updateDummy

/** TODO: investigate total
    [compare x y] returns [0] if [x] is equal to [y],
    a negative integer if [x] is less than [y],
    and a positive integer if [x] is greater than [y].
    The ordering implemented by compare is compatible with the comparison
    predicates [=], [<] and [>] defined above, with one difference on the treatment of the float value
    [nan].

    Namely, the comparison predicates treat nan as different from any other float value,
    including itself; while compare treats [nan] as equal to itself and less than any other float value.
    This treatment of [nan] ensures that compare defines a total ordering relation.
    compare applied to functional values may raise Invalid_argument. compare applied to cyclic structures
    may not terminate.

    The compare function can be used as the comparison function required by the [Set.Make] and [Map.Make] functors,
    as well as the [List.sort] and [Array.sort] functions.
*/
let rec compare = (a: t, b: t): int =>
  if a === b {
    0
  } else {
    /* front and formoest, we do not compare function values */
    let a_type = Js.typeof(a)
    let b_type = Js.typeof(b)
    switch (a_type, b_type) {
    | ("undefined", _) => -1
    | (_, "undefined") => 1
    /* [a] is of type string, b can not be None,
        [a] could be (Some (Some x)) in that case [b] could be [Some None] or [null]
         so [b] has to be of type string or null */
    | ("string", "string") => Pervasives.compare((magic(a): string), magic(b))
    | ("string", _) => /* [b] could be [Some None] or [null] */
      1
    | (_, "string") => -1
    | ("boolean", "boolean") => Pervasives.compare((magic(a): bool), magic(b))
    | ("boolean", _) => 1
    | (_, "boolean") => -1
    | ("function", "function") => raise(Invalid_argument("compare: functional value"))
    | ("function", _) => 1
    | (_, "function") => -1
    | ("bigint", "bigint")
    | ("number", "number") =>
      Pervasives.compare((magic(a): float), (magic(b): float))
    | ("number", _) =>
      if b === repr(Js.null) || Primitive_option.isNested(b) {
        1
      } else {
        /* Some (Some ..) < x */
        -1
      } /* Integer < Block in OCaml runtime GPR #1195, except Some.. */
    | (_, "number") =>
      if a === repr(Js.null) || Primitive_option.isNested(a) {
        -1
      } else {
        1
      }
    | _ =>
      if a === repr(Js.null) {
        /* [b] could not be null otherwise would equal */
        if Primitive_option.isNested(b) {
          1
        } else {
          -1
        }
      } else if b === repr(Js.null) {
        if Primitive_option.isNested(a) {
          -1
        } else {
          1
        }
      } else if (
        /* double_array_tag: 254
         */
        Primitive_option.isNested(a)
      ) {
        if Primitive_option.isNested(b) {
          aux_obj_compare(a, b)
        } else {
          /* Some (None) < Some (Some (None)) */

          /* b could not be undefined/None */
          /* Some (None) < Some (...) */
          -1
        }
      } else {
        let tag_a = tag(a)
        let tag_b = tag(b)
        if tag_a != tag_b {
          if tag_a < tag_b {
            -1
          } else {
            1
          }
        } else {
          let len_a = size(a)
          let len_b = size(b)
          if len_a == len_b {
            if O.isArray(a) {
              aux_same_length((magic(a): array<t>), (magic(b): array<t>), 0, len_a)
            } else if %raw(`a instanceof Date && b instanceof Date`) {
              %raw(`a - b`)
            } else {
              aux_obj_compare(a, b)
            }
          } else if len_a < len_b {
            /* at least one is not zero, so it is an array block */
            aux_length_a_short((magic(a): array<t>), (magic(b): array<t>), 0, len_a)
          } else {
            aux_length_b_short((magic(a): array<t>), (magic(b): array<t>), 0, len_b)
          }
        }
      }
    }
  }

and aux_same_length = (a: array<t>, b: array<t>, i, same_length) =>
  if i == same_length {
    0
  } else {
    let res = compare(Array.getUnsafe(a, i), Array.getUnsafe(b, i))

    if res != 0 {
      res
    } else {
      aux_same_length(a, b, i + 1, same_length)
    }
  }

and aux_length_a_short = (a: array<t>, b: array<t>, i, short_length) =>
  if i == short_length {
    -1
  } else {
    let res = compare(Array.getUnsafe(a, i), Array.getUnsafe(b, i))

    if res != 0 {
      res
    } else {
      aux_length_a_short(a, b, i + 1, short_length)
    }
  }

and aux_length_b_short = (a: array<t>, b: array<t>, i, short_length) =>
  if i == short_length {
    1
  } else {
    let res = compare(Array.getUnsafe(a, i), Array.getUnsafe(b, i))

    if res != 0 {
      res
    } else {
      aux_length_b_short(a, b, i + 1, short_length)
    }
  }

and aux_obj_compare = (a: t, b: t) => {
  let min_key_lhs = ref(None)
  let min_key_rhs = ref(None)
  let do_key = ((a, b, min_key), key) =>
    if !O.hasOwnProperty(b, key) || compare(O.get_value(a, key), O.get_value(b, key)) > 0 {
      switch min_key.contents {
      | None => min_key.contents = Some(key)
      | Some(mk) =>
        if key < mk {
          min_key.contents = Some(key)
        }
      }
    }

  let do_key_a = key => do_key((a, b, min_key_rhs), key)
  let do_key_b = key => do_key((b, a, min_key_lhs), key)
  O.for_in(a, do_key_a)
  O.for_in(b, do_key_b)
  let res = switch (min_key_lhs.contents, min_key_rhs.contents) {
  | (None, None) => 0
  | (Some(_), None) => -1
  | (None, Some(_)) => 1
  | (Some(x), Some(y)) => Pervasives.compare(x, y)
  }

  res
}

type eq = (t, t) => bool

/** It is easier to do equality check than comparision, since as long as its
    basic type is not the same, it will not equal 
*/
let rec equal = (a: t, b: t): bool =>
  /* front and formoest, we do not compare function values */
  if a === b {
    true
  } else {
    let a_type = Js.typeof(a)
    if (
      a_type == "string" ||
        (a_type == "number" ||
        (a_type == "bigint" ||
          (a_type == "boolean" ||
          (a_type == "undefined" || a === %raw(`null`)))))
    ) {
      false
    } else {
      let b_type = Js.typeof(b)
      if a_type == "function" || b_type == "function" {
        raise(Invalid_argument("equal: functional value"))
      } /* first, check using reference equality */
      else if (
        /* a_type = "object" || "symbol" */
        b_type == "number" || (b_type == "bigint" || (b_type == "undefined" || b === %raw(`null`)))
      ) {
        false
      } else {
        /* [a] [b] could not be null, so it can not raise */
        let tag_a = tag(a)
        let tag_b = tag(b)
        if tag_a != tag_b {
          false
        } else {
          let len_a = size(a)
          let len_b = size(b)
          if len_a == len_b {
            if O.isArray(a) {
              aux_equal_length((magic(a): array<t>), (magic(b): array<t>), 0, len_a)
            } else if %raw(`a instanceof Date && b instanceof Date`) {
              !(Js.gt(a, b) || Js.lt(a, b))
            } else {
              aux_obj_equal(a, b)
            }
          } else {
            false
          }
        }
      }
    }
  }

and aux_equal_length = (a: array<t>, b: array<t>, i, same_length) =>
  if i == same_length {
    true
  } else {
    equal(Array.getUnsafe(a, i), Array.getUnsafe(b, i)) &&
    aux_equal_length(a, b, i + 1, same_length)
  }

and aux_obj_equal = (a: t, b: t) => {
  let result = ref(true)
  let do_key_a = key =>
    if !O.hasOwnProperty(b, key) {
      result.contents = false
    }

  let do_key_b = key =>
    if !O.hasOwnProperty(a, key) || !equal(O.get_value(b, key), O.get_value(a, key)) {
      result.contents = false
    }

  O.for_in(a, do_key_a)
  if result.contents {
    O.for_in(b, do_key_b)
  }
  result.contents
}

@inline
let isNumberOrBigInt = a => Js.typeof(a) == "number" || Js.typeof(a) == "bigint"

@inline
let canNumericCompare = (a, b) => isNumberOrBigInt(a) && isNumberOrBigInt(b)

let notequal = (a, b) =>
  if canNumericCompare(a, b) {
    (magic(a): float) != (magic(b): float)
  } else {
    !equal(a, b)
  }

let greaterequal = (a, b) =>
  if canNumericCompare(a, b) {
    (magic(a): float) >= (magic(b): float)
  } else {
    compare(a, b) >= 0
  }

let greaterthan = (a, b) =>
  if canNumericCompare(a, b) {
    (magic(a): float) > (magic(b): float)
  } else {
    compare(a, b) > 0
  }

let lessequal = (a, b) =>
  if canNumericCompare(a, b) {
    (magic(a): float) <= (magic(b): float)
  } else {
    compare(a, b) <= 0
  }

let lessthan = (a, b) =>
  if canNumericCompare(a, b) {
    (magic(a): float) < (magic(b): float)
  } else {
    compare(a, b) < 0
  }

let min = (x: t, y) =>
  if compare(x, y) <= 0 {
    x
  } else {
    y
  }

let max = (x: t, y) =>
  if compare(x, y) >= 0 {
    x
  } else {
    y
  }

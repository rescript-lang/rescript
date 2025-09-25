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

/***
JavaScript Typed Array API

**see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray)
*/

@@warning("-103")

@deprecated({
  reason: "Use `ArrayBuffer.t` instead.",
  migrate: %replace.type(: ArrayBuffer.t),
})
type array_buffer = Js_typed_array2.array_buffer

@deprecated(
  "This has been deprecated and will be removed in v13. Use functions and types from the `TypedArray` module instead."
)
type array_like<'a> = Js_typed_array2.array_like<'a>

module type Type = {
  type t
}
module ArrayBuffer = {
  /***
  The underlying buffer that the typed arrays provide views of

  **see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer)
  */

  type t = array_buffer

  /** takes length. initializes elements to 0 */
  @new
  external make: int => t = "ArrayBuffer"

  /* ArrayBuffer.isView: seems pointless with a type system */
  /* experimental
  external transfer : array_buffer -> t = "ArrayBuffer.transfer" [@@val]
  external transferWithLength : array_buffer -> int -> t = "ArrayBuffer.transfer" [@@val]
 */

  @get external byteLength: t => int = "byteLength"

  // @bs.send.pipe(: t) external slice: (~start: int, ~end_: int) => array_buffer = "slice" /* FIXME */
  // @bs.send.pipe(: t) external sliceFrom: int => array_buffer = "slice"
}
module type S = {
  /*** Implements functionality common to all the typed arrays */

  type elt
  type typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  // @bs.send.pipe(: t) external setArray: array<elt> => unit = "set"
  // @bs.send.pipe(: t) external setArrayOffset: (array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish)
   * ---
   */
  @get external length: t => int = "length"

  /* Mutator functions
   */
  // @bs.send.pipe(: t) external copyWithin: (~to_: int) => t = "copyWithin"
  // @bs.send.pipe(: t) external copyWithinFrom: (~to_: int, ~from: int) => t = "copyWithin"
  // @bs.send.pipe(: t)
  external copyWithinFromRange: (~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  // @bs.send.pipe(: t) external fillInPlace: elt => t = "fill"
  // @bs.send.pipe(: t) external fillFromInPlace: (elt, ~from: int) => t = "fill"
  // @bs.send.pipe(: t) external fillRangeInPlace: (elt, ~start: int, ~end_: int) => t = "fill"

  // @bs.send.pipe(: t) external reverseInPlace: t = "reverse"

  // @bs.send.pipe(: t) external sortInPlace: t = "sort"
  // @bs.send.pipe(: t) external sortInPlaceWith: ((. elt, elt) => int) => t = "sort"

  /* Accessor functions
   */
  // @bs.send.pipe(: t) /** ES2016 */
  @deprecated({
    reason: "Use `TypedArray.includes` instead.",
    migrate: TypedArray.includes(),
  })
  external includes: elt => bool = "includes"

  // @bs.send.pipe(: t) external indexOf: elt => int = "indexOf"
  // @bs.send.pipe(: t) external indexOfFrom: (elt, ~from: int) => int = "indexOf"

  // @bs.send.pipe(: t) external join: string = "join"
  // @bs.send.pipe(: t) external joinWith: string => string = "join"

  // @bs.send.pipe(: t) external lastIndexOf: elt => int = "lastIndexOf"
  // @bs.send.pipe(: t) external lastIndexOfFrom: (elt, ~from: int) => int = "lastIndexOf"

  // @bs.send.pipe(: t) external slice: (~start: int, ~end_: int) => t = "slice"
  // @bs.send.pipe(: t) external copy: t = "slice"
  // @bs.send.pipe(: t) external sliceFrom: int => t = "slice"

  // @bs.send.pipe(: t) external subarray: (~start: int, ~end_: int) => t = "subarray"
  // @bs.send.pipe(: t) external subarrayFrom: int => t = "subarray"

  // @bs.send.pipe(: t) external toString: string = "toString"
  // @bs.send.pipe(: t) external toLocaleString: string = "toLocaleString"

  /* Iteration functions
   */
  /* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@// @bs.send.pipe: t]
 */

  // @bs.send.pipe(: t) external every: ((. elt) => bool) => bool = "every"
  // @bs.send.pipe(: t) external everyi: ((. elt, int) => bool) => bool = "every"

  /** should we use `bool` or `boolean` seems they are intechangeable here */
  external // @bs.send.pipe(: t)
  filter: (elt => bool) => t = "filter"
  // @bs.send.pipe(: t) external filteri: ((. elt, int) => bool) => t = "filter"

  // @bs.send.pipe(: t) external find: ((. elt) => bool) => Js.undefined<elt> = "find"
  // @bs.send.pipe(: t) external findi: ((. elt, int) => bool) => Js.undefined<elt> = "find"

  // @bs.send.pipe(: t) external findIndex: ((. elt) => bool) => int = "findIndex"
  // @bs.send.pipe(: t) external findIndexi: ((. elt, int) => bool) => int = "findIndex"

  // @bs.send.pipe(: t) external forEach: ((. elt) => unit) => unit = "forEach"
  // @bs.send.pipe(: t) external forEachi: ((. elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@// @bs.send.pipe: t]
 */

  // @bs.send.pipe(: t) external map: ((. elt) => 'b) => typed_array<'b> = "map"
  // @bs.send.pipe(: t) external mapi: ((. elt, int) => 'b) => typed_array<'b> = "map"

  // @bs.send.pipe(: t) external reduce: ((. 'b, elt) => 'b, 'b) => 'b = "reduce"
  // @bs.send.pipe(: t) external reducei: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduce"

  // @bs.send.pipe(: t) external reduceRight: ((. 'b, elt) => 'b, 'b) => 'b = "reduceRight"
  // @bs.send.pipe(: t) external reduceRighti: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  // @bs.send.pipe(: t) external some: ((. elt) => bool) => bool = "some"
  // @bs.send.pipe(: t) external somei: ((. elt, int) => bool) => bool = "some"

  /* commented out until bs has a plan for iterators
  external values : elt array_iter = "" [@// @bs.send.pipe: t]
 */
}

/* commented out until bs has a plan for iterators
  external values : elt array_iter = "" [@// @bs.send.pipe: t]
 */

module Int8Array = {
  /** */
  type elt = int
  type typed_array<'a> = Js_typed_array2.Int8Array.typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  // @bs.send.pipe(: t) external setArray: array<elt> => unit = "set"
  // @bs.send.pipe(: t) external setArrayOffset: (array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  // @bs.send.pipe(: t) external copyWithin: (~to_: int) => t = "copyWithin"
  // @bs.send.pipe(: t) external copyWithinFrom: (~to_: int, ~from: int) => t = "copyWithin"
  // @bs.send.pipe(: t)
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  external copyWithinFromRange: (~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  // @bs.send.pipe(: t) external fillInPlace: elt => t = "fill"
  // @bs.send.pipe(: t) external fillFromInPlace: (elt, ~from: int) => t = "fill"
  // @bs.send.pipe(: t) external fillRangeInPlace: (elt, ~start: int, ~end_: int) => t = "fill"

  // @bs.send.pipe(: t) external reverseInPlace: t = "reverse"

  // @bs.send.pipe(: t) external sortInPlace: t = "sort"
  // @bs.send.pipe(: t) external sortInPlaceWith: ((. elt, elt) => int) => t = "sort"

  /* Accessor functions */
  // @bs.send.pipe(: t) external includes: elt => bool = "includes" /* ES2016 */

  // @bs.send.pipe(: t) external indexOf: elt => int = "indexOf"
  // @bs.send.pipe(: t) external indexOfFrom: (elt, ~from: int) => int = "indexOf"

  // @bs.send.pipe(: t) external join: string = "join"
  // @bs.send.pipe(: t) external joinWith: string => string = "join"

  // @bs.send.pipe(: t) external lastIndexOf: elt => int = "lastIndexOf"
  // @bs.send.pipe(: t) external lastIndexOfFrom: (elt, ~from: int) => int = "lastIndexOf"

  @deprecated("Use `TypedArray.slice` instead.")
  external // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  slice: (~start: int, ~end_: int) => t = "slice"

  // @bs.send.pipe(: t) external copy: t = "slice"
  @deprecated("Use `TypedArray.sliceToEnd` instead.")
  external // @bs.send.pipe(: t) external sliceFrom: int => t = "slice"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  subarray: (~start: int, ~end_: int) => t = "subarray"

  // @bs.send.pipe(: t) external subarrayFrom: int => t = "subarray"

  // @bs.send.pipe(: t) external toString: string = "toString"
  // @bs.send.pipe(: t) external toLocaleString: string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@// @bs.send.pipe: t]
 */
  // @bs.send.pipe(: t) external every: ((. elt) => bool) => bool = "every"
  // @bs.send.pipe(: t) external everyi: ((. elt, int) => bool) => bool = "every"

  // @bs.send.pipe(: t) external filter: ((. elt) => bool) => t = "filter"
  // @bs.send.pipe(: t) external filteri: ((. elt, int) => bool) => t = "filter"

  // @bs.send.pipe(: t) external find: ((. elt) => bool) => Js.undefined<elt> = "find"
  // @bs.send.pipe(: t) external findi: ((. elt, int) => bool) => Js.undefined<elt> = "find"

  // @bs.send.pipe(: t) external findIndex: ((. elt) => bool) => int = "findIndex"
  // @bs.send.pipe(: t) external findIndexi: ((. elt, int) => bool) => int = "findIndex"

  // @bs.send.pipe(: t) external forEach: ((. elt) => unit) => unit = "forEach"
  // @bs.send.pipe(: t) external forEachi: ((. elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@// @bs.send.pipe: t]
 */

  // @bs.send.pipe(: t) external map: ((. elt) => 'b) => typed_array<'b> = "map"
  // @bs.send.pipe(: t) external mapi: ((. elt, int) => 'b) => typed_array<'b> = "map"

  // @bs.send.pipe(: t) external reduce: ((. 'b, elt) => 'b, 'b) => 'b = "reduce"
  // @bs.send.pipe(: t) external reducei: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduce"

  // @bs.send.pipe(: t) external reduceRight: ((. 'b, elt) => 'b, 'b) => 'b = "reduceRight"
  // @bs.send.pipe(: t) external reduceRighti: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  // @bs.send.pipe(: t) external some: ((. elt) => bool) => bool = "some"
  // @bs.send.pipe(: t) external somei: ((. elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Int8Array.Constants.bytesPerElement` instead.",
    migrate: Int8Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Int8Array.BYTES_PER_ELEMENT"

  @deprecated({
    reason: "Use `Int8Array.fromArray` instead.",
    migrate: Int8Array.fromArray(),
  })
  @new
  external make: array<elt> => t = "Int8Array"
  /** can throw */
  @new
  external fromBuffer: array_buffer => t = "Int8Array"

  /**
  throw Js.Exn.Error throw Js exception

  param offset is in bytes
  */
  @deprecated({
    reason: "Use `Int8Array.fromBufferToEnd` instead.",
    migrate: Int8Array.fromBufferToEnd(~byteOffset=%insert.unlabelledArgument(1)),
  })
  @new external fromBufferOffset: (array_buffer, int) => t = "Int8Array"

  /**
  throw Js.Exn.Error throws Js exception

  param offset is in bytes, length in elements
  */
  @deprecated({
    reason: "Use `Int8Array.fromBufferWithRange` instead.",
    migrate: Int8Array.fromBufferWithRange(
      ~byteOffset=%insert.labelledArgument("offset"),
      ~length=%insert.labelledArgument("length"),
    ),
  })
  @new
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Int8Array"

  @deprecated({
    reason: "Use `Int8Array.fromLength` instead.",
    migrate: Int8Array.fromLength(),
  })
  @new
  external fromLength: int => t = "Int8Array"
  @deprecated({
    reason: "Use `Int8Array.fromArrayLikeOrIterable` instead.",
    migrate: Int8Array.fromArrayLikeOrIterable(),
  })
  @val
  external from: array_like<elt> => t = "Int8Array.from"
  /* *Array.of is redundant, use make */
}

module Uint8Array = {
  /** */
  type elt = int
  type typed_array<'a> = Js_typed_array2.Uint8Array.typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  // @bs.send.pipe(: t) external setArray: array<elt> => unit = "set"
  // @bs.send.pipe(: t) external setArrayOffset: (array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  // @bs.send.pipe(: t) external copyWithin: (~to_: int) => t = "copyWithin"
  // @bs.send.pipe(: t) external copyWithinFrom: (~to_: int, ~from: int) => t = "copyWithin"
  // @bs.send.pipe(: t)
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  external copyWithinFromRange: (~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  // @bs.send.pipe(: t) external fillInPlace: elt => t = "fill"
  // @bs.send.pipe(: t) external fillFromInPlace: (elt, ~from: int) => t = "fill"
  // @bs.send.pipe(: t) external fillRangeInPlace: (elt, ~start: int, ~end_: int) => t = "fill"

  // @bs.send.pipe(: t) external reverseInPlace: t = "reverse"

  // @bs.send.pipe(: t) external sortInPlace: t = "sort"
  // @bs.send.pipe(: t) external sortInPlaceWith: ((. elt, elt) => int) => t = "sort"

  /* Accessor functions */
  // @bs.send.pipe(: t) external includes: elt => bool = "includes" /* ES2016 */

  // @bs.send.pipe(: t) external indexOf: elt => int = "indexOf"
  // @bs.send.pipe(: t) external indexOfFrom: (elt, ~from: int) => int = "indexOf"

  // @bs.send.pipe(: t) external join: string = "join"
  // @bs.send.pipe(: t) external joinWith: string => string = "join"

  // @bs.send.pipe(: t) external lastIndexOf: elt => int = "lastIndexOf"
  // @bs.send.pipe(: t) external lastIndexOfFrom: (elt, ~from: int) => int = "lastIndexOf"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  external slice: (~start: int, ~end_: int) => t = "slice"

  // @bs.send.pipe(: t) external copy: t = "slice"
  // @bs.send.pipe(: t) external sliceFrom: int => t = "slice"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  external subarray: (~start: int, ~end_: int) => t = "subarray"

  // @bs.send.pipe(: t) external subarrayFrom: int => t = "subarray"

  // @bs.send.pipe(: t) external toString: string = "toString"
  // @bs.send.pipe(: t) external toLocaleString: string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@// @bs.send.pipe: t]
 */
  // @bs.send.pipe(: t) external every: ((. elt) => bool) => bool = "every"
  // @bs.send.pipe(: t) external everyi: ((. elt, int) => bool) => bool = "every"

  // @bs.send.pipe(: t) external filter: ((. elt) => bool) => t = "filter"
  // @bs.send.pipe(: t) external filteri: ((. elt, int) => bool) => t = "filter"

  // @bs.send.pipe(: t) external find: ((. elt) => bool) => Js.undefined<elt> = "find"
  // @bs.send.pipe(: t) external findi: ((. elt, int) => bool) => Js.undefined<elt> = "find"

  // @bs.send.pipe(: t) external findIndex: ((. elt) => bool) => int = "findIndex"
  // @bs.send.pipe(: t) external findIndexi: ((. elt, int) => bool) => int = "findIndex"

  // @bs.send.pipe(: t) external forEach: ((. elt) => unit) => unit = "forEach"
  // @bs.send.pipe(: t) external forEachi: ((. elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@// @bs.send.pipe: t]
 */

  // @bs.send.pipe(: t) external map: ((. elt) => 'b) => typed_array<'b> = "map"
  // @bs.send.pipe(: t) external mapi: ((. elt, int) => 'b) => typed_array<'b> = "map"

  // @bs.send.pipe(: t) external reduce: ((. 'b, elt) => 'b, 'b) => 'b = "reduce"
  // @bs.send.pipe(: t) external reducei: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduce"

  // @bs.send.pipe(: t) external reduceRight: ((. 'b, elt) => 'b, 'b) => 'b = "reduceRight"
  // @bs.send.pipe(: t) external reduceRighti: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  // @bs.send.pipe(: t) external some: ((. elt) => bool) => bool = "some"
  // @bs.send.pipe(: t) external somei: ((. elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Uint8Array.Constants.bytesPerElement` instead.",
    migrate: Uint8Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Uint8Array.BYTES_PER_ELEMENT"

  @deprecated({
    reason: "Use `Uint8Array.fromArray` instead.",
    migrate: Uint8Array.fromArray(),
  })
  @new
  external make: array<elt> => t = "Uint8Array"
  /** can throw */
  @new
  external fromBuffer: array_buffer => t = "Uint8Array"

  /**
  **throw** Js.Exn.Error throw Js exception

  **param** offset is in bytes
  */
  @deprecated({
    reason: "Use `Uint8Array.fromBufferToEnd` instead.",
    migrate: Uint8Array.fromBufferToEnd(~byteOffset=%insert.unlabelledArgument(1)),
  })
  @new external fromBufferOffset: (array_buffer, int) => t = "Uint8Array"

  /**
  **throw** Js.Exn.Error throws Js exception

  **param** offset is in bytes, length in elements
  */
  @deprecated({
    reason: "Use `Uint8Array.fromBufferWithRange` instead.",
    migrate: Uint8Array.fromBufferWithRange(
      ~byteOffset=%insert.labelledArgument("offset"),
      ~length=%insert.labelledArgument("length"),
    ),
  })
  @new
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Uint8Array"

  @deprecated({
    reason: "Use `Uint8Array.fromLength` instead.",
    migrate: Uint8Array.fromLength(),
  })
  @new
  external fromLength: int => t = "Uint8Array"
  @deprecated({
    reason: "Use `Uint8Array.fromArrayLikeOrIterable` instead.",
    migrate: Uint8Array.fromArrayLikeOrIterable(),
  })
  @val
  external from: array_like<elt> => t = "Uint8Array.from"
  /* *Array.of is redundant, use make */
}

module Uint8ClampedArray = {
  /** */
  type elt = int
  type typed_array<'a> = Js_typed_array2.Uint8ClampedArray.typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  // @bs.send.pipe(: t) external setArray: array<elt> => unit = "set"
  // @bs.send.pipe(: t) external setArrayOffset: (array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  // @bs.send.pipe(: t) external copyWithin: (~to_: int) => t = "copyWithin"
  // @bs.send.pipe(: t) external copyWithinFrom: (~to_: int, ~from: int) => t = "copyWithin"
  // @bs.send.pipe(: t)
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  external copyWithinFromRange: (~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  // @bs.send.pipe(: t) external fillInPlace: elt => t = "fill"
  // @bs.send.pipe(: t) external fillFromInPlace: (elt, ~from: int) => t = "fill"
  // @bs.send.pipe(: t) external fillRangeInPlace: (elt, ~start: int, ~end_: int) => t = "fill"

  // @bs.send.pipe(: t) external reverseInPlace: t = "reverse"

  // @bs.send.pipe(: t) external sortInPlace: t = "sort"
  // @bs.send.pipe(: t) external sortInPlaceWith: ((. elt, elt) => int) => t = "sort"

  /* Accessor functions */
  // @bs.send.pipe(: t) external includes: elt => bool = "includes" /* ES2016 */

  // @bs.send.pipe(: t) external indexOf: elt => int = "indexOf"
  // @bs.send.pipe(: t) external indexOfFrom: (elt, ~from: int) => int = "indexOf"

  // @bs.send.pipe(: t) external join: string = "join"
  // @bs.send.pipe(: t) external joinWith: string => string = "join"

  // @bs.send.pipe(: t) external lastIndexOf: elt => int = "lastIndexOf"
  // @bs.send.pipe(: t) external lastIndexOfFrom: (elt, ~from: int) => int = "lastIndexOf"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  external slice: (~start: int, ~end_: int) => t = "slice"

  // @bs.send.pipe(: t) external copy: t = "slice"
  // @bs.send.pipe(: t) external sliceFrom: int => t = "slice"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  external subarray: (~start: int, ~end_: int) => t = "subarray"

  // @bs.send.pipe(: t) external subarrayFrom: int => t = "subarray"

  // @bs.send.pipe(: t) external toString: string = "toString"
  // @bs.send.pipe(: t) external toLocaleString: string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@// @bs.send.pipe: t]
 */
  // @bs.send.pipe(: t) external every: ((. elt) => bool) => bool = "every"
  // @bs.send.pipe(: t) external everyi: ((. elt, int) => bool) => bool = "every"

  // @bs.send.pipe(: t) external filter: ((. elt) => bool) => t = "filter"
  // @bs.send.pipe(: t) external filteri: ((. elt, int) => bool) => t = "filter"

  // @bs.send.pipe(: t) external find: ((. elt) => bool) => Js.undefined<elt> = "find"
  // @bs.send.pipe(: t) external findi: ((. elt, int) => bool) => Js.undefined<elt> = "find"

  // @bs.send.pipe(: t) external findIndex: ((. elt) => bool) => int = "findIndex"
  // @bs.send.pipe(: t) external findIndexi: ((. elt, int) => bool) => int = "findIndex"

  // @bs.send.pipe(: t) external forEach: ((. elt) => unit) => unit = "forEach"
  // @bs.send.pipe(: t) external forEachi: ((. elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@// @bs.send.pipe: t]
 */

  // @bs.send.pipe(: t) external map: ((. elt) => 'b) => typed_array<'b> = "map"
  // @bs.send.pipe(: t) external mapi: ((. elt, int) => 'b) => typed_array<'b> = "map"

  // @bs.send.pipe(: t) external reduce: ((. 'b, elt) => 'b, 'b) => 'b = "reduce"
  // @bs.send.pipe(: t) external reducei: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduce"

  // @bs.send.pipe(: t) external reduceRight: ((. 'b, elt) => 'b, 'b) => 'b = "reduceRight"
  // @bs.send.pipe(: t) external reduceRighti: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  // @bs.send.pipe(: t) external some: ((. elt) => bool) => bool = "some"
  // @bs.send.pipe(: t) external somei: ((. elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Uint8ClampedArray.Constants.bytesPerElement` instead.",
    migrate: Uint8ClampedArray.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Uint8ClampedArray.BYTES_PER_ELEMENT"

  @deprecated({
    reason: "Use `Uint8ClampedArray.fromArray` instead.",
    migrate: Uint8ClampedArray.fromArray(),
  })
  @new
  external make: array<elt> => t = "Uint8ClampedArray"
  /** can throw */
  @new
  external fromBuffer: array_buffer => t = "Uint8ClampedArray"

  /**
  **throw** Js.Exn.Error throw Js exception

  **param** offset is in bytes
  */
  @deprecated({
    reason: "Use `Uint8ClampedArray.fromBufferToEnd` instead.",
    migrate: Uint8ClampedArray.fromBufferToEnd(~byteOffset=%insert.unlabelledArgument(1)),
  })
  @new external fromBufferOffset: (array_buffer, int) => t = "Uint8ClampedArray"

  /**
  **throw** Js.Exn.Error throws Js exception

  **param** offset is in bytes, length in elements
  */
  @deprecated({
    reason: "Use `Uint8ClampedArray.fromBufferWithRange` instead.",
    migrate: Uint8ClampedArray.fromBufferWithRange(
      ~byteOffset=%insert.labelledArgument("offset"),
      ~length=%insert.labelledArgument("length"),
    ),
  })
  @new
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Uint8ClampedArray"

  @deprecated({
    reason: "Use `Uint8ClampedArray.fromLength` instead.",
    migrate: Uint8ClampedArray.fromLength(),
  })
  @new
  external fromLength: int => t = "Uint8ClampedArray"
  @deprecated({
    reason: "Use `Uint8ClampedArray.fromArrayLikeOrIterable` instead.",
    migrate: Uint8ClampedArray.fromArrayLikeOrIterable(),
  })
  @val
  external from: array_like<elt> => t = "Uint8ClampedArray.from"
  /* *Array.of is redundant, use make */
}

module Int16Array = {
  /** */
  type elt = int
  type typed_array<'a> = Js_typed_array2.Int16Array.typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  // @bs.send.pipe(: t) external setArray: array<elt> => unit = "set"
  // @bs.send.pipe(: t) external setArrayOffset: (array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  // @bs.send.pipe(: t) external copyWithin: (~to_: int) => t = "copyWithin"
  // @bs.send.pipe(: t) external copyWithinFrom: (~to_: int, ~from: int) => t = "copyWithin"
  // @bs.send.pipe(: t)
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  external copyWithinFromRange: (~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  // @bs.send.pipe(: t) external fillInPlace: elt => t = "fill"
  // @bs.send.pipe(: t) external fillFromInPlace: (elt, ~from: int) => t = "fill"
  // @bs.send.pipe(: t) external fillRangeInPlace: (elt, ~start: int, ~end_: int) => t = "fill"

  // @bs.send.pipe(: t) external reverseInPlace: t = "reverse"

  // @bs.send.pipe(: t) external sortInPlace: t = "sort"
  // @bs.send.pipe(: t) external sortInPlaceWith: ((. elt, elt) => int) => t = "sort"

  /* Accessor functions */
  // @bs.send.pipe(: t) external includes: elt => bool = "includes" /* ES2016 */

  // @bs.send.pipe(: t) external indexOf: elt => int = "indexOf"
  // @bs.send.pipe(: t) external indexOfFrom: (elt, ~from: int) => int = "indexOf"

  // @bs.send.pipe(: t) external join: string = "join"
  // @bs.send.pipe(: t) external joinWith: string => string = "join"

  // @bs.send.pipe(: t) external lastIndexOf: elt => int = "lastIndexOf"
  // @bs.send.pipe(: t) external lastIndexOfFrom: (elt, ~from: int) => int = "lastIndexOf"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  external slice: (~start: int, ~end_: int) => t = "slice"

  // @bs.send.pipe(: t) external copy: t = "slice"
  // @bs.send.pipe(: t) external sliceFrom: int => t = "slice"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  external subarray: (~start: int, ~end_: int) => t = "subarray"

  // @bs.send.pipe(: t) external subarrayFrom: int => t = "subarray"

  // @bs.send.pipe(: t) external toString: string = "toString"
  // @bs.send.pipe(: t) external toLocaleString: string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@// @bs.send.pipe: t]
 */
  // @bs.send.pipe(: t) external every: ((. elt) => bool) => bool = "every"
  // @bs.send.pipe(: t) external everyi: ((. elt, int) => bool) => bool = "every"

  // @bs.send.pipe(: t) external filter: ((. elt) => bool) => t = "filter"
  // @bs.send.pipe(: t) external filteri: ((. elt, int) => bool) => t = "filter"

  // @bs.send.pipe(: t) external find: ((. elt) => bool) => Js.undefined<elt> = "find"
  // @bs.send.pipe(: t) external findi: ((. elt, int) => bool) => Js.undefined<elt> = "find"

  // @bs.send.pipe(: t) external findIndex: ((. elt) => bool) => int = "findIndex"
  // @bs.send.pipe(: t) external findIndexi: ((. elt, int) => bool) => int = "findIndex"

  // @bs.send.pipe(: t) external forEach: ((. elt) => unit) => unit = "forEach"
  // @bs.send.pipe(: t) external forEachi: ((. elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@// @bs.send.pipe: t]
 */

  // @bs.send.pipe(: t) external map: ((. elt) => 'b) => typed_array<'b> = "map"
  // @bs.send.pipe(: t) external mapi: ((. elt, int) => 'b) => typed_array<'b> = "map"

  // @bs.send.pipe(: t) external reduce: ((. 'b, elt) => 'b, 'b) => 'b = "reduce"
  // @bs.send.pipe(: t) external reducei: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduce"

  // @bs.send.pipe(: t) external reduceRight: ((. 'b, elt) => 'b, 'b) => 'b = "reduceRight"
  // @bs.send.pipe(: t) external reduceRighti: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  // @bs.send.pipe(: t) external some: ((. elt) => bool) => bool = "some"
  // @bs.send.pipe(: t) external somei: ((. elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Int16Array.Constants.bytesPerElement` instead.",
    migrate: Int16Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Int16Array.BYTES_PER_ELEMENT"

  @deprecated({
    reason: "Use `Int16Array.fromArray` instead.",
    migrate: Int16Array.fromArray(),
  })
  @new
  external make: array<elt> => t = "Int16Array"
  /** can throw */
  @new
  @deprecated({
    reason: "Use `Int16Array.fromBuffer` instead.",
    migrate: Int16Array.fromBuffer(),
  })
  external fromBuffer: array_buffer => t = "Int16Array"

  /**
  **throw** Js.Exn.Error throw Js exception

  **param** offset is in bytes
  */
  @new
  @deprecated({
    reason: "Use `Int16Array.fromBufferToEnd` instead.",
    migrate: Int16Array.fromBufferToEnd(~byteOffset=%insert.unlabelledArgument(1)),
  })
  external fromBufferOffset: (array_buffer, int) => t = "Int16Array"

  /**
  **throw** Js.Exn.Error throws Js exception

  **param** offset is in bytes, length in elements
  */
  @new
  @deprecated({
    reason: "Use `Int16Array.fromBufferWithRange` instead.",
    migrate: Int16Array.fromBufferWithRange(
      ~byteOffset=%insert.labelledArgument("offset"),
      ~length=%insert.labelledArgument("length"),
    ),
  })
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Int16Array"

  @deprecated({
    reason: "Use `Int16Array.fromLength` instead.",
    migrate: Int16Array.fromLength(),
  })
  @new
  external fromLength: int => t = "Int16Array"
  @deprecated({
    reason: "Use `Int16Array.fromArrayLikeOrIterable` instead.",
    migrate: Int16Array.fromArrayLikeOrIterable(),
  })
  @val
  external from: array_like<elt> => t = "Int16Array.from"
  /* *Array.of is redundant, use make */
}

module Uint16Array = {
  /** */
  type elt = int
  type typed_array<'a> = Js_typed_array2.Uint16Array.typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  // @bs.send.pipe(: t) external setArray: array<elt> => unit = "set"
  // @bs.send.pipe(: t) external setArrayOffset: (array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  // @bs.send.pipe(: t) external copyWithin: (~to_: int) => t = "copyWithin"
  // @bs.send.pipe(: t) external copyWithinFrom: (~to_: int, ~from: int) => t = "copyWithin"
  // @bs.send.pipe(: t)
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  external copyWithinFromRange: (~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  // @bs.send.pipe(: t) external fillInPlace: elt => t = "fill"
  // @bs.send.pipe(: t) external fillFromInPlace: (elt, ~from: int) => t = "fill"
  // @bs.send.pipe(: t) external fillRangeInPlace: (elt, ~start: int, ~end_: int) => t = "fill"

  // @bs.send.pipe(: t) external reverseInPlace: t = "reverse"

  // @bs.send.pipe(: t) external sortInPlace: t = "sort"
  // @bs.send.pipe(: t) external sortInPlaceWith: ((. elt, elt) => int) => t = "sort"

  /* Accessor functions */
  // @bs.send.pipe(: t) external includes: elt => bool = "includes" /* ES2016 */

  // @bs.send.pipe(: t) external indexOf: elt => int = "indexOf"
  // @bs.send.pipe(: t) external indexOfFrom: (elt, ~from: int) => int = "indexOf"

  // @bs.send.pipe(: t) external join: string = "join"
  // @bs.send.pipe(: t) external joinWith: string => string = "join"

  // @bs.send.pipe(: t) external lastIndexOf: elt => int = "lastIndexOf"
  // @bs.send.pipe(: t) external lastIndexOfFrom: (elt, ~from: int) => int = "lastIndexOf"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  external slice: (~start: int, ~end_: int) => t = "slice"

  // @bs.send.pipe(: t) external copy: t = "slice"
  // @bs.send.pipe(: t) external sliceFrom: int => t = "slice"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  external subarray: (~start: int, ~end_: int) => t = "subarray"

  // @bs.send.pipe(: t) external subarrayFrom: int => t = "subarray"

  // @bs.send.pipe(: t) external toString: string = "toString"
  // @bs.send.pipe(: t) external toLocaleString: string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@// @bs.send.pipe: t]
 */
  // @bs.send.pipe(: t) external every: ((. elt) => bool) => bool = "every"
  // @bs.send.pipe(: t) external everyi: ((. elt, int) => bool) => bool = "every"

  // @bs.send.pipe(: t) external filter: ((. elt) => bool) => t = "filter"
  // @bs.send.pipe(: t) external filteri: ((. elt, int) => bool) => t = "filter"

  // @bs.send.pipe(: t) external find: ((. elt) => bool) => Js.undefined<elt> = "find"
  // @bs.send.pipe(: t) external findi: ((. elt, int) => bool) => Js.undefined<elt> = "find"

  // @bs.send.pipe(: t) external findIndex: ((. elt) => bool) => int = "findIndex"
  // @bs.send.pipe(: t) external findIndexi: ((. elt, int) => bool) => int = "findIndex"

  // @bs.send.pipe(: t) external forEach: ((. elt) => unit) => unit = "forEach"
  // @bs.send.pipe(: t) external forEachi: ((. elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@// @bs.send.pipe: t]
 */

  // @bs.send.pipe(: t) external map: ((. elt) => 'b) => typed_array<'b> = "map"
  // @bs.send.pipe(: t) external mapi: ((. elt, int) => 'b) => typed_array<'b> = "map"

  // @bs.send.pipe(: t) external reduce: ((. 'b, elt) => 'b, 'b) => 'b = "reduce"
  // @bs.send.pipe(: t) external reducei: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduce"

  // @bs.send.pipe(: t) external reduceRight: ((. 'b, elt) => 'b, 'b) => 'b = "reduceRight"
  // @bs.send.pipe(: t) external reduceRighti: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  // @bs.send.pipe(: t) external some: ((. elt) => bool) => bool = "some"
  // @bs.send.pipe(: t) external somei: ((. elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Uint16Array.Constants.bytesPerElement` instead.",
    migrate: Uint16Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Uint16Array.BYTES_PER_ELEMENT"

  @deprecated({
    reason: "Use `Uint16Array.fromArray` instead.",
    migrate: Uint16Array.fromArray(),
  })
  @new
  external make: array<elt> => t = "Uint16Array"
  /** can throw */
  @new
  @deprecated({
    reason: "Use `Uint16Array.fromBuffer` instead.",
    migrate: Uint16Array.fromBuffer(),
  })
  external fromBuffer: array_buffer => t = "Uint16Array"

  /**
  **throw** Js.Exn.Error throw Js exception

  **param** offset is in bytes
  */
  @new
  @deprecated({
    reason: "Use `Uint16Array.fromBufferToEnd` instead.",
    migrate: Uint16Array.fromBufferToEnd(~byteOffset=%insert.unlabelledArgument(1)),
  })
  external fromBufferOffset: (array_buffer, int) => t = "Uint16Array"

  /**
  **throw** Js.Exn.Error throws Js exception

  **param** offset is in bytes, length in elements
  */
  @new
  @deprecated({
    reason: "Use `Uint16Array.fromBufferWithRange` instead.",
    migrate: Uint16Array.fromBufferWithRange(
      ~byteOffset=%insert.labelledArgument("offset"),
      ~length=%insert.labelledArgument("length"),
    ),
  })
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Uint16Array"

  @deprecated({
    reason: "Use `Uint16Array.fromLength` instead.",
    migrate: Uint16Array.fromLength(),
  })
  @new
  external fromLength: int => t = "Uint16Array"
  @deprecated({
    reason: "Use `Uint16Array.fromArrayLikeOrIterable` instead.",
    migrate: Uint16Array.fromArrayLikeOrIterable(),
  })
  @val
  external from: array_like<elt> => t = "Uint16Array.from"
  /* *Array.of is redundant, use make */
}

module Int32Array = {
  /** */
  type elt = int
  type typed_array<'a> = Js_typed_array2.Int32Array.typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  // @bs.send.pipe(: t) external setArray: array<elt> => unit = "set"
  // @bs.send.pipe(: t) external setArrayOffset: (array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  // @bs.send.pipe(: t) external copyWithin: (~to_: int) => t = "copyWithin"
  // @bs.send.pipe(: t) external copyWithinFrom: (~to_: int, ~from: int) => t = "copyWithin"
  // @bs.send.pipe(: t)
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  external copyWithinFromRange: (~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  // @bs.send.pipe(: t) external fillInPlace: elt => t = "fill"
  // @bs.send.pipe(: t) external fillFromInPlace: (elt, ~from: int) => t = "fill"
  // @bs.send.pipe(: t) external fillRangeInPlace: (elt, ~start: int, ~end_: int) => t = "fill"

  // @bs.send.pipe(: t) external reverseInPlace: t = "reverse"

  // @bs.send.pipe(: t) external sortInPlace: t = "sort"
  // @bs.send.pipe(: t) external sortInPlaceWith: ((. elt, elt) => int) => t = "sort"

  /* Accessor functions */
  // @bs.send.pipe(: t) external includes: elt => bool = "includes" /* ES2016 */

  // @bs.send.pipe(: t) external indexOf: elt => int = "indexOf"
  // @bs.send.pipe(: t) external indexOfFrom: (elt, ~from: int) => int = "indexOf"

  // @bs.send.pipe(: t) external join: string = "join"
  // @bs.send.pipe(: t) external joinWith: string => string = "join"

  // @bs.send.pipe(: t) external lastIndexOf: elt => int = "lastIndexOf"
  // @bs.send.pipe(: t) external lastIndexOfFrom: (elt, ~from: int) => int = "lastIndexOf"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  external slice: (~start: int, ~end_: int) => t = "slice"

  // @bs.send.pipe(: t) external copy: t = "slice"
  // @bs.send.pipe(: t) external sliceFrom: int => t = "slice"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  external subarray: (~start: int, ~end_: int) => t = "subarray"

  // @bs.send.pipe(: t) external subarrayFrom: int => t = "subarray"

  // @bs.send.pipe(: t) external toString: string = "toString"
  // @bs.send.pipe(: t) external toLocaleString: string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@// @bs.send.pipe: t]
 */
  // @bs.send.pipe(: t) external every: ((. elt) => bool) => bool = "every"
  // @bs.send.pipe(: t) external everyi: ((. elt, int) => bool) => bool = "every"

  // @bs.send.pipe(: t) external filter: ((. elt) => bool) => t = "filter"
  // @bs.send.pipe(: t) external filteri: ((. elt, int) => bool) => t = "filter"

  // @bs.send.pipe(: t) external find: ((. elt) => bool) => Js.undefined<elt> = "find"
  // @bs.send.pipe(: t) external findi: ((. elt, int) => bool) => Js.undefined<elt> = "find"

  // @bs.send.pipe(: t) external findIndex: ((. elt) => bool) => int = "findIndex"
  // @bs.send.pipe(: t) external findIndexi: ((. elt, int) => bool) => int = "findIndex"

  // @bs.send.pipe(: t) external forEach: ((. elt) => unit) => unit = "forEach"
  // @bs.send.pipe(: t) external forEachi: ((. elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@// @bs.send.pipe: t]
 */

  // @bs.send.pipe(: t) external map: ((. elt) => 'b) => typed_array<'b> = "map"
  // @bs.send.pipe(: t) external mapi: ((. elt, int) => 'b) => typed_array<'b> = "map"

  // @bs.send.pipe(: t) external reduce: ((. 'b, elt) => 'b, 'b) => 'b = "reduce"
  // @bs.send.pipe(: t) external reducei: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduce"

  // @bs.send.pipe(: t) external reduceRight: ((. 'b, elt) => 'b, 'b) => 'b = "reduceRight"
  // @bs.send.pipe(: t) external reduceRighti: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  // @bs.send.pipe(: t) external some: ((. elt) => bool) => bool = "some"
  // @bs.send.pipe(: t) external somei: ((. elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Int32Array.Constants.bytesPerElement` instead.",
    migrate: Int32Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Int32Array.BYTES_PER_ELEMENT"

  @deprecated({
    reason: "Use `Int32Array.fromArray` instead.",
    migrate: Int32Array.fromArray(),
  })
  @new
  external make: array<elt> => t = "Int32Array"
  /** can throw */
  @new
  @deprecated({
    reason: "Use `Int32Array.fromBuffer` instead.",
    migrate: Int32Array.fromBuffer(),
  })
  external fromBuffer: array_buffer => t = "Int32Array"

  /**
  **throw** Js.Exn.Error throw Js exception

  **param** offset is in bytes
  */
  @new
  @deprecated({
    reason: "Use `Int32Array.fromBufferToEnd",
    migrate: Int32Array.fromBufferToEnd(~byteOffset=%insert.unlabelledArgument(1)),
  })
  external fromBufferOffset: (array_buffer, int) => t = "Int32Array"

  /**
  **throw** Js.Exn.Error throws Js exception

  **param** offset is in bytes, length in elements
  */
  @new
  @deprecated({
    reason: "Use `Int32Array.fromBufferWithRange` instead.",
    migrate: Int32Array.fromBufferWithRange(
      ~byteOffset=%insert.labelledArgument("offset"),
      ~length=%insert.labelledArgument("length"),
    ),
  })
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Int32Array"

  @deprecated({
    reason: "Use `Int32Array.fromLength` instead.",
    migrate: Int32Array.fromLength(),
  })
  @new
  external fromLength: int => t = "Int32Array"
  @deprecated({
    reason: "Use `Int32Array.fromArrayLikeOrIterable` instead.",
    migrate: Int32Array.fromArrayLikeOrIterable(),
  })
  @val
  external from: array_like<elt> => t = "Int32Array.from"
  /* *Array.of is redundant, use make */
  @new @deprecated("use `make` instead") external create: array<int> => t = "Int32Array"
  @new @deprecated("use `fromBuffer` instead") external of_buffer: array_buffer => t = "Int32Array"
}
module Int32_array = Int32Array

module Uint32Array = {
  /** */
  type elt = int
  type typed_array<'a> = Js_typed_array2.Uint32Array.typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  // @bs.send.pipe(: t) external setArray: array<elt> => unit = "set"
  // @bs.send.pipe(: t) external setArrayOffset: (array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  // @bs.send.pipe(: t) external copyWithin: (~to_: int) => t = "copyWithin"
  // @bs.send.pipe(: t) external copyWithinFrom: (~to_: int, ~from: int) => t = "copyWithin"
  // @bs.send.pipe(: t)
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  external copyWithinFromRange: (~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  // @bs.send.pipe(: t) external fillInPlace: elt => t = "fill"
  // @bs.send.pipe(: t) external fillFromInPlace: (elt, ~from: int) => t = "fill"
  // @bs.send.pipe(: t) external fillRangeInPlace: (elt, ~start: int, ~end_: int) => t = "fill"

  // @bs.send.pipe(: t) external reverseInPlace: t = "reverse"

  // @bs.send.pipe(: t) external sortInPlace: t = "sort"
  // @bs.send.pipe(: t) external sortInPlaceWith: ((. elt, elt) => int) => t = "sort"

  /* Accessor functions */
  // @bs.send.pipe(: t) external includes: elt => bool = "includes" /* ES2016 */

  // @bs.send.pipe(: t) external indexOf: elt => int = "indexOf"
  // @bs.send.pipe(: t) external indexOfFrom: (elt, ~from: int) => int = "indexOf"

  // @bs.send.pipe(: t) external join: string = "join"
  // @bs.send.pipe(: t) external joinWith: string => string = "join"

  // @bs.send.pipe(: t) external lastIndexOf: elt => int = "lastIndexOf"
  // @bs.send.pipe(: t) external lastIndexOfFrom: (elt, ~from: int) => int = "lastIndexOf"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  external slice: (~start: int, ~end_: int) => t = "slice"

  // @bs.send.pipe(: t) external copy: t = "slice"
  // @bs.send.pipe(: t) external sliceFrom: int => t = "slice"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  external subarray: (~start: int, ~end_: int) => t = "subarray"

  // @bs.send.pipe(: t) external subarrayFrom: int => t = "subarray"

  // @bs.send.pipe(: t) external toString: string = "toString"
  // @bs.send.pipe(: t) external toLocaleString: string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@// @bs.send.pipe: t]
 */
  // @bs.send.pipe(: t) external every: ((. elt) => bool) => bool = "every"
  // @bs.send.pipe(: t) external everyi: ((. elt, int) => bool) => bool = "every"

  // @bs.send.pipe(: t) external filter: ((. elt) => bool) => t = "filter"
  // @bs.send.pipe(: t) external filteri: ((. elt, int) => bool) => t = "filter"

  // @bs.send.pipe(: t) external find: ((. elt) => bool) => Js.undefined<elt> = "find"
  // @bs.send.pipe(: t) external findi: ((. elt, int) => bool) => Js.undefined<elt> = "find"

  // @bs.send.pipe(: t) external findIndex: ((. elt) => bool) => int = "findIndex"
  // @bs.send.pipe(: t) external findIndexi: ((. elt, int) => bool) => int = "findIndex"

  // @bs.send.pipe(: t) external forEach: ((. elt) => unit) => unit = "forEach"
  // @bs.send.pipe(: t) external forEachi: ((. elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@// @bs.send.pipe: t]
 */

  // @bs.send.pipe(: t) external map: ((. elt) => 'b) => typed_array<'b> = "map"
  // @bs.send.pipe(: t) external mapi: ((. elt, int) => 'b) => typed_array<'b> = "map"

  // @bs.send.pipe(: t) external reduce: ((. 'b, elt) => 'b, 'b) => 'b = "reduce"
  // @bs.send.pipe(: t) external reducei: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduce"

  // @bs.send.pipe(: t) external reduceRight: ((. 'b, elt) => 'b, 'b) => 'b = "reduceRight"
  // @bs.send.pipe(: t) external reduceRighti: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  // @bs.send.pipe(: t) external some: ((. elt) => bool) => bool = "some"
  // @bs.send.pipe(: t) external somei: ((. elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Uint32Array.Constants.bytesPerElement` instead.",
    migrate: Uint32Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Uint32Array.BYTES_PER_ELEMENT"

  @deprecated({
    reason: "Use `Uint32Array.fromArray` instead.",
    migrate: Uint32Array.fromArray(),
  })
  @new
  external make: array<elt> => t = "Uint32Array"
  /** can throw */
  @new
  @deprecated({
    reason: "Use `Uint32Array.fromBuffer` instead.",
    migrate: Uint32Array.fromBuffer(),
  })
  external fromBuffer: array_buffer => t = "Uint32Array"

  /**
  **throw** Js.Exn.Error throw Js exception

  **param** offset is in bytes
  */
  @new
  @deprecated({
    reason: "Use `Uint32Array.fromBufferToEnd` instead.",
    migrate: Uint32Array.fromBufferToEnd(~byteOffset=%insert.unlabelledArgument(1)),
  })
  external fromBufferOffset: (array_buffer, int) => t = "Uint32Array"

  /**
  **throw** Js.Exn.Error throws Js exception

  **param** offset is in bytes, length in elements
  */
  @new
  @deprecated({
    reason: "Use `Uint32Array.fromBufferWithRange` instead.",
    migrate: Uint32Array.fromBufferWithRange(
      ~byteOffset=%insert.labelledArgument("offset"),
      ~length=%insert.labelledArgument("length"),
    ),
  })
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Uint32Array"

  @deprecated({
    reason: "Use `Uint32Array.fromLength` instead.",
    migrate: Uint32Array.fromLength(),
  })
  @new
  external fromLength: int => t = "Uint32Array"
  @deprecated({
    reason: "Use `Uint32Array.fromArrayLikeOrIterable` instead.",
    migrate: Uint32Array.fromArrayLikeOrIterable(),
  })
  @val
  external from: array_like<elt> => t = "Uint32Array.from"
  /* *Array.of is redundant, use make */
}

/*
 it still return number, `float` in this case
*/
module Float32Array = {
  /** */
  type elt = float
  type typed_array<'a> = Js_typed_array2.Float32Array.typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  // @bs.send.pipe(: t) external setArray: array<elt> => unit = "set"
  // @bs.send.pipe(: t) external setArrayOffset: (array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  // @bs.send.pipe(: t) external copyWithin: (~to_: int) => t = "copyWithin"
  // @bs.send.pipe(: t) external copyWithinFrom: (~to_: int, ~from: int) => t = "copyWithin"
  // @bs.send.pipe(: t)
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  external copyWithinFromRange: (~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  // @bs.send.pipe(: t) external fillInPlace: elt => t = "fill"
  // @bs.send.pipe(: t) external fillFromInPlace: (elt, ~from: int) => t = "fill"
  // @bs.send.pipe(: t) external fillRangeInPlace: (elt, ~start: int, ~end_: int) => t = "fill"

  // @bs.send.pipe(: t) external reverseInPlace: t = "reverse"

  // @bs.send.pipe(: t) external sortInPlace: t = "sort"
  // @bs.send.pipe(: t) external sortInPlaceWith: ((. elt, elt) => int) => t = "sort"

  /* Accessor functions */
  // @bs.send.pipe(: t) external includes: elt => bool = "includes" /* ES2016 */

  // @bs.send.pipe(: t) external indexOf: elt => int = "indexOf"
  // @bs.send.pipe(: t) external indexOfFrom: (elt, ~from: int) => int = "indexOf"

  // @bs.send.pipe(: t) external join: string = "join"
  // @bs.send.pipe(: t) external joinWith: string => string = "join"

  // @bs.send.pipe(: t) external lastIndexOf: elt => int = "lastIndexOf"
  // @bs.send.pipe(: t) external lastIndexOfFrom: (elt, ~from: int) => int = "lastIndexOf"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  external slice: (~start: int, ~end_: int) => t = "slice"

  // @bs.send.pipe(: t) external copy: t = "slice"
  // @bs.send.pipe(: t) external sliceFrom: int => t = "slice"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  external subarray: (~start: int, ~end_: int) => t = "subarray"

  // @bs.send.pipe(: t) external subarrayFrom: int => t = "subarray"

  // @bs.send.pipe(: t) external toString: string = "toString"
  // @bs.send.pipe(: t) external toLocaleString: string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@// @bs.send.pipe: t]
 */
  // @bs.send.pipe(: t) external every: ((. elt) => bool) => bool = "every"
  // @bs.send.pipe(: t) external everyi: ((. elt, int) => bool) => bool = "every"

  // @bs.send.pipe(: t) external filter: ((. elt) => bool) => t = "filter"
  // @bs.send.pipe(: t) external filteri: ((. elt, int) => bool) => t = "filter"

  // @bs.send.pipe(: t) external find: ((. elt) => bool) => Js.undefined<elt> = "find"
  // @bs.send.pipe(: t) external findi: ((. elt, int) => bool) => Js.undefined<elt> = "find"

  // @bs.send.pipe(: t) external findIndex: ((. elt) => bool) => int = "findIndex"
  // @bs.send.pipe(: t) external findIndexi: ((. elt, int) => bool) => int = "findIndex"

  // @bs.send.pipe(: t) external forEach: ((. elt) => unit) => unit = "forEach"
  // @bs.send.pipe(: t) external forEachi: ((. elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@// @bs.send.pipe: t]
 */

  // @bs.send.pipe(: t) external map: ((. elt) => 'b) => typed_array<'b> = "map"
  // @bs.send.pipe(: t) external mapi: ((. elt, int) => 'b) => typed_array<'b> = "map"

  // @bs.send.pipe(: t) external reduce: ((. 'b, elt) => 'b, 'b) => 'b = "reduce"
  // @bs.send.pipe(: t) external reducei: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduce"

  // @bs.send.pipe(: t) external reduceRight: ((. 'b, elt) => 'b, 'b) => 'b = "reduceRight"
  // @bs.send.pipe(: t) external reduceRighti: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  // @bs.send.pipe(: t) external some: ((. elt) => bool) => bool = "some"
  // @bs.send.pipe(: t) external somei: ((. elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Float32Array.Constants.bytesPerElement` instead.",
    migrate: Float32Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Float32Array.BYTES_PER_ELEMENT"

  @deprecated({
    reason: "Use `Float32Array.fromArray` instead.",
    migrate: Float32Array.fromArray(),
  })
  @new
  external make: array<elt> => t = "Float32Array"
  /** can throw */
  @new
  @deprecated({
    reason: "Use `Float32Array.fromBuffer` instead.",
    migrate: Float32Array.fromBuffer(),
  })
  external fromBuffer: array_buffer => t = "Float32Array"

  /**
  **throw** Js.Exn.Error throw Js exception

  **param** offset is in bytes
  */
  @new
  @deprecated({
    reason: "Use `Float32Array.fromBufferToEnd` instead.",
    migrate: Float32Array.fromBufferToEnd(~byteOffset=%insert.unlabelledArgument(1)),
  })
  external fromBufferOffset: (array_buffer, int) => t = "Float32Array"

  /**
  **throw** Js.Exn.Error throws Js exception

  **param** offset is in bytes, length in elements
  */
  @new
  @deprecated({
    reason: "Use `Float32Array.fromBufferWithRange` instead.",
    migrate: Float32Array.fromBufferWithRange(
      ~byteOffset=%insert.labelledArgument("offset"),
      ~length=%insert.labelledArgument("length"),
    ),
  })
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Float32Array"

  @deprecated({
    reason: "Use `Float32Array.fromLength` instead.",
    migrate: Float32Array.fromLength(),
  })
  @new
  external fromLength: int => t = "Float32Array"
  @deprecated({
    reason: "Use `Float32Array.fromArrayLikeOrIterable` instead.",
    migrate: Float32Array.fromArrayLikeOrIterable(),
  })
  @val
  external from: array_like<elt> => t = "Float32Array.from"
  /* *Array.of is redundant, use make */
  @new @deprecated("use `make` instead") external create: array<float> => t = "Float32Array"
  @new @deprecated("use `fromBuffer` instead")
  external of_buffer: array_buffer => t = "Float32Array"
}
module Float32_array = Float32Array

module Float64Array = {
  /** */
  type elt = float
  type typed_array<'a> = Js_typed_array2.Float64Array.typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  // @bs.send.pipe(: t) external setArray: array<elt> => unit = "set"
  // @bs.send.pipe(: t) external setArrayOffset: (array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  // @bs.send.pipe(: t) external copyWithin: (~to_: int) => t = "copyWithin"
  // @bs.send.pipe(: t) external copyWithinFrom: (~to_: int, ~from: int) => t = "copyWithin"
  // @bs.send.pipe(: t)
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  external copyWithinFromRange: (~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  // @bs.send.pipe(: t) external fillInPlace: elt => t = "fill"
  // @bs.send.pipe(: t) external fillFromInPlace: (elt, ~from: int) => t = "fill"
  // @bs.send.pipe(: t) external fillRangeInPlace: (elt, ~start: int, ~end_: int) => t = "fill"

  // @bs.send.pipe(: t) external reverseInPlace: t = "reverse"

  // @bs.send.pipe(: t) external sortInPlace: t = "sort"
  // @bs.send.pipe(: t) external sortInPlaceWith: ((. elt, elt) => int) => t = "sort"

  /* Accessor functions */
  // @bs.send.pipe(: t) external includes: elt => bool = "includes" /* ES2016 */

  // @bs.send.pipe(: t) external indexOf: elt => int = "indexOf"
  // @bs.send.pipe(: t) external indexOfFrom: (elt, ~from: int) => int = "indexOf"

  // @bs.send.pipe(: t) external join: string = "join"
  // @bs.send.pipe(: t) external joinWith: string => string = "join"

  // @bs.send.pipe(: t) external lastIndexOf: elt => int = "lastIndexOf"
  // @bs.send.pipe(: t) external lastIndexOfFrom: (elt, ~from: int) => int = "lastIndexOf"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  external slice: (~start: int, ~end_: int) => t = "slice"

  // @bs.send.pipe(: t) external copy: t = "slice"
  // @bs.send.pipe(: t) external sliceFrom: int => t = "slice"

  // @bs.send.pipe(: t) /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  external subarray: (~start: int, ~end_: int) => t = "subarray"

  // @bs.send.pipe(: t) external subarrayFrom: int => t = "subarray"

  // @bs.send.pipe(: t) external toString: string = "toString"
  // @bs.send.pipe(: t) external toLocaleString: string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : (int * elt) array_iter = "" [@// @bs.send.pipe: t]
 */
  // @bs.send.pipe(: t) external every: ((. elt) => bool) => bool = "every"
  // @bs.send.pipe(: t) external everyi: ((. elt, int) => bool) => bool = "every"

  // @bs.send.pipe(: t) external filter: ((. elt) => bool) => t = "filter"
  // @bs.send.pipe(: t) external filteri: ((. elt, int) => bool) => t = "filter"

  // @bs.send.pipe(: t) external find: ((. elt) => bool) => Js.undefined<elt> = "find"
  // @bs.send.pipe(: t) external findi: ((. elt, int) => bool) => Js.undefined<elt> = "find"

  // @bs.send.pipe(: t) external findIndex: ((. elt) => bool) => int = "findIndex"
  // @bs.send.pipe(: t) external findIndexi: ((. elt, int) => bool) => int = "findIndex"

  // @bs.send.pipe(: t) external forEach: ((. elt) => unit) => unit = "forEach"
  // @bs.send.pipe(: t) external forEachi: ((. elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : int array_iter = "" [@// @bs.send.pipe: t]
 */

  // @bs.send.pipe(: t) external map: ((. elt) => 'b) => typed_array<'b> = "map"
  // @bs.send.pipe(: t) external mapi: ((. elt, int) => 'b) => typed_array<'b> = "map"

  // @bs.send.pipe(: t) external reduce: ((. 'b, elt) => 'b, 'b) => 'b = "reduce"
  // @bs.send.pipe(: t) external reducei: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduce"

  // @bs.send.pipe(: t) external reduceRight: ((. 'b, elt) => 'b, 'b) => 'b = "reduceRight"
  // @bs.send.pipe(: t) external reduceRighti: ((. 'b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  // @bs.send.pipe(: t) external some: ((. elt) => bool) => bool = "some"
  // @bs.send.pipe(: t) external somei: ((. elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Float64Array.Constants.bytesPerElement` instead.",
    migrate: Float64Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Float64Array.BYTES_PER_ELEMENT"

  @deprecated({
    reason: "Use `Float64Array.fromArray` instead.",
    migrate: Float64Array.fromArray(),
  })
  @new
  external make: array<elt> => t = "Float64Array"
  /** can throw */
  @new
  @deprecated({
    reason: "Use `Float64Array.fromBuffer` instead.",
    migrate: Float64Array.fromBuffer(),
  })
  external fromBuffer: array_buffer => t = "Float64Array"

  /**
  **throw** Js.Exn.Error throw Js exception

  **param** offset is in bytes
  */
  @new
  @deprecated({
    reason: "Use `Float64Array.fromBufferToEnd` instead.",
    migrate: Float64Array.fromBufferToEnd(~byteOffset=%insert.unlabelledArgument(1)),
  })
  external fromBufferOffset: (array_buffer, int) => t = "Float64Array"

  /**
  **throw** Js.Exn.Error throws Js exception

  **param** offset is in bytes, length in elements
  */
  @new
  @deprecated({
    reason: "Use `Float64Array.fromBufferWithRange` instead.",
    migrate: Float64Array.fromBufferWithRange(
      ~byteOffset=%insert.labelledArgument("offset"),
      ~length=%insert.labelledArgument("length"),
    ),
  })
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Float64Array"

  @deprecated({
    reason: "Use `Float64Array.fromLength` instead.",
    migrate: Float64Array.fromLength(),
  })
  @new
  external fromLength: int => t = "Float64Array"
  @deprecated({
    reason: "Use `Float64Array.fromArrayLikeOrIterable` instead.",
    migrate: Float64Array.fromArrayLikeOrIterable(),
  })
  @val
  external from: array_like<elt> => t = "Float64Array.from"
  /* *Array.of is redundant, use make */
  @new @deprecated("use `make` instead") external create: array<float> => t = "Float64Array"
  @new @deprecated("use `fromBuffer` instead")
  external of_buffer: array_buffer => t = "Float64Array"
}
module Float64_array = Float64Array

/**
The DataView view provides a low-level interface for reading and writing
multiple number types in an ArrayBuffer irrespective of the platform's endianness.

**see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView)
*/
module DataView = {
  type t = Js_typed_array2.DataView.t

  @new external make: array_buffer => t = "DataView"
  @new external fromBuffer: array_buffer => t = "DataView"
  @new external fromBufferOffset: (array_buffer, int) => t = "DataView"
  @new external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "DataView"

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  @send external getInt8: (t, int) => int = "getInt8"
  @send external getUint8: (t, int) => int = "getUint8"

  @send external getInt16: (t, int) => int = "getInt16"
  @send external getInt16LittleEndian: (t, int, @as(1) _) => int = "getInt16"

  @send external getUint16: (t, int) => int = "getUint16"
  @send external getUint16LittleEndian: (t, int, @as(1) _) => int = "getUint16"

  @send external getInt32: (t, int) => int = "getInt32"
  @send external getInt32LittleEndian: (t, int, @as(1) _) => int = "getInt32"

  @send external getUint32: (t, int) => int = "getUint32"
  @send external getUint32LittleEndian: (t, int, @as(1) _) => int = "getUint32"

  @send external getFloat32: (t, int) => float = "getFloat32"
  @send external getFloat32LittleEndian: (t, int, @as(1) _) => float = "getFloat32"

  @send external getFloat64: (t, int) => float = "getFloat64"
  @send external getFloat64LittleEndian: (t, int, @as(1) _) => float = "getFloat64"

  @send external setInt8: (t, int, int) => unit = "setInt8"
  @send external setUint8: (t, int, int) => unit = "setUint8"

  @send external setInt16: (t, int, int) => unit = "setInt16"
  @send external setInt16LittleEndian: (t, int, int, @as(1) _) => unit = "setInt16"

  @send external setUint16: (t, int, int) => unit = "setUint16"
  @send external setUint16LittleEndian: (t, int, int, @as(1) _) => unit = "setUint16"

  @send external setInt32: (t, int, int) => unit = "setInt32"
  @send external setInt32LittleEndian: (t, int, int, @as(1) _) => unit = "setInt32"

  @send external setUint32: (t, int, int) => unit = "setUint32"
  @send external setUint32LittleEndian: (t, int, int, @as(1) _) => unit = "setUint32"

  @send external setFloat32: (t, int, float) => unit = "setFloat32"
  @send external setFloat32LittleEndian: (t, int, float, @as(1) _) => unit = "setFloat32"

  @send external setFloat64: (t, int, float) => unit = "setFloat64"
  @send external setFloat64LittleEndian: (t, int, float, @as(1) _) => unit = "setFloat64"
}

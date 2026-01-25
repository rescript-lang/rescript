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

type array_buffer = Stdlib_ArrayBuffer.t
type array_like<'a> /* should be shared with js_array */

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

  @deprecated({
    reason: "Use `ArrayBuffer.slice` instead.",
    migrate: ArrayBuffer.slice(~end=%insert.labelledArgument("end_")),
  })
  @send
  external slice: (t, ~start: int, ~end_: int) => array_buffer = "slice"
  @deprecated({
    reason: "Use `ArrayBuffer.sliceToEnd` instead.",
    migrate: ArrayBuffer.sliceToEnd(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external sliceFrom: (t, int) => array_buffer = "slice"
}

/* commented out until bs has a plan for iterators
  external values : t -> elt array_iter = "" [@@send]
 */

module Int8Array = {
  /** */
  type elt = int
  type typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  @deprecated({
    reason: "Use `TypedArray.setArray` instead.",
    migrate: TypedArray.setArray(),
  })
  @send
  external setArray: (t, array<elt>) => unit = "set"
  @deprecated({
    reason: "Use `TypedArray.setArrayFrom` instead.",
    migrate: TypedArray.setArrayFrom(%insert.unlabelledArgument(2)),
  })
  @send
  external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  @deprecated({
    reason: "Use `TypedArray.copyAllWithin` instead.",
    migrate: TypedArray.copyAllWithin(~target=%insert.labelledArgument("to_")),
  })
  @send
  external copyWithin: (t, ~to_: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithinToEnd` instead.",
    migrate: TypedArray.copyWithinToEnd(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("from"),
    ),
  })
  @send
  external copyWithinFrom: (t, ~to_: int, ~from: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external copyWithinFromRange: (t, ~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  @deprecated({
    reason: "Use `TypedArray.fillAll` instead.",
    migrate: TypedArray.fillAll(),
  })
  @send
  external fillInPlace: (t, elt) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fillToEnd` instead.",
    migrate: TypedArray.fillToEnd(~start=%insert.labelledArgument("from")),
  })
  @send
  external fillFromInPlace: (t, elt, ~from: int) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fill` instead.",
    migrate: TypedArray.fill(
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external fillRangeInPlace: (t, elt, ~start: int, ~end_: int) => t = "fill"

  @deprecated({
    reason: "Use `TypedArray.reverse` instead.",
    migrate: TypedArray.reverse(),
  })
  @send
  external reverseInPlace: t => t = "reverse"

  @deprecated({
    reason: "Use `TypedArray.toSorted` instead.",
    migrate: TypedArray.toSorted((a, b) =>
      %todo_("This needs a comparator function. Use `Int.compare` for ints, etc.")
    ),
  })
  @send
  external sortInPlace: t => t = "sort"
  @deprecated({
    reason: "Use `TypedArray.sort` instead.",
    migrate: TypedArray.sort(),
  })
  @deprecated({
    reason: "Use `TypedArray.sort` instead.",
    migrate: TypedArray.sort(),
  })
  @send
  external sortInPlaceWith: (t, (elt, elt) => int) => t = "sort"

  /* Accessor functions */
  @deprecated({
    reason: "Use `TypedArray.includes` instead.",
    migrate: TypedArray.includes(),
  })
  @send
  external includes: (t, elt) => bool = "includes" /* ES2016 */

  @deprecated({
    reason: "Use `TypedArray.indexOf` instead.",
    migrate: TypedArray.indexOf(),
  })
  @send
  external indexOf: (t, elt) => int = "indexOf"
  @deprecated({
    reason: "Use `TypedArray.indexOfFrom` instead.",
    migrate: TypedArray.indexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @deprecated({
    reason: "Use `TypedArray.joinWith` instead.",
    migrate: TypedArray.joinWith(","),
  })
  @send
  external join: t => string = "join"
  @deprecated({
    reason: "Use `TypedArray.joinWith` instead.",
    migrate: TypedArray.joinWith(),
  })
  @send
  external joinWith: (t, string) => string = "join"

  @deprecated({
    reason: "Use `TypedArray.lastIndexOf` instead.",
    migrate: TypedArray.lastIndexOf(),
  })
  @send
  external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @deprecated({
    reason: "Use `TypedArray.lastIndexOfFrom` instead.",
    migrate: TypedArray.lastIndexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  @send
  external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @deprecated({
    reason: "Use `TypedArray.copy` instead.",
    migrate: TypedArray.copy(),
  })
  @send
  external copy: t => t = "slice"
  @deprecated({
    reason: "Use `TypedArray.sliceToEnd` instead.",
    migrate: TypedArray.sliceToEnd(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external sliceFrom: (t, int) => t = "slice"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  @send
  external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external subarrayFrom: (t, int) => t = "subarray"

  @deprecated({
    reason: "Use `TypedArray.toString` instead.",
    migrate: TypedArray.toString(),
  })
  @send
  external toString: t => string = "toString"
  @deprecated({
    reason: "Use `TypedArray.toLocaleString` instead.",
    migrate: TypedArray.toLocaleString(),
  })
  @send
  external toLocaleString: t => string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : t -> (int * elt) array_iter = "" [@@send]
 */
  @deprecated({
    reason: "Use `TypedArray.every` instead.",
    migrate: TypedArray.every(),
  })
  @send
  external every: (t, elt => bool) => bool = "every"
  @deprecated({
    reason: "Use `TypedArray.everyWithIndex` instead.",
    migrate: TypedArray.everyWithIndex(),
  })
  @send
  external everyi: (t, (elt, int) => bool) => bool = "every"

  @deprecated({
    reason: "Use `TypedArray.filter` instead.",
    migrate: TypedArray.filter(),
  })
  @send
  external filter: (t, elt => bool) => t = "filter"
  @deprecated({
    reason: "Use `TypedArray.filterWithIndex` instead.",
    migrate: TypedArray.filterWithIndex(),
  })
  @send
  external filteri: (t, (elt, int) => bool) => t = "filter"

  @deprecated({
    reason: "Use `TypedArray.find` instead.",
    migrate: TypedArray.find(),
  })
  @send
  external find: (t, elt => bool) => Js_undefined.t<elt> = "find"
  @deprecated({
    reason: "Use `TypedArray.findWithIndex` instead.",
    migrate: TypedArray.findWithIndex(),
  })
  @send
  external findi: (t, (elt, int) => bool) => Js_undefined.t<elt> = "find"

  @deprecated({
    reason: "Use `TypedArray.findIndex` instead.",
    migrate: TypedArray.findIndex(),
  })
  @send
  external findIndex: (t, elt => bool) => int = "findIndex"
  @deprecated({
    reason: "Use `TypedArray.findIndexWithIndex` instead.",
    migrate: TypedArray.findIndexWithIndex(),
  })
  @send
  external findIndexi: (t, (elt, int) => bool) => int = "findIndex"

  @deprecated({
    reason: "Use `TypedArray.forEach` instead.",
    migrate: TypedArray.forEach(),
  })
  @send
  external forEach: (t, elt => unit) => unit = "forEach"
  @deprecated({
    reason: "Use `TypedArray.forEachWithIndex` instead.",
    migrate: TypedArray.forEachWithIndex(),
  })
  @send
  external forEachi: (t, (elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : t -> int array_iter = "" [@@send]
 */

  @deprecated({
    reason: "Use `TypedArray.map` instead.",
    migrate: TypedArray.map(),
  })
  @send
  external map: (t, elt => 'b) => typed_array<'b> = "map"
  @deprecated({
    reason: "Use `TypedArray.mapWithIndex` instead.",
    migrate: TypedArray.mapWithIndex(),
  })
  @send
  external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @deprecated({
    reason: "Use `TypedArray.reduce` instead.",
    migrate: TypedArray.reduce(),
  })
  @send
  external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @deprecated({
    reason: "Use `TypedArray.reduceWithIndex` instead.",
    migrate: TypedArray.reduceWithIndex(),
  })
  @send
  external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @deprecated({
    reason: "Use `TypedArray.reduceRight` instead.",
    migrate: TypedArray.reduceRight(),
  })
  @send
  external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @deprecated({
    reason: "Use `TypedArray.reduceRightWithIndex` instead.",
    migrate: TypedArray.reduceRightWithIndex(),
  })
  @send
  external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @deprecated({
    reason: "Use `TypedArray.some` instead.",
    migrate: TypedArray.some(),
  })
  @send
  external some: (t, elt => bool) => bool = "some"
  @deprecated({
    reason: "Use `TypedArray.someWithIndex` instead.",
    migrate: TypedArray.someWithIndex(),
  })
  @send
  external somei: (t, (elt, int) => bool) => bool = "some"

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
  **throw** Js.Exn.Error throw Js exception

  **param** offset is in bytes
  */
  @deprecated({
    reason: "Use `Int8Array.fromBufferToEnd` instead.",
    migrate: Int8Array.fromBufferToEnd(~byteOffset=%insert.unlabelledArgument(1)),
  })
  @new external fromBufferOffset: (array_buffer, int) => t = "Int8Array"

  /**
  **throw** Js.Exn.Error throws Js exception

  **param** offset is in bytes, length in elements
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
  type typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  @deprecated({
    reason: "Use `TypedArray.setArray` instead.",
    migrate: TypedArray.setArray(),
  })
  @send
  external setArray: (t, array<elt>) => unit = "set"
  @deprecated({
    reason: "Use `TypedArray.setArrayFrom` instead.",
    migrate: TypedArray.setArrayFrom(%insert.unlabelledArgument(2)),
  })
  @send
  external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  @deprecated({
    reason: "Use `TypedArray.copyAllWithin` instead.",
    migrate: TypedArray.copyAllWithin(~target=%insert.labelledArgument("to_")),
  })
  @send
  external copyWithin: (t, ~to_: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithinToEnd` instead.",
    migrate: TypedArray.copyWithinToEnd(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("from"),
    ),
  })
  @send
  external copyWithinFrom: (t, ~to_: int, ~from: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external copyWithinFromRange: (t, ~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  @deprecated({
    reason: "Use `TypedArray.fillAll` instead.",
    migrate: TypedArray.fillAll(),
  })
  @send
  external fillInPlace: (t, elt) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fillToEnd` instead.",
    migrate: TypedArray.fillToEnd(~start=%insert.labelledArgument("from")),
  })
  @send
  external fillFromInPlace: (t, elt, ~from: int) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fill` instead.",
    migrate: TypedArray.fill(
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external fillRangeInPlace: (t, elt, ~start: int, ~end_: int) => t = "fill"

  @deprecated({
    reason: "Use `TypedArray.reverse` instead.",
    migrate: TypedArray.reverse(),
  })
  @send
  external reverseInPlace: t => t = "reverse"

  @deprecated({
    reason: "Use `TypedArray.toSorted` instead.",
    migrate: TypedArray.toSorted((a, b) =>
      %todo_("This needs a comparator function. Use an appropriate comparator (e.g. Int.compare).")
    ),
  })
  @send
  external sortInPlace: t => t = "sort"
  @send external sortInPlaceWith: (t, (elt, elt) => int) => t = "sort"

  /* Accessor functions */
  @deprecated({
    reason: "Use `TypedArray.includes` instead.",
    migrate: TypedArray.includes(),
  })
  @send
  external includes: (t, elt) => bool = "includes" /* ES2016 */

  @deprecated({
    reason: "Use `TypedArray.indexOf` instead.",
    migrate: TypedArray.indexOf(),
  })
  @send
  external indexOf: (t, elt) => int = "indexOf"
  @deprecated({
    reason: "Use `TypedArray.indexOfFrom` instead.",
    migrate: TypedArray.indexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @deprecated({
    reason: "Use `TypedArray.joinWith` instead.",
    migrate: TypedArray.joinWith(","),
  })
  @send
  external join: t => string = "join"
  @deprecated({
    reason: "Use `TypedArray.joinWith` instead.",
    migrate: TypedArray.joinWith(),
  })
  @send
  external joinWith: (t, string) => string = "join"

  @deprecated({
    reason: "Use `TypedArray.lastIndexOf` instead.",
    migrate: TypedArray.lastIndexOf(),
  })
  @send
  external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @deprecated({
    reason: "Use `TypedArray.lastIndexOfFrom` instead.",
    migrate: TypedArray.lastIndexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  @send external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @deprecated({
    reason: "Use `TypedArray.copy` instead.",
    migrate: TypedArray.copy(),
  })
  @send
  external copy: t => t = "slice"
  @deprecated({
    reason: "Use `TypedArray.sliceToEnd` instead.",
    migrate: TypedArray.sliceToEnd(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external sliceFrom: (t, int) => t = "slice"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  @send external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external subarrayFrom: (t, int) => t = "subarray"

  @deprecated({
    reason: "Use `TypedArray.toString` instead.",
    migrate: TypedArray.toString(),
  })
  @send
  external toString: t => string = "toString"
  @deprecated({
    reason: "Use `TypedArray.toLocaleString` instead.",
    migrate: TypedArray.toLocaleString(),
  })
  @send
  external toLocaleString: t => string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : t -> (int * elt) array_iter = "" [@@send]
 */
  @deprecated({
    reason: "Use `TypedArray.every` instead.",
    migrate: TypedArray.every(),
  })
  @send
  external every: (t, elt => bool) => bool = "every"
  @deprecated({
    reason: "Use `TypedArray.everyWithIndex` instead.",
    migrate: TypedArray.everyWithIndex(),
  })
  @send
  external everyi: (t, (elt, int) => bool) => bool = "every"

  @deprecated({
    reason: "Use `TypedArray.filter` instead.",
    migrate: TypedArray.filter(),
  })
  @send
  external filter: (t, elt => bool) => t = "filter"
  @deprecated({
    reason: "Use `TypedArray.filterWithIndex` instead.",
    migrate: TypedArray.filterWithIndex(),
  })
  @send
  external filteri: (t, (elt, int) => bool) => t = "filter"

  @deprecated({
    reason: "Use `TypedArray.find` instead.",
    migrate: TypedArray.find(),
  })
  @send
  external find: (t, elt => bool) => Js_undefined.t<elt> = "find"
  @deprecated({
    reason: "Use `TypedArray.findWithIndex` instead.",
    migrate: TypedArray.findWithIndex(),
  })
  @send
  external findi: (t, (elt, int) => bool) => Js_undefined.t<elt> = "find"

  @deprecated({
    reason: "Use `TypedArray.findIndex` instead.",
    migrate: TypedArray.findIndex(),
  })
  @send
  external findIndex: (t, elt => bool) => int = "findIndex"
  @deprecated({
    reason: "Use `TypedArray.findIndexWithIndex` instead.",
    migrate: TypedArray.findIndexWithIndex(),
  })
  @send
  external findIndexi: (t, (elt, int) => bool) => int = "findIndex"

  @deprecated({
    reason: "Use `TypedArray.forEach` instead.",
    migrate: TypedArray.forEach(),
  })
  @send
  external forEach: (t, elt => unit) => unit = "forEach"
  @deprecated({
    reason: "Use `TypedArray.forEachWithIndex` instead.",
    migrate: TypedArray.forEachWithIndex(),
  })
  @send
  external forEachi: (t, (elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : t -> int array_iter = "" [@@send]
 */

  @deprecated({
    reason: "Use `TypedArray.map` instead.",
    migrate: TypedArray.map(),
  })
  @send
  external map: (t, elt => 'b) => typed_array<'b> = "map"
  @deprecated({
    reason: "Use `TypedArray.mapWithIndex` instead.",
    migrate: TypedArray.mapWithIndex(),
  })
  @send
  external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @deprecated({
    reason: "Use `TypedArray.reduce` instead.",
    migrate: TypedArray.reduce(),
  })
  @send
  external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @deprecated({
    reason: "Use `TypedArray.reduceWithIndex` instead.",
    migrate: TypedArray.reduceWithIndex(),
  })
  @send
  external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @deprecated({
    reason: "Use `TypedArray.reduceRight` instead.",
    migrate: TypedArray.reduceRight(),
  })
  @send
  external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @deprecated({
    reason: "Use `TypedArray.reduceRightWithIndex` instead.",
    migrate: TypedArray.reduceRightWithIndex(),
  })
  @send
  external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @deprecated({
    reason: "Use `TypedArray.some` instead.",
    migrate: TypedArray.some(),
  })
  @send
  external some: (t, elt => bool) => bool = "some"
  @deprecated({
    reason: "Use `TypedArray.someWithIndex` instead.",
    migrate: TypedArray.someWithIndex(),
  })
  @send
  external somei: (t, (elt, int) => bool) => bool = "some"

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
  type typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  @deprecated({
    reason: "Use `TypedArray.setArray` instead.",
    migrate: TypedArray.setArray(),
  })
  @send
  external setArray: (t, array<elt>) => unit = "set"
  @deprecated({
    reason: "Use `TypedArray.setArrayFrom` instead.",
    migrate: TypedArray.setArrayFrom(%insert.unlabelledArgument(2)),
  })
  @send
  external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  @deprecated({
    reason: "Use `TypedArray.copyAllWithin` instead.",
    migrate: TypedArray.copyAllWithin(~target=%insert.labelledArgument("to_")),
  })
  @send
  external copyWithin: (t, ~to_: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithinToEnd` instead.",
    migrate: TypedArray.copyWithinToEnd(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("from"),
    ),
  })
  @send
  external copyWithinFrom: (t, ~to_: int, ~from: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external copyWithinFromRange: (t, ~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  @deprecated({
    reason: "Use `TypedArray.fillAll` instead.",
    migrate: TypedArray.fillAll(),
  })
  @send
  external fillInPlace: (t, elt) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fillToEnd` instead.",
    migrate: TypedArray.fillToEnd(~start=%insert.labelledArgument("from")),
  })
  @send
  external fillFromInPlace: (t, elt, ~from: int) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fill` instead.",
    migrate: TypedArray.fill(
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external fillRangeInPlace: (t, elt, ~start: int, ~end_: int) => t = "fill"

  @deprecated({
    reason: "Use `TypedArray.reverse` instead.",
    migrate: TypedArray.reverse(),
  })
  @send
  external reverseInPlace: t => t = "reverse"

  @deprecated({
    reason: "Use `TypedArray.toSorted` instead.",
    migrate: TypedArray.toSorted((a, b) =>
      %todo_("This needs a comparator function. Use an appropriate comparator (e.g. Int.compare).")
    ),
  })
  @send
  external sortInPlace: t => t = "sort"
  @send external sortInPlaceWith: (t, (elt, elt) => int) => t = "sort"

  /* Accessor functions */
  @deprecated({
    reason: "Use `TypedArray.includes` instead.",
    migrate: TypedArray.includes(),
  })
  @send
  external includes: (t, elt) => bool = "includes" /* ES2016 */

  @deprecated({
    reason: "Use `TypedArray.indexOf` instead.",
    migrate: TypedArray.indexOf(),
  })
  @send
  external indexOf: (t, elt) => int = "indexOf"
  @deprecated({
    reason: "Use `TypedArray.indexOfFrom` instead.",
    migrate: TypedArray.indexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @deprecated({
    reason: "Use `TypedArray.joinWith` instead.",
    migrate: TypedArray.joinWith(","),
  })
  @send
  external join: t => string = "join"
  @deprecated({
    reason: "Use `TypedArray.joinWith` instead.",
    migrate: TypedArray.joinWith(),
  })
  @send
  external joinWith: (t, string) => string = "join"

  @deprecated({
    reason: "Use `TypedArray.lastIndexOf` instead.",
    migrate: TypedArray.lastIndexOf(),
  })
  @send
  external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @deprecated({
    reason: "Use `TypedArray.lastIndexOfFrom` instead.",
    migrate: TypedArray.lastIndexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  @send external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @deprecated({
    reason: "Use `TypedArray.copy` instead.",
    migrate: TypedArray.copy(),
  })
  @send
  external copy: t => t = "slice"
  @deprecated({
    reason: "Use `TypedArray.sliceToEnd` instead.",
    migrate: TypedArray.sliceToEnd(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external sliceFrom: (t, int) => t = "slice"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  @send external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external subarrayFrom: (t, int) => t = "subarray"

  @deprecated({
    reason: "Use `TypedArray.toString` instead.",
    migrate: TypedArray.toString(),
  })
  @send
  external toString: t => string = "toString"
  @deprecated({
    reason: "Use `TypedArray.toLocaleString` instead.",
    migrate: TypedArray.toLocaleString(),
  })
  @send
  external toLocaleString: t => string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : t -> (int * elt) array_iter = "" [@@send]
 */
  @deprecated({
    reason: "Use `TypedArray.every` instead.",
    migrate: TypedArray.every(),
  })
  @send
  external every: (t, elt => bool) => bool = "every"
  @deprecated({
    reason: "Use `TypedArray.everyWithIndex` instead.",
    migrate: TypedArray.everyWithIndex(),
  })
  @send
  external everyi: (t, (elt, int) => bool) => bool = "every"

  @deprecated({
    reason: "Use `TypedArray.filter` instead.",
    migrate: TypedArray.filter(),
  })
  @send
  external filter: (t, elt => bool) => t = "filter"
  @deprecated({
    reason: "Use `TypedArray.filterWithIndex` instead.",
    migrate: TypedArray.filterWithIndex(),
  })
  @send
  external filteri: (t, (elt, int) => bool) => t = "filter"

  @deprecated({
    reason: "Use `TypedArray.find` instead.",
    migrate: TypedArray.find(),
  })
  @send
  external find: (t, elt => bool) => Js_undefined.t<elt> = "find"
  @deprecated({
    reason: "Use `TypedArray.findWithIndex` instead.",
    migrate: TypedArray.findWithIndex(),
  })
  @send
  external findi: (t, (elt, int) => bool) => Js_undefined.t<elt> = "find"

  @deprecated({
    reason: "Use `TypedArray.findIndex` instead.",
    migrate: TypedArray.findIndex(),
  })
  @send
  external findIndex: (t, elt => bool) => int = "findIndex"
  @deprecated({
    reason: "Use `TypedArray.findIndexWithIndex` instead.",
    migrate: TypedArray.findIndexWithIndex(),
  })
  @send
  external findIndexi: (t, (elt, int) => bool) => int = "findIndex"

  @deprecated({
    reason: "Use `TypedArray.forEach` instead.",
    migrate: TypedArray.forEach(),
  })
  @send
  external forEach: (t, elt => unit) => unit = "forEach"
  @deprecated({
    reason: "Use `TypedArray.forEachWithIndex` instead.",
    migrate: TypedArray.forEachWithIndex(),
  })
  @send
  external forEachi: (t, (elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : t -> int array_iter = "" [@@send]
 */

  @deprecated({
    reason: "Use `TypedArray.map` instead.",
    migrate: TypedArray.map(),
  })
  @send
  external map: (t, elt => 'b) => typed_array<'b> = "map"
  @deprecated({
    reason: "Use `TypedArray.mapWithIndex` instead.",
    migrate: TypedArray.mapWithIndex(),
  })
  @send
  external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @deprecated({
    reason: "Use `TypedArray.reduce` instead.",
    migrate: TypedArray.reduce(),
  })
  @send
  external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @deprecated({
    reason: "Use `TypedArray.reduceWithIndex` instead.",
    migrate: TypedArray.reduceWithIndex(),
  })
  @send
  external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @deprecated({
    reason: "Use `TypedArray.reduceRight` instead.",
    migrate: TypedArray.reduceRight(),
  })
  @send
  external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @deprecated({
    reason: "Use `TypedArray.reduceRightWithIndex` instead.",
    migrate: TypedArray.reduceRightWithIndex(),
  })
  @send
  external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @deprecated({
    reason: "Use `TypedArray.some` instead.",
    migrate: TypedArray.some(),
  })
  @send
  external some: (t, elt => bool) => bool = "some"
  @deprecated({
    reason: "Use `TypedArray.someWithIndex` instead.",
    migrate: TypedArray.someWithIndex(),
  })
  @send
  external somei: (t, (elt, int) => bool) => bool = "some"

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
  type typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  @deprecated({reason: "Use `TypedArray.setArray` instead.", migrate: TypedArray.setArray()}) @send
  external setArray: (t, array<elt>) => unit = "set"
  @deprecated({
    reason: "Use `TypedArray.setArrayFrom` instead.",
    migrate: TypedArray.setArrayFrom(%insert.unlabelledArgument(2)),
  })
  @send
  external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({reason: "Use `TypedArray.length` instead.", migrate: TypedArray.length()}) @get
  external length: t => int = "length"

  /* Mutator functions */
  @deprecated({
    reason: "Use `TypedArray.copyAllWithin` instead.",
    migrate: TypedArray.copyAllWithin(~target=%insert.labelledArgument("to_")),
  })
  @send
  external copyWithin: (t, ~to_: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithinToEnd` instead.",
    migrate: TypedArray.copyWithinToEnd(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("from"),
    ),
  })
  @send
  external copyWithinFrom: (t, ~to_: int, ~from: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithin",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external /* end mapped below */

  copyWithinFromRange: (t, ~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  @deprecated({reason: "Use `TypedArray.fillAll` instead.", migrate: TypedArray.fillAll()}) @send
  external fillInPlace: (t, elt) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fillToEnd` instead.",
    migrate: TypedArray.fillToEnd(~start=%insert.labelledArgument("from")),
  })
  @send
  external fillFromInPlace: (t, elt, ~from: int) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fill` instead.",
    migrate: TypedArray.fill(
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external fillRangeInPlace: (t, elt, ~start: int, ~end_: int) => t = "fill"

  @deprecated({reason: "Use `TypedArray.reverse` instead.", migrate: TypedArray.reverse()}) @send
  external reverseInPlace: t => t = "reverse"

  @deprecated({
    reason: "Use `TypedArray.toSorted` instead.",
    migrate: TypedArray.toSorted((a, b) =>
      %todo_("This needs a comparator function. Use an appropriate comparator (e.g. Int.compare).")
    ),
  })
  @send
  external sortInPlace: t => t = "sort"
  @deprecated({reason: "Use `TypedArray.sort` instead.", migrate: TypedArray.sort()}) @send
  external sortInPlaceWith: (t, (elt, elt) => int) => t = "sort"

  /* Accessor functions */
  @deprecated({reason: "Use `TypedArray.includes` instead.", migrate: TypedArray.includes()}) @send
  external includes: (t, elt) => bool = "includes" /* ES2016 */

  @deprecated({reason: "Use `TypedArray.indexOf` instead.", migrate: TypedArray.indexOf()}) @send
  external indexOf: (t, elt) => int = "indexOf"
  @deprecated({
    reason: "Use `TypedArray.indexOfFrom` instead.",
    migrate: TypedArray.indexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @deprecated({reason: "Use `TypedArray.joinWith` instead.", migrate: TypedArray.joinWith(",")})
  @send
  external join: t => string = "join"
  @deprecated({reason: "Use `TypedArray.joinWith` instead.", migrate: TypedArray.joinWith()}) @send
  external joinWith: (t, string) => string = "join"

  @deprecated({reason: "Use `TypedArray.lastIndexOf` instead.", migrate: TypedArray.lastIndexOf()})
  @send
  external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @deprecated({
    reason: "Use `TypedArray.lastIndexOfFrom` instead.",
    migrate: TypedArray.lastIndexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  @send external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @deprecated({reason: "Use `TypedArray.copy` instead.", migrate: TypedArray.copy()}) @send
  external copy: t => t = "slice"
  @deprecated({
    reason: "Use `TypedArray.sliceToEnd` instead.",
    migrate: TypedArray.sliceToEnd(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external sliceFrom: (t, int) => t = "slice"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  @send external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external subarrayFrom: (t, int) => t = "subarray"

  @deprecated({reason: "Use `TypedArray.toString` instead.", migrate: TypedArray.toString()}) @send
  external toString: t => string = "toString"
  @deprecated({
    reason: "Use `TypedArray.toLocaleString` instead.",
    migrate: TypedArray.toLocaleString(),
  })
  @send
  external toLocaleString: t => string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : t -> (int * elt) array_iter = "" [@@send]
 */
  @deprecated({reason: "Use `TypedArray.every` instead.", migrate: TypedArray.every()}) @send
  external every: (t, elt => bool) => bool = "every"
  @deprecated({
    reason: "Use `TypedArray.everyWithIndex` instead.",
    migrate: TypedArray.everyWithIndex(),
  })
  @send
  external everyi: (t, (elt, int) => bool) => bool = "every"

  @deprecated({reason: "Use `TypedArray.filter` instead.", migrate: TypedArray.filter()}) @send
  external filter: (t, elt => bool) => t = "filter"
  @deprecated({
    reason: "Use `TypedArray.filterWithIndex` instead.",
    migrate: TypedArray.filterWithIndex(),
  })
  @send
  external filteri: (t, (elt, int) => bool) => t = "filter"

  @deprecated({reason: "Use `TypedArray.find` instead.", migrate: TypedArray.find()}) @send
  external find: (t, elt => bool) => Js_undefined.t<elt> = "find"
  @deprecated({
    reason: "Use `TypedArray.findWithIndex` instead.",
    migrate: TypedArray.findWithIndex(),
  })
  @send
  external findi: (t, (elt, int) => bool) => Js_undefined.t<elt> = "find"

  @deprecated({reason: "Use `TypedArray.findIndex` instead.", migrate: TypedArray.findIndex()})
  @send
  external findIndex: (t, elt => bool) => int = "findIndex"
  @deprecated({
    reason: "Use `TypedArray.findIndexWithIndex` instead.",
    migrate: TypedArray.findIndexWithIndex(),
  })
  @send
  external findIndexi: (t, (elt, int) => bool) => int = "findIndex"

  @deprecated({reason: "Use `TypedArray.forEach` instead.", migrate: TypedArray.forEach()}) @send
  external forEach: (t, elt => unit) => unit = "forEach"
  @deprecated({
    reason: "Use `TypedArray.forEachWithIndex` instead.",
    migrate: TypedArray.forEachWithIndex(),
  })
  @send
  external forEachi: (t, (elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : t -> int array_iter = "" [@@send]
 */

  @deprecated({reason: "Use `TypedArray.map` instead.", migrate: TypedArray.map()}) @send
  external map: (t, elt => 'b) => typed_array<'b> = "map"
  @deprecated({
    reason: "Use `TypedArray.mapWithIndex` instead.",
    migrate: TypedArray.mapWithIndex(),
  })
  @send
  external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @deprecated({reason: "Use `TypedArray.reduce` instead.", migrate: TypedArray.reduce()}) @send
  external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @deprecated({
    reason: "Use `TypedArray.reduceWithIndex` instead.",
    migrate: TypedArray.reduceWithIndex(),
  })
  @send
  external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @deprecated({reason: "Use `TypedArray.reduceRight` instead.", migrate: TypedArray.reduceRight()})
  @send
  external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @deprecated({
    reason: "Use `TypedArray.reduceRightWithIndex` instead.",
    migrate: TypedArray.reduceRightWithIndex(),
  })
  @send
  external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @deprecated({reason: "Use `TypedArray.some` instead.", migrate: TypedArray.some()}) @send
  external some: (t, elt => bool) => bool = "some"
  @deprecated({
    reason: "Use `TypedArray.someWithIndex` instead.",
    migrate: TypedArray.someWithIndex(),
  })
  @send
  external somei: (t, (elt, int) => bool) => bool = "some"

  @val external _BYTES_PER_ELEMENT: int = "Int16Array.BYTES_PER_ELEMENT"

  @new external make: array<elt> => t = "Int16Array"
  /** can throw */
  @new
  external fromBuffer: array_buffer => t = "Int16Array"

  /**
  **throw** Js.Exn.Error throw Js exception

  **param** offset is in bytes
  */
  @new
  external fromBufferOffset: (array_buffer, int) => t = "Int16Array"

  /**
  **throw** Js.Exn.Error throws Js exception

  **param** offset is in bytes, length in elements
  */
  @new
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Int16Array"

  @new external fromLength: int => t = "Int16Array"
  @val external from: array_like<elt> => t = "Int16Array.from"
  /* *Array.of is redundant, use make */
}

module Uint16Array = {
  /** */
  type elt = int
  type typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  @send external setArray: (t, array<elt>) => unit = "set"
  @send external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({
    reason: "Use `TypedArray.length` instead.",
    migrate: TypedArray.length(),
  })
  @get
  external length: t => int = "length"

  /* Mutator functions */
  @send external copyWithin: (t, ~to_: int) => t = "copyWithin"
  @send external copyWithinFrom: (t, ~to_: int, ~from: int) => t = "copyWithin"
  @send external copyWithinFromRange: (t, ~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  @send external fillInPlace: (t, elt) => t = "fill"
  @send external fillFromInPlace: (t, elt, ~from: int) => t = "fill"
  @send external fillRangeInPlace: (t, elt, ~start: int, ~end_: int) => t = "fill"

  @send external reverseInPlace: t => t = "reverse"

  @send external sortInPlace: t => t = "sort"
  @send external sortInPlaceWith: (t, (elt, elt) => int) => t = "sort"

  /* Accessor functions */
  @deprecated({
    reason: "Use `TypedArray.includes` instead.",
    migrate: TypedArray.includes(),
  })
  @send
  external includes: (t, elt) => bool = "includes" /* ES2016 */

  @send external indexOf: (t, elt) => int = "indexOf"
  @deprecated({
    reason: "Use `TypedArray.indexOfFrom` instead.",
    migrate: TypedArray.indexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @send external join: t => string = "join"
  @send external joinWith: (t, string) => string = "join"

  @send external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @send external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  @send
  external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @send external copy: t => t = "slice"
  @deprecated({
    reason: "Use `TypedArray.sliceToEnd` instead.",
    migrate: TypedArray.sliceToEnd(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external sliceFrom: (t, int) => t = "slice"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  @send
  external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external subarrayFrom: (t, int) => t = "subarray"

  @send external toString: t => string = "toString"
  @send external toLocaleString: t => string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : t -> (int * elt) array_iter = "" [@@send]
 */
  @send external every: (t, elt => bool) => bool = "every"
  @send external everyi: (t, (elt, int) => bool) => bool = "every"

  @send external filter: (t, elt => bool) => t = "filter"
  @send external filteri: (t, (elt, int) => bool) => t = "filter"

  @send external find: (t, elt => bool) => Js_undefined.t<elt> = "find"
  @send external findi: (t, (elt, int) => bool) => Js_undefined.t<elt> = "find"

  @send external findIndex: (t, elt => bool) => int = "findIndex"
  @send external findIndexi: (t, (elt, int) => bool) => int = "findIndex"

  @send external forEach: (t, elt => unit) => unit = "forEach"
  @send external forEachi: (t, (elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : t -> int array_iter = "" [@@send]
 */

  @deprecated({
    reason: "Use `TypedArray.map` instead.",
    migrate: TypedArray.map(),
  })
  @send
  external map: (t, elt => 'b) => typed_array<'b> = "map"
  @send external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @deprecated({
    reason: "Use `TypedArray.reduce` instead.",
    migrate: TypedArray.reduce(),
  })
  @send
  external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @send external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @send external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @send external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @send external some: (t, elt => bool) => bool = "some"
  @send external somei: (t, (elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Uint16Array.Constants.bytesPerElement` instead.",
    migrate: Uint16Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Uint16Array.BYTES_PER_ELEMENT"

  @deprecated({reason: "Use `Uint16Array.fromArray` instead.", migrate: Uint16Array.fromArray()})
  @new
  external make: array<elt> => t = "Uint16Array"
  /** can throw */
  @new
  @deprecated({reason: "Use `Uint16Array.fromBuffer` instead.", migrate: Uint16Array.fromBuffer()})
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
    reason: "Use `Uint16Array.fromBufferWithRange",
    migrate: Uint16Array.fromBufferWithRange(
      ~byteOffset=%insert.labelledArgument("offset"),
      ~length=%insert.labelledArgument("length"),
    ),
  })
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Uint16Array"

  @deprecated({reason: "Use `Uint16Array.fromLength` instead.", migrate: Uint16Array.fromLength()})
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
  type typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  @deprecated({reason: "Use `TypedArray.setArray` instead.", migrate: TypedArray.setArray()}) @send
  external setArray: (t, array<elt>) => unit = "set"
  @deprecated({
    reason: "Use `TypedArray.setArrayFrom` instead.",
    migrate: TypedArray.setArrayFrom(%insert.unlabelledArgument(2)),
  })
  @send
  external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({reason: "Use `TypedArray.length` instead.", migrate: TypedArray.length()}) @get
  external length: t => int = "length"

  /* Mutator functions */
  @deprecated({
    reason: "Use `TypedArray.copyAllWithin` instead.",
    migrate: TypedArray.copyAllWithin(~target=%insert.labelledArgument("to_")),
  })
  @send
  external copyWithin: (t, ~to_: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithinToEnd` instead.",
    migrate: TypedArray.copyWithinToEnd(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("from"),
    ),
  })
  @send
  external copyWithinFrom: (t, ~to_: int, ~from: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external copyWithinFromRange: (t, ~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  @deprecated({reason: "Use `TypedArray.fillAll` instead.", migrate: TypedArray.fillAll()}) @send
  external fillInPlace: (t, elt) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fillToEnd` instead.",
    migrate: TypedArray.fillToEnd(~start=%insert.labelledArgument("from")),
  })
  @send
  external fillFromInPlace: (t, elt, ~from: int) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fill` instead.",
    migrate: TypedArray.fill(
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external fillRangeInPlace: (t, elt, ~start: int, ~end_: int) => t = "fill"

  @deprecated({reason: "Use `TypedArray.reverse` instead.", migrate: TypedArray.reverse()}) @send
  external reverseInPlace: t => t = "reverse"

  @deprecated({
    reason: "Use `TypedArray.toSorted` instead.",
    migrate: TypedArray.toSorted((a, b) =>
      %todo_("This needs a comparator function. Use an appropriate comparator (e.g. Int.compare).")
    ),
  })
  @send
  external sortInPlace: t => t = "sort"
  @deprecated({reason: "Use `TypedArray.sort` instead.", migrate: TypedArray.sort()}) @send
  external sortInPlaceWith: (t, (elt, elt) => int) => t = "sort"

  /* Accessor functions */
  @deprecated({reason: "Use `TypedArray.includes` instead.", migrate: TypedArray.includes()}) @send
  external includes: (t, elt) => bool = "includes" /* ES2016 */

  @deprecated({reason: "Use `TypedArray.indexOf` instead.", migrate: TypedArray.indexOf()}) @send
  external indexOf: (t, elt) => int = "indexOf"
  @deprecated({
    reason: "Use `TypedArray.indexOfFrom` instead.",
    migrate: TypedArray.indexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @deprecated({reason: "Use `TypedArray.joinWith` instead.", migrate: TypedArray.joinWith(",")})
  @send
  external join: t => string = "join"
  @deprecated({reason: "Use `TypedArray.joinWith` instead.", migrate: TypedArray.joinWith()}) @send
  external joinWith: (t, string) => string = "join"

  @deprecated({reason: "Use `TypedArray.lastIndexOf` instead.", migrate: TypedArray.lastIndexOf()})
  @send
  external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @deprecated({
    reason: "Use `TypedArray.lastIndexOfFrom` instead.",
    migrate: TypedArray.lastIndexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  @send external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @deprecated({reason: "Use `TypedArray.copy` instead.", migrate: TypedArray.copy()}) @send
  external copy: t => t = "slice"
  @deprecated({
    reason: "Use `TypedArray.sliceToEnd` instead.",
    migrate: TypedArray.sliceToEnd(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external sliceFrom: (t, int) => t = "slice"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  @send external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external subarrayFrom: (t, int) => t = "subarray"

  @deprecated({reason: "Use `TypedArray.toString` instead.", migrate: TypedArray.toString()}) @send
  external toString: t => string = "toString"
  @deprecated({
    reason: "Use `TypedArray.toLocaleString` instead.",
    migrate: TypedArray.toLocaleString(),
  })
  @send
  external toLocaleString: t => string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : t -> (int * elt) array_iter = "" [@@send]
 */
  @deprecated({reason: "Use `TypedArray.every` instead.", migrate: TypedArray.every()}) @send
  external every: (t, elt => bool) => bool = "every"
  @deprecated({
    reason: "Use `TypedArray.everyWithIndex` instead.",
    migrate: TypedArray.everyWithIndex(),
  })
  @send
  external everyi: (t, (elt, int) => bool) => bool = "every"

  @deprecated({reason: "Use `TypedArray.filter` instead.", migrate: TypedArray.filter()}) @send
  external filter: (t, elt => bool) => t = "filter"
  @deprecated({
    reason: "Use `TypedArray.filterWithIndex` instead.",
    migrate: TypedArray.filterWithIndex(),
  })
  @send
  external filteri: (t, (elt, int) => bool) => t = "filter"

  @deprecated({reason: "Use `TypedArray.find` instead.", migrate: TypedArray.find()}) @send
  external find: (t, elt => bool) => Js_undefined.t<elt> = "find"
  @deprecated({
    reason: "Use `TypedArray.findWithIndex` instead.",
    migrate: TypedArray.findWithIndex(),
  })
  @send
  external findi: (t, (elt, int) => bool) => Js_undefined.t<elt> = "find"

  @deprecated({reason: "Use `TypedArray.findIndex` instead.", migrate: TypedArray.findIndex()})
  @send
  external findIndex: (t, elt => bool) => int = "findIndex"
  @deprecated({
    reason: "Use `TypedArray.findIndexWithIndex` instead.",
    migrate: TypedArray.findIndexWithIndex(),
  })
  @send
  external findIndexi: (t, (elt, int) => bool) => int = "findIndex"

  @deprecated({reason: "Use `TypedArray.forEach` instead.", migrate: TypedArray.forEach()}) @send
  external forEach: (t, elt => unit) => unit = "forEach"
  @deprecated({
    reason: "Use `TypedArray.forEachWithIndex` instead.",
    migrate: TypedArray.forEachWithIndex(),
  })
  @send
  external forEachi: (t, (elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : t -> int array_iter = "" [@@send]
 */

  @deprecated({reason: "Use `TypedArray.map` instead.", migrate: TypedArray.map()}) @send
  external map: (t, elt => 'b) => typed_array<'b> = "map"
  @deprecated({
    reason: "Use `TypedArray.mapWithIndex` instead.",
    migrate: TypedArray.mapWithIndex(),
  })
  @send
  external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @deprecated({reason: "Use `TypedArray.reduce` instead.", migrate: TypedArray.reduce()}) @send
  external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @deprecated({
    reason: "Use `TypedArray.reduceWithIndex` instead.",
    migrate: TypedArray.reduceWithIndex(),
  })
  @send
  external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @deprecated({reason: "Use `TypedArray.reduceRight` instead.", migrate: TypedArray.reduceRight()})
  @send
  external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @deprecated({
    reason: "Use `TypedArray.reduceRightWithIndex` instead.",
    migrate: TypedArray.reduceRightWithIndex(),
  })
  @send
  external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @deprecated({reason: "Use `TypedArray.some` instead.", migrate: TypedArray.some()}) @send
  external some: (t, elt => bool) => bool = "some"
  @deprecated({
    reason: "Use `TypedArray.someWithIndex` instead.",
    migrate: TypedArray.someWithIndex(),
  })
  @send
  external somei: (t, (elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Int32Array.Constants.bytesPerElement` instead.",
    migrate: Int32Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Int32Array.BYTES_PER_ELEMENT"

  @deprecated({reason: "Use `Int32Array.fromArray` instead.", migrate: Int32Array.fromArray()}) @new
  external make: array<elt> => t = "Int32Array"
  /** can throw */
  @new @deprecated({reason: "Use `Int32Array.fromBuffer", migrate: Int32Array.fromBuffer()})
  external fromBuffer: array_buffer => t = "Int32Array"

  /**
  **throw** Js.Exn.Error throw Js exception

  **param** offset is in bytes
  */
  @new
  @deprecated({
    reason: "Use `Int32Array.fromBufferToEnd` instead.",
    migrate: Int32Array.fromBufferToEnd(~byteOffset=%insert.unlabelledArgument(1)),
  })
  external fromBufferOffset: (array_buffer, int) => t = "Int32Array"

  /**
  **throw** Js.Exn.Error throws Js exception

  **param** offset is in bytes, length in elements
  */
  @new
  @deprecated({
    reason: "Use `Int32Array.fromBufferWithRange",
    migrate: Int32Array.fromBufferWithRange(
      ~byteOffset=%insert.labelledArgument("offset"),
      ~length=%insert.labelledArgument("length"),
    ),
  })
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Int32Array"

  @deprecated({reason: "Use `Int32Array.fromLength` instead.", migrate: Int32Array.fromLength()})
  @new
  external fromLength: int => t = "Int32Array"
  @deprecated({
    reason: "Use `Int32Array.fromArrayLikeOrIterable` instead.",
    migrate: Int32Array.fromArrayLikeOrIterable(),
  })
  @val
  external from: array_like<elt> => t = "Int32Array.from"
  /* *Array.of is redundant, use make */
}

module Uint32Array = {
  /** */
  type elt = int
  type typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  @deprecated({reason: "Use `TypedArray.setArray` instead.", migrate: TypedArray.setArray()}) @send
  external setArray: (t, array<elt>) => unit = "set"
  @deprecated({
    reason: "Use `TypedArray.setArrayFrom` instead.",
    migrate: TypedArray.setArrayFrom(%insert.unlabelledArgument(2)),
  })
  @send
  external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({reason: "Use `TypedArray.length` instead.", migrate: TypedArray.length()}) @get
  external length: t => int = "length"

  /* Mutator functions */
  @deprecated({
    reason: "Use `TypedArray.copyAllWithin` instead.",
    migrate: TypedArray.copyAllWithin(~target=%insert.labelledArgument("to_")),
  })
  @send
  external copyWithin: (t, ~to_: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithinToEnd` instead.",
    migrate: TypedArray.copyWithinToEnd(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("from"),
    ),
  })
  @send
  external copyWithinFrom: (t, ~to_: int, ~from: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external copyWithinFromRange: (t, ~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  @deprecated({reason: "Use `TypedArray.fillAll` instead.", migrate: TypedArray.fillAll()}) @send
  external fillInPlace: (t, elt) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fillToEnd` instead.",
    migrate: TypedArray.fillToEnd(~start=%insert.labelledArgument("from")),
  })
  @send
  external fillFromInPlace: (t, elt, ~from: int) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fill` instead.",
    migrate: TypedArray.fill(
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external fillRangeInPlace: (t, elt, ~start: int, ~end_: int) => t = "fill"

  @deprecated({reason: "Use `TypedArray.reverse` instead.", migrate: TypedArray.reverse()}) @send
  external reverseInPlace: t => t = "reverse"

  @deprecated({
    reason: "Use `TypedArray.toSorted` instead.",
    migrate: TypedArray.toSorted((a, b) =>
      %todo_("This needs a comparator function. Use an appropriate comparator (e.g. Int.compare).")
    ),
  })
  @send
  external sortInPlace: t => t = "sort"
  @deprecated({reason: "Use `TypedArray.sort` instead.", migrate: TypedArray.sort()}) @send
  external sortInPlaceWith: (t, (elt, elt) => int) => t = "sort"

  /* Accessor functions */
  @deprecated({reason: "Use `TypedArray.includes` instead.", migrate: TypedArray.includes()}) @send
  external includes: (t, elt) => bool = "includes" /* ES2016 */

  @deprecated({reason: "Use `TypedArray.indexOf` instead.", migrate: TypedArray.indexOf()}) @send
  external indexOf: (t, elt) => int = "indexOf"
  @deprecated({
    reason: "Use `TypedArray.indexOfFrom` instead.",
    migrate: TypedArray.indexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @deprecated({reason: "Use `TypedArray.joinWith` instead.", migrate: TypedArray.joinWith(",")})
  @send
  external join: t => string = "join"
  @deprecated({reason: "Use `TypedArray.joinWith` instead.", migrate: TypedArray.joinWith()}) @send
  external joinWith: (t, string) => string = "join"

  @deprecated({reason: "Use `TypedArray.lastIndexOf` instead.", migrate: TypedArray.lastIndexOf()})
  @send
  external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @deprecated({
    reason: "Use `TypedArray.lastIndexOfFrom` instead.",
    migrate: TypedArray.lastIndexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  @send external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @deprecated({reason: "Use `TypedArray.copy` instead.", migrate: TypedArray.copy()}) @send
  external copy: t => t = "slice"
  @deprecated({
    reason: "Use `TypedArray.sliceToEnd` instead.",
    migrate: TypedArray.sliceToEnd(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external sliceFrom: (t, int) => t = "slice"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  @send external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external subarrayFrom: (t, int) => t = "subarray"

  @deprecated({reason: "Use `TypedArray.toString` instead.", migrate: TypedArray.toString()}) @send
  external toString: t => string = "toString"
  @deprecated({
    reason: "Use `TypedArray.toLocaleString` instead.",
    migrate: TypedArray.toLocaleString(),
  })
  @send
  external toLocaleString: t => string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : t -> (int * elt) array_iter = "" [@@send]
 */
  @deprecated({reason: "Use `TypedArray.every` instead.", migrate: TypedArray.every()}) @send
  external every: (t, elt => bool) => bool = "every"
  @deprecated({
    reason: "Use `TypedArray.everyWithIndex` instead.",
    migrate: TypedArray.everyWithIndex(),
  })
  @send
  external everyi: (t, (elt, int) => bool) => bool = "every"

  @deprecated({reason: "Use `TypedArray.filter` instead.", migrate: TypedArray.filter()}) @send
  external filter: (t, elt => bool) => t = "filter"
  @deprecated({
    reason: "Use `TypedArray.filterWithIndex` instead.",
    migrate: TypedArray.filterWithIndex(),
  })
  @send
  external filteri: (t, (elt, int) => bool) => t = "filter"

  @deprecated({reason: "Use `TypedArray.find` instead.", migrate: TypedArray.find()}) @send
  external find: (t, elt => bool) => Js_undefined.t<elt> = "find"
  @deprecated({
    reason: "Use `TypedArray.findWithIndex` instead.",
    migrate: TypedArray.findWithIndex(),
  })
  @send
  external findi: (t, (elt, int) => bool) => Js_undefined.t<elt> = "find"

  @deprecated({reason: "Use `TypedArray.findIndex` instead.", migrate: TypedArray.findIndex()})
  @send
  external findIndex: (t, elt => bool) => int = "findIndex"
  @deprecated({
    reason: "Use `TypedArray.findIndexWithIndex` instead.",
    migrate: TypedArray.findIndexWithIndex(),
  })
  @send
  external findIndexi: (t, (elt, int) => bool) => int = "findIndex"

  @deprecated({reason: "Use `TypedArray.forEach` instead.", migrate: TypedArray.forEach()}) @send
  external forEach: (t, elt => unit) => unit = "forEach"
  @deprecated({
    reason: "Use `TypedArray.forEachWithIndex` instead.",
    migrate: TypedArray.forEachWithIndex(),
  })
  @send
  external forEachi: (t, (elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : t -> int array_iter = "" [@@send]
 */

  @deprecated({reason: "Use `TypedArray.map` instead.", migrate: TypedArray.map()}) @send
  external map: (t, elt => 'b) => typed_array<'b> = "map"
  @deprecated({
    reason: "Use `TypedArray.mapWithIndex` instead.",
    migrate: TypedArray.mapWithIndex(),
  })
  @send
  external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @deprecated({reason: "Use `TypedArray.reduce` instead.", migrate: TypedArray.reduce()}) @send
  external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @deprecated({
    reason: "Use `TypedArray.reduceWithIndex` instead.",
    migrate: TypedArray.reduceWithIndex(),
  })
  @send
  external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @deprecated({reason: "Use `TypedArray.reduceRight` instead.", migrate: TypedArray.reduceRight()})
  @send
  external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @deprecated({
    reason: "Use `TypedArray.reduceRightWithIndex` instead.",
    migrate: TypedArray.reduceRightWithIndex(),
  })
  @send
  external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @deprecated({reason: "Use `TypedArray.some` instead.", migrate: TypedArray.some()}) @send
  external some: (t, elt => bool) => bool = "some"
  @deprecated({
    reason: "Use `TypedArray.someWithIndex` instead.",
    migrate: TypedArray.someWithIndex(),
  })
  @send
  external somei: (t, (elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Uint32Array.Constants.bytesPerElement` instead.",
    migrate: Uint32Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Uint32Array.BYTES_PER_ELEMENT"

  @deprecated({reason: "Use `Uint32Array.fromArray` instead.", migrate: Uint32Array.fromArray()})
  @new
  external make: array<elt> => t = "Uint32Array"
  /** can throw */
  @new
  @deprecated({reason: "Use `Uint32Array.fromBuffer` instead.", migrate: Uint32Array.fromBuffer()})
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

  @deprecated({reason: "Use `Uint32Array.fromLength` instead.", migrate: Uint32Array.fromLength()})
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
  type typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  @deprecated({reason: "Use `TypedArray.setArray` instead.", migrate: TypedArray.setArray()}) @send
  external setArray: (t, array<elt>) => unit = "set"
  @deprecated({
    reason: "Use `TypedArray.setArrayFrom` instead.",
    migrate: TypedArray.setArrayFrom(%insert.unlabelledArgument(2)),
  })
  @send
  external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({reason: "Use `TypedArray.length` instead.", migrate: TypedArray.length()}) @get
  external length: t => int = "length"

  /* Mutator functions */
  @deprecated({
    reason: "Use `TypedArray.copyAllWithin` instead.",
    migrate: TypedArray.copyAllWithin(~target=%insert.labelledArgument("to_")),
  })
  @send
  external copyWithin: (t, ~to_: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithinToEnd` instead.",
    migrate: TypedArray.copyWithinToEnd(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("from"),
    ),
  })
  @send
  external copyWithinFrom: (t, ~to_: int, ~from: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external copyWithinFromRange: (t, ~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  @deprecated({reason: "Use `TypedArray.fillAll` instead.", migrate: TypedArray.fillAll()}) @send
  external fillInPlace: (t, elt) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fillToEnd` instead.",
    migrate: TypedArray.fillToEnd(~start=%insert.labelledArgument("from")),
  })
  @send
  external fillFromInPlace: (t, elt, ~from: int) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fill` instead.",
    migrate: TypedArray.fill(
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external fillRangeInPlace: (t, elt, ~start: int, ~end_: int) => t = "fill"

  @deprecated({reason: "Use `TypedArray.reverse` instead.", migrate: TypedArray.reverse()}) @send
  external reverseInPlace: t => t = "reverse"

  @deprecated({
    reason: "Use `TypedArray.toSorted` instead.",
    migrate: TypedArray.toSorted((a, b) =>
      %todo_(
        "This needs a comparator function. Use an appropriate comparator (e.g. Float.compare)."
      )
    ),
  })
  @send
  external sortInPlace: t => t = "sort"
  @deprecated({reason: "Use `TypedArray.sort` instead.", migrate: TypedArray.sort()}) @send
  external sortInPlaceWith: (t, (elt, elt) => int) => t = "sort"

  /* Accessor functions */
  @deprecated({reason: "Use `TypedArray.includes` instead.", migrate: TypedArray.includes()}) @send
  external includes: (t, elt) => bool = "includes" /* ES2016 */

  @deprecated({reason: "Use `TypedArray.indexOf` instead.", migrate: TypedArray.indexOf()}) @send
  external indexOf: (t, elt) => int = "indexOf"
  @deprecated({
    reason: "Use `TypedArray.indexOfFrom` instead.",
    migrate: TypedArray.indexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @deprecated({reason: "Use `TypedArray.joinWith` instead.", migrate: TypedArray.joinWith(",")})
  @send
  external join: t => string = "join"
  @deprecated({reason: "Use `TypedArray.joinWith` instead.", migrate: TypedArray.joinWith()}) @send
  external joinWith: (t, string) => string = "join"

  @deprecated({reason: "Use `TypedArray.lastIndexOf` instead.", migrate: TypedArray.lastIndexOf()})
  @send
  external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @deprecated({
    reason: "Use `TypedArray.lastIndexOfFrom` instead.",
    migrate: TypedArray.lastIndexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  @send external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @deprecated({reason: "Use `TypedArray.copy` instead.", migrate: TypedArray.copy()}) @send
  external copy: t => t = "slice"
  @deprecated({
    reason: "Use `TypedArray.sliceToEnd` instead.",
    migrate: TypedArray.sliceToEnd(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external sliceFrom: (t, int) => t = "slice"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  @send external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external subarrayFrom: (t, int) => t = "subarray"

  @deprecated({reason: "Use `TypedArray.toString` instead.", migrate: TypedArray.toString()}) @send
  external toString: t => string = "toString"
  @deprecated({
    reason: "Use `TypedArray.toLocaleString` instead.",
    migrate: TypedArray.toLocaleString(),
  })
  @send
  external toLocaleString: t => string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : t -> (int * elt) array_iter = "" [@@send]
 */
  @deprecated({reason: "Use `TypedArray.every` instead.", migrate: TypedArray.every()}) @send
  external every: (t, elt => bool) => bool = "every"
  @deprecated({
    reason: "Use `TypedArray.everyWithIndex` instead.",
    migrate: TypedArray.everyWithIndex(),
  })
  @send
  external everyi: (t, (elt, int) => bool) => bool = "every"

  @deprecated({reason: "Use `TypedArray.filter` instead.", migrate: TypedArray.filter()}) @send
  external filter: (t, elt => bool) => t = "filter"
  @deprecated({
    reason: "Use `TypedArray.filterWithIndex` instead.",
    migrate: TypedArray.filterWithIndex(),
  })
  @send
  external filteri: (t, (elt, int) => bool) => t = "filter"

  @deprecated({reason: "Use `TypedArray.find` instead.", migrate: TypedArray.find()}) @send
  external find: (t, elt => bool) => Js_undefined.t<elt> = "find"
  @deprecated({
    reason: "Use `TypedArray.findWithIndex` instead.",
    migrate: TypedArray.findWithIndex(),
  })
  @send
  external findi: (t, (elt, int) => bool) => Js_undefined.t<elt> = "find"

  @deprecated({reason: "Use `TypedArray.findIndex` instead.", migrate: TypedArray.findIndex()})
  @send
  external findIndex: (t, elt => bool) => int = "findIndex"
  @deprecated({
    reason: "Use `TypedArray.findIndexWithIndex` instead.",
    migrate: TypedArray.findIndexWithIndex(),
  })
  @send
  external findIndexi: (t, (elt, int) => bool) => int = "findIndex"

  @deprecated({reason: "Use `TypedArray.forEach` instead.", migrate: TypedArray.forEach()}) @send
  external forEach: (t, elt => unit) => unit = "forEach"
  @deprecated({
    reason: "Use `TypedArray.forEachWithIndex` instead.",
    migrate: TypedArray.forEachWithIndex(),
  })
  @send
  external forEachi: (t, (elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : t -> int array_iter = "" [@@send]
 */

  @deprecated({reason: "Use `TypedArray.map` instead.", migrate: TypedArray.map()}) @send
  external map: (t, elt => 'b) => typed_array<'b> = "map"
  @deprecated({
    reason: "Use `TypedArray.mapWithIndex` instead.",
    migrate: TypedArray.mapWithIndex(),
  })
  @send
  external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @deprecated({reason: "Use `TypedArray.reduce` instead.", migrate: TypedArray.reduce()}) @send
  external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @deprecated({
    reason: "Use `TypedArray.reduceWithIndex` instead.",
    migrate: TypedArray.reduceWithIndex(),
  })
  @send
  external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @deprecated({reason: "Use `TypedArray.reduceRight` instead.", migrate: TypedArray.reduceRight()})
  @send
  external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @deprecated({
    reason: "Use `TypedArray.reduceRightWithIndex` instead.",
    migrate: TypedArray.reduceRightWithIndex(),
  })
  @send
  external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @deprecated({reason: "Use `TypedArray.some` instead.", migrate: TypedArray.some()}) @send
  external some: (t, elt => bool) => bool = "some"
  @deprecated({
    reason: "Use `TypedArray.someWithIndex` instead.",
    migrate: TypedArray.someWithIndex(),
  })
  @send
  external somei: (t, (elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Float32Array.Constants.bytesPerElement` instead.",
    migrate: Float32Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Float32Array.BYTES_PER_ELEMENT"

  @deprecated({reason: "Use `Float32Array.fromArray` instead.", migrate: Float32Array.fromArray()})
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
}

module Float64Array = {
  /** */
  type elt = float
  type typed_array<'a>
  type t = typed_array<elt>

  @get_index external unsafe_get: (t, int) => elt = ""
  @set_index external unsafe_set: (t, int, elt) => unit = ""

  @get external buffer: t => array_buffer = "buffer"
  @get external byteLength: t => int = "byteLength"
  @get external byteOffset: t => int = "byteOffset"

  @deprecated({reason: "Use `TypedArray.setArray` instead.", migrate: TypedArray.setArray()}) @send
  external setArray: (t, array<elt>) => unit = "set"
  @deprecated({
    reason: "Use `TypedArray.setArrayFrom` instead.",
    migrate: TypedArray.setArrayFrom(%insert.unlabelledArgument(2)),
  })
  @send
  external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @deprecated({reason: "Use `TypedArray.length` instead.", migrate: TypedArray.length()}) @get
  external length: t => int = "length"

  /* Mutator functions */
  @deprecated({
    reason: "Use `TypedArray.copyAllWithin` instead.",
    migrate: TypedArray.copyAllWithin(~target=%insert.labelledArgument("to_")),
  })
  @send
  external copyWithin: (t, ~to_: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithinToEnd` instead.",
    migrate: TypedArray.copyWithinToEnd(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("from"),
    ),
  })
  @send
  external copyWithinFrom: (t, ~to_: int, ~from: int) => t = "copyWithin"
  @deprecated({
    reason: "Use `TypedArray.copyWithin` instead.",
    migrate: TypedArray.copyWithin(
      ~target=%insert.labelledArgument("to_"),
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external copyWithinFromRange: (t, ~to_: int, ~start: int, ~end_: int) => t = "copyWithin"

  @deprecated({reason: "Use `TypedArray.fillAll` instead.", migrate: TypedArray.fillAll()}) @send
  external fillInPlace: (t, elt) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fillToEnd` instead.",
    migrate: TypedArray.fillToEnd(~start=%insert.labelledArgument("from")),
  })
  @send
  external fillFromInPlace: (t, elt, ~from: int) => t = "fill"
  @deprecated({
    reason: "Use `TypedArray.fill` instead.",
    migrate: TypedArray.fill(
      ~start=%insert.labelledArgument("start"),
      ~end=%insert.labelledArgument("end_"),
    ),
  })
  @send
  external fillRangeInPlace: (t, elt, ~start: int, ~end_: int) => t = "fill"

  @deprecated({reason: "Use `TypedArray.reverse` instead.", migrate: TypedArray.reverse()}) @send
  external reverseInPlace: t => t = "reverse"

  @deprecated({
    reason: "Use `TypedArray.toSorted` instead.",
    migrate: TypedArray.toSorted((a, b) =>
      %todo_(
        "This needs a comparator function. Use an appropriate comparator (e.g. Float.compare)."
      )
    ),
  })
  @send
  external sortInPlace: t => t = "sort"
  @deprecated({reason: "Use `TypedArray.sort` instead.", migrate: TypedArray.sort()}) @send
  external sortInPlaceWith: (t, (elt, elt) => int) => t = "sort"

  /* Accessor functions */
  @deprecated({reason: "Use `TypedArray.includes` instead.", migrate: TypedArray.includes()}) @send
  external includes: (t, elt) => bool = "includes" /* ES2016 */

  @deprecated({reason: "Use `TypedArray.indexOf` instead.", migrate: TypedArray.indexOf()}) @send
  external indexOf: (t, elt) => int = "indexOf"
  @deprecated({
    reason: "Use `TypedArray.indexOfFrom` instead.",
    migrate: TypedArray.indexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @deprecated({reason: "Use `TypedArray.joinWith` instead.", migrate: TypedArray.joinWith(",")})
  @send
  external join: t => string = "join"
  @deprecated({reason: "Use `TypedArray.joinWith` instead.", migrate: TypedArray.joinWith()}) @send
  external joinWith: (t, string) => string = "join"

  @deprecated({reason: "Use `TypedArray.lastIndexOf` instead.", migrate: TypedArray.lastIndexOf()})
  @send
  external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @deprecated({
    reason: "Use `TypedArray.lastIndexOfFrom` instead.",
    migrate: TypedArray.lastIndexOfFrom(%insert.labelledArgument("from")),
  })
  @send
  external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.slice` instead.",
    migrate: TypedArray.slice(~end=%insert.labelledArgument("end_")),
  })
  @send external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @deprecated({reason: "Use `TypedArray.copy` instead.", migrate: TypedArray.copy()}) @send
  external copy: t => t = "slice"
  @deprecated({
    reason: "Use `TypedArray.sliceToEnd` instead.",
    migrate: TypedArray.sliceToEnd(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external sliceFrom: (t, int) => t = "slice"

  /** `start` is inclusive, `end_` exclusive */
  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~end=%insert.labelledArgument("end_")),
  })
  @send external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @deprecated({
    reason: "Use `TypedArray.subarray` instead.",
    migrate: TypedArray.subarray(~start=%insert.unlabelledArgument(1)),
  })
  @send
  external subarrayFrom: (t, int) => t = "subarray"

  @deprecated({reason: "Use `TypedArray.toString` instead.", migrate: TypedArray.toString()}) @send
  external toString: t => string = "toString"
  @deprecated({
    reason: "Use `TypedArray.toLocaleString` instead.",
    migrate: TypedArray.toLocaleString(),
  })
  @send
  external toLocaleString: t => string = "toLocaleString"

  /* Iteration functions */
  /* commented out until bs has a plan for iterators
  external entries : t -> (int * elt) array_iter = "" [@@send]
 */
  @deprecated({reason: "Use `TypedArray.every` instead.", migrate: TypedArray.every()}) @send
  external every: (t, elt => bool) => bool = "every"
  @deprecated({
    reason: "Use `TypedArray.everyWithIndex` instead.",
    migrate: TypedArray.everyWithIndex(),
  })
  @send
  external everyi: (t, (elt, int) => bool) => bool = "every"

  @deprecated({reason: "Use `TypedArray.filter` instead.", migrate: TypedArray.filter()}) @send
  external filter: (t, elt => bool) => t = "filter"
  @deprecated({
    reason: "Use `TypedArray.filterWithIndex` instead.",
    migrate: TypedArray.filterWithIndex(),
  })
  @send
  external filteri: (t, (elt, int) => bool) => t = "filter"

  @deprecated({reason: "Use `TypedArray.find` instead.", migrate: TypedArray.find()}) @send
  external find: (t, elt => bool) => Js_undefined.t<elt> = "find"
  @deprecated({
    reason: "Use `TypedArray.findWithIndex` instead.",
    migrate: TypedArray.findWithIndex(),
  })
  @send
  external findi: (t, (elt, int) => bool) => Js_undefined.t<elt> = "find"

  @deprecated({reason: "Use `TypedArray.findIndex` instead.", migrate: TypedArray.findIndex()})
  @send
  external findIndex: (t, elt => bool) => int = "findIndex"
  @deprecated({
    reason: "Use `TypedArray.findIndexWithIndex` instead.",
    migrate: TypedArray.findIndexWithIndex(),
  })
  @send
  external findIndexi: (t, (elt, int) => bool) => int = "findIndex"

  @deprecated({reason: "Use `TypedArray.forEach` instead.", migrate: TypedArray.forEach()}) @send
  external forEach: (t, elt => unit) => unit = "forEach"
  @deprecated({
    reason: "Use `TypedArray.forEachWithIndex` instead.",
    migrate: TypedArray.forEachWithIndex(),
  })
  @send
  external forEachi: (t, (elt, int) => unit) => unit = "forEach"

  /* commented out until bs has a plan for iterators
  external keys : t -> int array_iter = "" [@@send]
 */

  @deprecated({reason: "Use `TypedArray.map` instead.", migrate: TypedArray.map()}) @send
  external map: (t, elt => 'b) => typed_array<'b> = "map"
  @deprecated({
    reason: "Use `TypedArray.mapWithIndex` instead.",
    migrate: TypedArray.mapWithIndex(),
  })
  @send
  external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @deprecated({reason: "Use `TypedArray.reduce` instead.", migrate: TypedArray.reduce()}) @send
  external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @deprecated({
    reason: "Use `TypedArray.reduceWithIndex` instead.",
    migrate: TypedArray.reduceWithIndex(),
  })
  @send
  external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @deprecated({reason: "Use `TypedArray.reduceRight` instead.", migrate: TypedArray.reduceRight()})
  @send
  external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @deprecated({
    reason: "Use `TypedArray.reduceRightWithIndex` instead.",
    migrate: TypedArray.reduceRightWithIndex(),
  })
  @send
  external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @deprecated({reason: "Use `TypedArray.some` instead.", migrate: TypedArray.some()}) @send
  external some: (t, elt => bool) => bool = "some"
  @deprecated({
    reason: "Use `TypedArray.someWithIndex` instead.",
    migrate: TypedArray.someWithIndex(),
  })
  @send
  external somei: (t, (elt, int) => bool) => bool = "some"

  @deprecated({
    reason: "Use `Float64Array.Constants.bytesPerElement` instead.",
    migrate: Float64Array.Constants.bytesPerElement,
  })
  @val
  external _BYTES_PER_ELEMENT: int = "Float64Array.BYTES_PER_ELEMENT"

  @deprecated({reason: "Use `Float64Array.fromArray` instead.", migrate: Float64Array.fromArray()})
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
}

/**
The DataView view provides a low-level interface for reading and writing
multiple number types in an ArrayBuffer irrespective of the platform's endianness.

**see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView)
*/
module DataView = {
  type t

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

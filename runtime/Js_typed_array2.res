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

  @new /** takes length. initializes elements to 0 */
  external make: int => t = "ArrayBuffer"

  /* ArrayBuffer.isView: seems pointless with a type system */
  /* experimental
  external transfer : array_buffer -> t = "ArrayBuffer.transfer" [@@val]
  external transferWithLength : array_buffer -> int -> t = "ArrayBuffer.transfer" [@@val]
 */

  @get external byteLength: t => int = "byteLength"

  @send external slice: (t, ~start: int, ~end_: int) => array_buffer = "slice"
  @send external sliceFrom: (t, int) => array_buffer = "slice"
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

  @send external setArray: (t, array<elt>) => unit = "set"
  @send external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @get external length: t => int = "length"

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
  @send external includes: (t, elt) => bool = "includes" /* ES2016 */

  @send external indexOf: (t, elt) => int = "indexOf"
  @send external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @send external join: t => string = "join"
  @send external joinWith: (t, string) => string = "join"

  @send external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @send external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  @send /** `start` is inclusive, `end_` exclusive */
  external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @send external copy: t => t = "slice"
  @send external sliceFrom: (t, int) => t = "slice"

  @send /** `start` is inclusive, `end_` exclusive */
  external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @send external subarrayFrom: (t, int) => t = "subarray"

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

  @send external map: (t, elt => 'b) => typed_array<'b> = "map"
  @send external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @send external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @send external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @send external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @send external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @send external some: (t, elt => bool) => bool = "some"
  @send external somei: (t, (elt, int) => bool) => bool = "some"

  @val external _BYTES_PER_ELEMENT: int = "Int8Array.BYTES_PER_ELEMENT"

  @new external make: array<elt> => t = "Int8Array"
  @new /** can throw */
  external fromBuffer: array_buffer => t = "Int8Array"

  @new /**
  **raise** Js.Exn.Error raise Js exception

  **param** offset is in bytes
  */
  external fromBufferOffset: (array_buffer, int) => t = "Int8Array"

  @new
  /**
  **raise** Js.Exn.Error raises Js exception

  **param** offset is in bytes, length in elements
  */
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Int8Array"

  @new external fromLength: int => t = "Int8Array"
  @val external from: array_like<elt> => t = "Int8Array.from"
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

  @send external setArray: (t, array<elt>) => unit = "set"
  @send external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @get external length: t => int = "length"

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
  @send external includes: (t, elt) => bool = "includes" /* ES2016 */

  @send external indexOf: (t, elt) => int = "indexOf"
  @send external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @send external join: t => string = "join"
  @send external joinWith: (t, string) => string = "join"

  @send external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @send external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  @send /** `start` is inclusive, `end_` exclusive */
  external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @send external copy: t => t = "slice"
  @send external sliceFrom: (t, int) => t = "slice"

  @send /** `start` is inclusive, `end_` exclusive */
  external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @send external subarrayFrom: (t, int) => t = "subarray"

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

  @send external map: (t, elt => 'b) => typed_array<'b> = "map"
  @send external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @send external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @send external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @send external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @send external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @send external some: (t, elt => bool) => bool = "some"
  @send external somei: (t, (elt, int) => bool) => bool = "some"

  @val external _BYTES_PER_ELEMENT: int = "Uint8Array.BYTES_PER_ELEMENT"

  @new external make: array<elt> => t = "Uint8Array"
  @new /** can throw */
  external fromBuffer: array_buffer => t = "Uint8Array"

  @new /**
  **raise** Js.Exn.Error raise Js exception

  **param** offset is in bytes
  */
  external fromBufferOffset: (array_buffer, int) => t = "Uint8Array"

  @new
  /**
  **raise** Js.Exn.Error raises Js exception

  **param** offset is in bytes, length in elements
  */
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Uint8Array"

  @new external fromLength: int => t = "Uint8Array"
  @val external from: array_like<elt> => t = "Uint8Array.from"
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

  @send external setArray: (t, array<elt>) => unit = "set"
  @send external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @get external length: t => int = "length"

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
  @send external includes: (t, elt) => bool = "includes" /* ES2016 */

  @send external indexOf: (t, elt) => int = "indexOf"
  @send external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @send external join: t => string = "join"
  @send external joinWith: (t, string) => string = "join"

  @send external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @send external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  @send /** `start` is inclusive, `end_` exclusive */
  external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @send external copy: t => t = "slice"
  @send external sliceFrom: (t, int) => t = "slice"

  @send /** `start` is inclusive, `end_` exclusive */
  external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @send external subarrayFrom: (t, int) => t = "subarray"

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

  @send external map: (t, elt => 'b) => typed_array<'b> = "map"
  @send external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @send external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @send external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @send external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @send external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @send external some: (t, elt => bool) => bool = "some"
  @send external somei: (t, (elt, int) => bool) => bool = "some"

  @val external _BYTES_PER_ELEMENT: int = "Uint8ClampedArray.BYTES_PER_ELEMENT"

  @new external make: array<elt> => t = "Uint8ClampedArray"
  @new /** can throw */
  external fromBuffer: array_buffer => t = "Uint8ClampedArray"

  @new /**
  **raise** Js.Exn.Error raise Js exception

  **param** offset is in bytes
  */
  external fromBufferOffset: (array_buffer, int) => t = "Uint8ClampedArray"

  @new
  /**
  **raise** Js.Exn.Error raises Js exception

  **param** offset is in bytes, length in elements
  */
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Uint8ClampedArray"

  @new external fromLength: int => t = "Uint8ClampedArray"
  @val external from: array_like<elt> => t = "Uint8ClampedArray.from"
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

  @send external setArray: (t, array<elt>) => unit = "set"
  @send external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @get external length: t => int = "length"

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
  @send external includes: (t, elt) => bool = "includes" /* ES2016 */

  @send external indexOf: (t, elt) => int = "indexOf"
  @send external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @send external join: t => string = "join"
  @send external joinWith: (t, string) => string = "join"

  @send external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @send external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  @send /** `start` is inclusive, `end_` exclusive */
  external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @send external copy: t => t = "slice"
  @send external sliceFrom: (t, int) => t = "slice"

  @send /** `start` is inclusive, `end_` exclusive */
  external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @send external subarrayFrom: (t, int) => t = "subarray"

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

  @send external map: (t, elt => 'b) => typed_array<'b> = "map"
  @send external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @send external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @send external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @send external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @send external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @send external some: (t, elt => bool) => bool = "some"
  @send external somei: (t, (elt, int) => bool) => bool = "some"

  @val external _BYTES_PER_ELEMENT: int = "Int16Array.BYTES_PER_ELEMENT"

  @new external make: array<elt> => t = "Int16Array"
  @new /** can throw */
  external fromBuffer: array_buffer => t = "Int16Array"

  @new /**
  **raise** Js.Exn.Error raise Js exception

  **param** offset is in bytes
  */
  external fromBufferOffset: (array_buffer, int) => t = "Int16Array"

  @new
  /**
  **raise** Js.Exn.Error raises Js exception

  **param** offset is in bytes, length in elements
  */
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
  @get external length: t => int = "length"

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
  @send external includes: (t, elt) => bool = "includes" /* ES2016 */

  @send external indexOf: (t, elt) => int = "indexOf"
  @send external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @send external join: t => string = "join"
  @send external joinWith: (t, string) => string = "join"

  @send external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @send external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  @send /** `start` is inclusive, `end_` exclusive */
  external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @send external copy: t => t = "slice"
  @send external sliceFrom: (t, int) => t = "slice"

  @send /** `start` is inclusive, `end_` exclusive */
  external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @send external subarrayFrom: (t, int) => t = "subarray"

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

  @send external map: (t, elt => 'b) => typed_array<'b> = "map"
  @send external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @send external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @send external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @send external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @send external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @send external some: (t, elt => bool) => bool = "some"
  @send external somei: (t, (elt, int) => bool) => bool = "some"

  @val external _BYTES_PER_ELEMENT: int = "Uint16Array.BYTES_PER_ELEMENT"

  @new external make: array<elt> => t = "Uint16Array"
  @new /** can throw */
  external fromBuffer: array_buffer => t = "Uint16Array"

  @new /**
  **raise** Js.Exn.Error raise Js exception

  **param** offset is in bytes
  */
  external fromBufferOffset: (array_buffer, int) => t = "Uint16Array"

  @new
  /**
  **raise** Js.Exn.Error raises Js exception

  **param** offset is in bytes, length in elements
  */
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Uint16Array"

  @new external fromLength: int => t = "Uint16Array"
  @val external from: array_like<elt> => t = "Uint16Array.from"
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

  @send external setArray: (t, array<elt>) => unit = "set"
  @send external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @get external length: t => int = "length"

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
  @send external includes: (t, elt) => bool = "includes" /* ES2016 */

  @send external indexOf: (t, elt) => int = "indexOf"
  @send external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @send external join: t => string = "join"
  @send external joinWith: (t, string) => string = "join"

  @send external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @send external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  @send /** `start` is inclusive, `end_` exclusive */
  external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @send external copy: t => t = "slice"
  @send external sliceFrom: (t, int) => t = "slice"

  @send /** `start` is inclusive, `end_` exclusive */
  external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @send external subarrayFrom: (t, int) => t = "subarray"

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

  @send external map: (t, elt => 'b) => typed_array<'b> = "map"
  @send external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @send external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @send external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @send external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @send external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @send external some: (t, elt => bool) => bool = "some"
  @send external somei: (t, (elt, int) => bool) => bool = "some"

  @val external _BYTES_PER_ELEMENT: int = "Int32Array.BYTES_PER_ELEMENT"

  @new external make: array<elt> => t = "Int32Array"
  @new /** can throw */
  external fromBuffer: array_buffer => t = "Int32Array"

  @new /**
  **raise** Js.Exn.Error raise Js exception

  **param** offset is in bytes
  */
  external fromBufferOffset: (array_buffer, int) => t = "Int32Array"

  @new
  /**
  **raise** Js.Exn.Error raises Js exception

  **param** offset is in bytes, length in elements
  */
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Int32Array"

  @new external fromLength: int => t = "Int32Array"
  @val external from: array_like<elt> => t = "Int32Array.from"
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

  @send external setArray: (t, array<elt>) => unit = "set"
  @send external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @get external length: t => int = "length"

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
  @send external includes: (t, elt) => bool = "includes" /* ES2016 */

  @send external indexOf: (t, elt) => int = "indexOf"
  @send external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @send external join: t => string = "join"
  @send external joinWith: (t, string) => string = "join"

  @send external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @send external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  @send /** `start` is inclusive, `end_` exclusive */
  external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @send external copy: t => t = "slice"
  @send external sliceFrom: (t, int) => t = "slice"

  @send /** `start` is inclusive, `end_` exclusive */
  external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @send external subarrayFrom: (t, int) => t = "subarray"

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

  @send external map: (t, elt => 'b) => typed_array<'b> = "map"
  @send external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @send external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @send external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @send external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @send external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @send external some: (t, elt => bool) => bool = "some"
  @send external somei: (t, (elt, int) => bool) => bool = "some"

  @val external _BYTES_PER_ELEMENT: int = "Uint32Array.BYTES_PER_ELEMENT"

  @new external make: array<elt> => t = "Uint32Array"
  @new /** can throw */
  external fromBuffer: array_buffer => t = "Uint32Array"

  @new /**
  **raise** Js.Exn.Error raise Js exception

  **param** offset is in bytes
  */
  external fromBufferOffset: (array_buffer, int) => t = "Uint32Array"

  @new
  /**
  **raise** Js.Exn.Error raises Js exception

  **param** offset is in bytes, length in elements
  */
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Uint32Array"

  @new external fromLength: int => t = "Uint32Array"
  @val external from: array_like<elt> => t = "Uint32Array.from"
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

  @send external setArray: (t, array<elt>) => unit = "set"
  @send external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @get external length: t => int = "length"

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
  @send external includes: (t, elt) => bool = "includes" /* ES2016 */

  @send external indexOf: (t, elt) => int = "indexOf"
  @send external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @send external join: t => string = "join"
  @send external joinWith: (t, string) => string = "join"

  @send external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @send external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  @send /** `start` is inclusive, `end_` exclusive */
  external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @send external copy: t => t = "slice"
  @send external sliceFrom: (t, int) => t = "slice"

  @send /** `start` is inclusive, `end_` exclusive */
  external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @send external subarrayFrom: (t, int) => t = "subarray"

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

  @send external map: (t, elt => 'b) => typed_array<'b> = "map"
  @send external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @send external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @send external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @send external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @send external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @send external some: (t, elt => bool) => bool = "some"
  @send external somei: (t, (elt, int) => bool) => bool = "some"

  @val external _BYTES_PER_ELEMENT: int = "Float32Array.BYTES_PER_ELEMENT"

  @new external make: array<elt> => t = "Float32Array"
  @new /** can throw */
  external fromBuffer: array_buffer => t = "Float32Array"

  @new /**
  **raise** Js.Exn.Error raise Js exception

  **param** offset is in bytes
  */
  external fromBufferOffset: (array_buffer, int) => t = "Float32Array"

  @new
  /**
  **raise** Js.Exn.Error raises Js exception

  **param** offset is in bytes, length in elements
  */
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Float32Array"

  @new external fromLength: int => t = "Float32Array"
  @val external from: array_like<elt> => t = "Float32Array.from"
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

  @send external setArray: (t, array<elt>) => unit = "set"
  @send external setArrayOffset: (t, array<elt>, int) => unit = "set"
  /* There's also an overload for typed arrays, but don't know how to model that without subtyping */

  /* Array interface(-ish) */
  @get external length: t => int = "length"

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
  @send external includes: (t, elt) => bool = "includes" /* ES2016 */

  @send external indexOf: (t, elt) => int = "indexOf"
  @send external indexOfFrom: (t, elt, ~from: int) => int = "indexOf"

  @send external join: t => string = "join"
  @send external joinWith: (t, string) => string = "join"

  @send external lastIndexOf: (t, elt) => int = "lastIndexOf"
  @send external lastIndexOfFrom: (t, elt, ~from: int) => int = "lastIndexOf"

  @send /** `start` is inclusive, `end_` exclusive */
  external slice: (t, ~start: int, ~end_: int) => t = "slice"

  @send external copy: t => t = "slice"
  @send external sliceFrom: (t, int) => t = "slice"

  @send /** `start` is inclusive, `end_` exclusive */
  external subarray: (t, ~start: int, ~end_: int) => t = "subarray"

  @send external subarrayFrom: (t, int) => t = "subarray"

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

  @send external map: (t, elt => 'b) => typed_array<'b> = "map"
  @send external mapi: (t, (elt, int) => 'b) => typed_array<'b> = "map"

  @send external reduce: (t, ('b, elt) => 'b, 'b) => 'b = "reduce"
  @send external reducei: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduce"

  @send external reduceRight: (t, ('b, elt) => 'b, 'b) => 'b = "reduceRight"
  @send external reduceRighti: (t, ('b, elt, int) => 'b, 'b) => 'b = "reduceRight"

  @send external some: (t, elt => bool) => bool = "some"
  @send external somei: (t, (elt, int) => bool) => bool = "some"

  @val external _BYTES_PER_ELEMENT: int = "Float64Array.BYTES_PER_ELEMENT"

  @new external make: array<elt> => t = "Float64Array"
  @new /** can throw */
  external fromBuffer: array_buffer => t = "Float64Array"

  @new /**
  **raise** Js.Exn.Error raise Js exception

  **param** offset is in bytes
  */
  external fromBufferOffset: (array_buffer, int) => t = "Float64Array"

  @new
  /**
  **raise** Js.Exn.Error raises Js exception

  **param** offset is in bytes, length in elements
  */
  external fromBufferRange: (array_buffer, ~offset: int, ~length: int) => t = "Float64Array"

  @new external fromLength: int => t = "Float64Array"
  @val external from: array_like<elt> => t = "Float64Array.from"
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

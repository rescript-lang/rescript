/** The `BigInt64Array` typed array represents an array of 64-bit signed integers in platform byte order. See [BigInt64Array on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt64Array)
*/
type t = Stdlib_TypedArray.t<bigint>

module Constants = {
  /**`bytesPerElement` returns the element size. See [BYTES_PER_ELEMENT on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/BYTES_PER_ELEMENT)
  */
  @val
  external bytesPerElement: int = "BigInt64Array.BYTES_PER_ELEMENT"
}

/** `fromArray` creates a `BigInt64Array` from an array of values. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt64Array/BigInt64Array)
*/
@new
external fromArray: array<bigint> => t = "BigInt64Array"

/** `fromBuffer` creates a `BigInt64Array` from an `ArrayBuffer.t`. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt64Array/BigInt64Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds. 
*/
@new
external fromBuffer: Stdlib_ArrayBuffer.t => t = "BigInt64Array"

/** `fromBufferToEnd` creates a `BigInt64Array` from an `ArrayBuffer.t`, starting at a particular offset and continuing through to the end. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt64Array/BigInt64Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds. 
*/
@new
external fromBufferToEnd: (Stdlib_ArrayBuffer.t, ~byteOffset: int) => t = "BigInt64Array"

/** `fromBufferWithRange` creates a `BigInt64Array` from an `ArrayBuffer.t`, starting at a particular offset and consuming `length` **bytes**. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt64Array/BigInt64Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds. 
*/
@new
external fromBufferWithRange: (Stdlib_ArrayBuffer.t, ~byteOffset: int, ~length: int) => t =
  "BigInt64Array"

/** `fromLength` creates a zero-initialized `BigInt64Array` to hold the specified count of numbers; this is **not** a byte length. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt64Array/BigInt64Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds. 
*/
@new
external fromLength: int => t = "BigInt64Array"

/** `fromArrayLikeOrIterable` creates a `BigInt64Array` from an array-like or iterable object. See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@val
external fromArrayLikeOrIterable: 'a => t = "BigInt64Array.from"

/** `fromArrayLikeOrIterableWithMap` creates a `BigInt64Array` from an array-like or iterable object and applies the mapping function to each item. The mapping function expects (value, index). See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@val
external fromArrayLikeOrIterableWithMap: ('a, ('b, int) => bigint) => t = "BigInt64Array.from"

/**
  `ignore(bigIntArray)` ignores the provided bigIntArray and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

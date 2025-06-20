/** The `Uint8Array` typed array represents an array of 8-bit unsigned integers. See [Uint8Array on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8Array)
*/
type t = Stdlib_TypedArray.t<int>

module Constants = {
  /**`bytesPerElement` returns the element size. See [BYTES_PER_ELEMENT on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/BYTES_PER_ELEMENT)
  */
  @val
  external bytesPerElement: int = "Uint8Array.BYTES_PER_ELEMENT"
}

/** `fromArray` creates a `Uint8Array` from an array of values. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8Array/Uint8Array)
*/
@new
external fromArray: array<int> => t = "Uint8Array"

/** `fromBuffer` creates a `Uint8Array` from an `ArrayBuffer.t`. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8Array/Uint8Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds. 
*/
@new
external fromBuffer: Stdlib_ArrayBuffer.t => t = "Uint8Array"

/** `fromBufferToEnd` creates a `Uint8Array` from an `ArrayBuffer.t`, starting at a particular offset and continuing through to the end. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8Array/Uint8Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds. 
*/
@new
external fromBufferToEnd: (Stdlib_ArrayBuffer.t, ~byteOffset: int) => t = "Uint8Array"

/** `fromBufferWithRange` creates a `Uint8Array` from an `ArrayBuffer.t`, starting at a particular offset and consuming `length` **bytes**. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8Array/Uint8Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds. 
*/
@new
external fromBufferWithRange: (Stdlib_ArrayBuffer.t, ~byteOffset: int, ~length: int) => t =
  "Uint8Array"

/** `fromLength` creates a zero-initialized `Uint8Array` to hold the specified count of numbers; this is **not** a byte length. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8Array/Uint8Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds. 
*/
@new
external fromLength: int => t = "Uint8Array"

/** `fromArrayLikeOrIterable` creates a `Uint8Array` from an array-like or iterable object. See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@val
external fromArrayLikeOrIterable: 'a => t = "Uint8Array.from"

/** `fromArrayLikeOrIterableWithMap` creates a `Uint8Array` from an array-like or iterable object and applies the mapping function to each item. The mapping function expects (value, index). See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@val
external fromArrayLikeOrIterableWithMap: ('a, ('b, int) => int) => t = "Uint8Array.from"

/**
  `ignore(uintArray)` ignores the provided uintArray and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

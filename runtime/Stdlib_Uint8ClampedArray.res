/** The `Uint8ClampedArray` typed array represents an array of 8-bit unsigned integers clamped to 0-255. See [Uint8ClampedArray on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray)
*/
type t = Stdlib_TypedArray.t<int>

module Constants = {
  /**`bytesPerElement` returns the element size. See [BYTES_PER_ELEMENT on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/BYTES_PER_ELEMENT)
  */
  @val
  external bytesPerElement: int = "Uint8ClampedArray.BYTES_PER_ELEMENT"
}

/** `fromArray` creates a `Uint8ClampedArray` from an array of values. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray/Uint8ClampedArray)
*/
@new
external fromArray: array<int> => t = "Uint8ClampedArray"

/** `fromBuffer` creates a `Uint8ClampedArray` from an `ArrayBuffer.t`. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray/Uint8ClampedArray)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@new
external fromBuffer: (Stdlib_ArrayBuffer.t, ~byteOffset: int=?, ~length: int=?) => t =
  "Uint8ClampedArray"

/** `fromBufferToEnd` creates a `Uint8ClampedArray` from an `ArrayBuffer.t`, starting at a particular offset and continuing through to the end. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray/Uint8ClampedArray)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@deprecated("Use `fromBuffer` instead") @new
external fromBufferToEnd: (Stdlib_ArrayBuffer.t, ~byteOffset: int) => t = "Uint8ClampedArray"

/** `fromBufferWithRange` creates a `Uint8ClampedArray` from an `ArrayBuffer.t`, starting at a particular offset and consuming `length` **bytes**. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray/Uint8ClampedArray)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@deprecated("Use `fromBuffer` instead") @new
external fromBufferWithRange: (Stdlib_ArrayBuffer.t, ~byteOffset: int, ~length: int) => t =
  "Uint8ClampedArray"

/** `fromLength` creates a zero-initialized `Uint8ClampedArray` to hold the specified count of numbers; this is **not** a byte length. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8ClampedArray/Uint8ClampedArray)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@new
external fromLength: int => t = "Uint8ClampedArray"

/** `fromArrayLikeOrIterable` creates a `Uint8ClampedArray` from an array-like or iterable object. See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@val
external fromArrayLikeOrIterable: ('a, ~map: ('b, int) => int=?) => t = "Uint8ClampedArray.from"

/** `fromArrayLikeOrIterableWithMap` creates a `Uint8ClampedArray` from an array-like or iterable object and applies the mapping function to each item. The mapping function expects (value, index). See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@deprecated("Use `fromArrayLikeOrIterable` instead") @val
external fromArrayLikeOrIterableWithMap: ('a, ('b, int) => int) => t = "Uint8ClampedArray.from"

/**
  `ignore(uintArray)` ignores the provided uintArray and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

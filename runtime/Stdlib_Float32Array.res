/** The `Float32Array` typed array represents an array of 32-bit floating point numbers in platform byte order. See [Float32Array on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array)
*/
type t = Stdlib_TypedArray.t<float>

module Constants = {
  /**`bytesPerElement` returns the element size. See [BYTES_PER_ELEMENT on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/BYTES_PER_ELEMENT)
  */
  @val
  external bytesPerElement: int = "Float32Array.BYTES_PER_ELEMENT"
}

/** `fromArray` creates a `Float32Array` from an array of values. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array/Float32Array)
*/
@new
external fromArray: array<float> => t = "Float32Array"

/** `fromBuffer` creates a `Float32Array` from an `ArrayBuffer.t`. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array/Float32Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds. 
*/
@new
external fromBuffer: Stdlib_ArrayBuffer.t => t = "Float32Array"

/** `fromBufferToEnd` creates a `Float32Array` from an `ArrayBuffer.t`, starting at a particular offset and continuing through to the end. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array/Float32Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds. 
*/
@new
external fromBufferToEnd: (Stdlib_ArrayBuffer.t, ~byteOffset: int) => t = "Float32Array"

/** `fromBufferWithRange` creates a `Float32Array` from an `ArrayBuffer.t`, starting at a particular offset and consuming `length` **bytes**. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array/Float32Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds. 
*/
@new
external fromBufferWithRange: (Stdlib_ArrayBuffer.t, ~byteOffset: int, ~length: int) => t =
  "Float32Array"

/** `fromLength` creates a zero-initialized `Float32Array` to hold the specified count of numbers; this is **not** a byte length. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float32Array/Float32Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds. 
*/
@new
external fromLength: int => t = "Float32Array"

/** `fromArrayLikeOrIterable` creates a `Float32Array` from an array-like or iterable object. See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@val
external fromArrayLikeOrIterable: 'a => t = "Float32Array.from"

/** `fromArrayLikeOrIterableWithMap` creates a `Float32Array` from an array-like or iterable object and applies the mapping function to each item. The mapping function expects (value, index). See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@val
external fromArrayLikeOrIterableWithMap: ('a, ('b, int) => float) => t = "Float32Array.from"

/**
  `ignore(floatArray)` ignores the provided floatArray and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

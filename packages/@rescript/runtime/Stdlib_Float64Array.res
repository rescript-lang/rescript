/** The `Float64Array` typed array represents an array of 64-bit floating point numbers in platform byte order. See [Float64Array on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array)
*/
@editor.completeFrom(Stdlib.TypedArray)
type t = Stdlib_TypedArray.t<float>

module Constants = {
  /**`bytesPerElement` returns the element size. See [BYTES_PER_ELEMENT on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/BYTES_PER_ELEMENT)
  */
  @val
  external bytesPerElement: int = "Float64Array.BYTES_PER_ELEMENT"
}

/** `fromArray` creates a `Float64Array` from an array of values. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array/Float64Array)
*/
@new
external fromArray: array<float> => t = "Float64Array"

/** `fromBuffer` creates a `Float64Array` from an `ArrayBuffer.t`. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array/Float64Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@new
external fromBuffer: (Stdlib_ArrayBuffer.t, ~byteOffset: int=?, ~length: int=?) => t =
  "Float64Array"

/** `fromBufferToEnd` creates a `Float64Array` from an `ArrayBuffer.t`, starting at a particular offset and continuing through to the end. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array/Float64Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@deprecated({
  reason: "Use `fromBuffer` instead",
  migrate: Float64Array.fromBuffer(),
})
@new
external fromBufferToEnd: (Stdlib_ArrayBuffer.t, ~byteOffset: int) => t = "Float64Array"

/** `fromBufferWithRange` creates a `Float64Array` from an `ArrayBuffer.t`, starting at a particular offset and consuming `length` **bytes**. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array/Float64Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@deprecated({
  reason: "Use `fromBuffer` instead",
  migrate: Float64Array.fromBuffer(),
})
@new
external fromBufferWithRange: (Stdlib_ArrayBuffer.t, ~byteOffset: int, ~length: int) => t =
  "Float64Array"

/** `fromLength` creates a zero-initialized `Float64Array` to hold the specified count of numbers; this is **not** a byte length. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Float64Array/Float64Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@new
external fromLength: int => t = "Float64Array"

/** `fromArrayLikeOrIterable` creates a `Float64Array` from an array-like or iterable object. See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@val
external fromArrayLikeOrIterable: ('a, ~map: ('b, int) => float=?) => t = "Float64Array.from"

/** `fromArrayLikeOrIterableWithMap` creates a `Float64Array` from an array-like or iterable object and applies the mapping function to each item. The mapping function expects (value, index). See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@deprecated({
  reason: "Use `fromArrayLikeOrIterable` instead",
  migrate: Float64Array.fromArrayLikeOrIterable(
    %insert.unlabelledArgument(0),
    ~map=%insert.unlabelledArgument(1),
  ),
})
@val
external fromArrayLikeOrIterableWithMap: ('a, ('b, int) => float) => t = "Float64Array.from"

/**
  `ignore(floatArray)` ignores the provided floatArray and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

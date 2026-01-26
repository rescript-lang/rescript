/** The `Int8Array` typed array represents an array of twos-complement 8-bit signed integers. See [Int8Array on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int8Array)
*/
@editor.completeFrom(Stdlib.TypedArray)
type t = Stdlib_TypedArray.t<int>

module Constants = {
  /**`bytesPerElement` returns the element size. See [BYTES_PER_ELEMENT on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/BYTES_PER_ELEMENT)
  */
  @val
  external bytesPerElement: int = "Int8Array.BYTES_PER_ELEMENT"
}

/** `fromArray` creates a `Int8Array` from an array of values. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int8Array/Int8Array)
*/
@new
external fromArray: array<int> => t = "Int8Array"

/** `fromBuffer` creates a `Int8Array` from an `ArrayBuffer.t`. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int8Array/Int8Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@new
external fromBuffer: (Stdlib_ArrayBuffer.t, ~byteOffset: int=?, ~length: int=?) => t = "Int8Array"

/** `fromBufferToEnd` creates a `Int8Array` from an `ArrayBuffer.t`, starting at a particular offset and continuing through to the end. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int8Array/Int8Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@deprecated({
  reason: "Use `fromBuffer` instead",
  migrate: Int8Array.fromBuffer(),
})
@new
external fromBufferToEnd: (Stdlib_ArrayBuffer.t, ~byteOffset: int) => t = "Int8Array"

/** `fromBufferWithRange` creates a `Int8Array` from an `ArrayBuffer.t`, starting at a particular offset and consuming `length` **bytes**. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int8Array/Int8Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@deprecated({
  reason: "Use `fromBuffer` instead",
  migrate: Int8Array.fromBuffer(),
})
@new
external fromBufferWithRange: (Stdlib_ArrayBuffer.t, ~byteOffset: int, ~length: int) => t =
  "Int8Array"

/** `fromLength` creates a zero-initialized `Int8Array` to hold the specified count of numbers; this is **not** a byte length. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int8Array/Int8Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@new
external fromLength: int => t = "Int8Array"

/** `fromArrayLikeOrIterable` creates a `Int8Array` from an array-like or iterable object. See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@val
external fromArrayLikeOrIterable: ('a, ~map: ('b, int) => int=?) => t = "Int8Array.from"

/** `fromArrayLikeOrIterableWithMap` creates a `Int8Array` from an array-like or iterable object and applies the mapping function to each item. The mapping function expects (value, index). See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@deprecated({
  reason: "Use `fromArrayLikeOrIterable` instead",
  migrate: Int8Array.fromArrayLikeOrIterable(
    %insert.unlabelledArgument(0),
    ~map=%insert.unlabelledArgument(1),
  ),
})
@val
external fromArrayLikeOrIterableWithMap: ('a, ('b, int) => int) => t = "Int8Array.from"

/**
  `ignore(intArray)` ignores the provided intArray and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

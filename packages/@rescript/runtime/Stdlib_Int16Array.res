/** The `Int16Array` typed array represents an array of twos-complement 16-bit signed integers in platform byte order. See [Int16Array on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int16Array)
*/
@editor.completeFrom(Stdlib.TypedArray)
type t = Stdlib_TypedArray.t<int>

module Constants = {
  /**`bytesPerElement` returns the element size. See [BYTES_PER_ELEMENT on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/BYTES_PER_ELEMENT)
  */
  @val
  external bytesPerElement: int = "Int16Array.BYTES_PER_ELEMENT"
}

/** `fromArray` creates a `Int16Array` from an array of values. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int16Array/Int16Array)
*/
@new
external fromArray: array<int> => t = "Int16Array"

/** `fromBuffer` creates a `Int16Array` from an `ArrayBuffer.t`. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int16Array/Int16Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@new
external fromBuffer: (Stdlib_ArrayBuffer.t, ~byteOffset: int=?, ~length: int=?) => t = "Int16Array"

/** `fromBufferToEnd` creates a `Int16Array` from an `ArrayBuffer.t`, starting at a particular offset and continuing through to the end. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int16Array/Int16Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@deprecated({
  reason: "Use `fromBuffer` instead",
  migrate: Int16Array.fromBuffer(),
})
@new
external fromBufferToEnd: (Stdlib_ArrayBuffer.t, ~byteOffset: int) => t = "Int16Array"

/** `fromBufferWithRange` creates a `Int16Array` from an `ArrayBuffer.t`, starting at a particular offset and consuming `length` **bytes**. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int16Array/Int16Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@deprecated({
  reason: "Use `fromBuffer` instead",
  migrate: Int16Array.fromBuffer(),
})
@new
external fromBufferWithRange: (Stdlib_ArrayBuffer.t, ~byteOffset: int, ~length: int) => t =
  "Int16Array"

/** `fromLength` creates a zero-initialized `Int16Array` to hold the specified count of numbers; this is **not** a byte length. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Int16Array/Int16Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@new
external fromLength: int => t = "Int16Array"

/** `fromArrayLikeOrIterable` creates a `Int16Array` from an array-like or iterable object. See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@val
external fromArrayLikeOrIterable: ('a, ~map: ('b, int) => int=?) => t = "Int16Array.from"

/** `fromArrayLikeOrIterableWithMap` creates a `Int16Array` from an array-like or iterable object and applies the mapping function to each item. The mapping function expects (value, index). See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@deprecated({
  reason: "Use `fromArrayLikeOrIterable` instead",
  migrate: Int16Array.fromArrayLikeOrIterable(
    %insert.unlabelledArgument(0),
    ~map=%insert.unlabelledArgument(1),
  ),
})
@val
external fromArrayLikeOrIterableWithMap: ('a, ('b, int) => int) => t = "Int16Array.from"

/**
  `ignore(intArray)` ignores the provided intArray and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

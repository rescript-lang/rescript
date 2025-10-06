/** The `Uint16Array` typed array represents an array of 16-bit unsigned integers in platform byte order. See [Uint16Array on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint16Array)
*/
@editor.completeFrom(Stdlib.TypedArray)
type t = Stdlib_TypedArray.t<int>

module Constants = {
  /**`bytesPerElement` returns the element size. See [BYTES_PER_ELEMENT on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/BYTES_PER_ELEMENT)
  */
  @val
  external bytesPerElement: int = "Uint16Array.BYTES_PER_ELEMENT"
}

/** `fromArray` creates a `Uint16Array` from an array of values. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint16Array/Uint16Array)
*/
@new
external fromArray: array<int> => t = "Uint16Array"

/** `fromBuffer` creates a `Uint16Array` from an `ArrayBuffer.t`. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint16Array/Uint16Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@new
external fromBuffer: (Stdlib_ArrayBuffer.t, ~byteOffset: int=?, ~length: int=?) => t = "Uint16Array"

/** `fromBufferToEnd` creates a `Uint16Array` from an `ArrayBuffer.t`, starting at a particular offset and continuing through to the end. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint16Array/Uint16Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@deprecated({
  reason: "Use `fromBuffer` instead",
  migrate: Uint16Array.fromBuffer(
    %insert.unlabelledArgument(0),
    ~byteOffset=%insert.labelledArgument("byteOffset"),
  ),
})
@new
external fromBufferToEnd: (Stdlib_ArrayBuffer.t, ~byteOffset: int) => t = "Uint16Array"

/** `fromBufferWithRange` creates a `Uint16Array` from an `ArrayBuffer.t`, starting at a particular offset and consuming `length` **bytes**. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint16Array/Uint16Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@deprecated({
  reason: "Use `fromBuffer` instead",
  migrate: Uint16Array.fromBuffer(
    %insert.unlabelledArgument(0),
    ~byteOffset=%insert.labelledArgument("byteOffset"),
    ~length=%insert.labelledArgument("length"),
  ),
})
@new
external fromBufferWithRange: (Stdlib_ArrayBuffer.t, ~byteOffset: int, ~length: int) => t =
  "Uint16Array"

/** `fromLength` creates a zero-initialized `Uint16Array` to hold the specified count of numbers; this is **not** a byte length. See [TypedArray constructor on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint16Array/Uint16Array)

**Note:** This is a potentially unsafe operation. Ensure the buffer is large enough and only accessed within its bounds.
*/
@new
external fromLength: int => t = "Uint16Array"

/** `fromArrayLikeOrIterable` creates a `Uint16Array` from an array-like or iterable object. See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@val
external fromArrayLikeOrIterable: ('a, ~map: ('b, int) => int=?) => t = "Uint16Array.from"

/** `fromArrayLikeOrIterableWithMap` creates a `Uint16Array` from an array-like or iterable object and applies the mapping function to each item. The mapping function expects (value, index). See [TypedArray.from on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/from)
*/
@deprecated({
  reason: "Use `fromArrayLikeOrIterable` instead",
  migrate: Uint16Array.fromArrayLikeOrIterable(
    %insert.unlabelledArgument(0),
    ~map=%insert.unlabelledArgument(1),
  ),
})
@val
external fromArrayLikeOrIterableWithMap: ('a, ('b, int) => int) => t = "Uint16Array.from"

/**
  `ignore(uintArray)` ignores the provided uintArray and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

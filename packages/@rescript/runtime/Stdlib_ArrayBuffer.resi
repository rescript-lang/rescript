/***
Functions for interacting with JavaScript ArrayBuffer.
See: [`ArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer).
*/

/**
Type representing an ArrayBuffer object used to represent a generic raw binary data buffer.
*/
@notUndefined
type t

/**
`make(length)` creates a new ArrayBuffer with the specified length in bytes.
See [`ArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer/ArrayBuffer) on MDN.

## Examples

```rescript
let buffer = ArrayBuffer.make(8)
ArrayBuffer.byteLength(buffer) == 8
```

## Exceptions

- `RangeError`: If `length` is larger than `Number.MAX_SAFE_INTEGER` or negative.
*/
@new external make: int => t = "ArrayBuffer"

/**
`byteLength(arrayBuffer)` returns the size, in bytes, of the ArrayBuffer.
See [`ArrayBuffer.byteLength`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer/byteLength) on MDN.

## Examples

```rescript
let buffer = ArrayBuffer.make(16)
ArrayBuffer.byteLength(buffer) == 16
```
*/
@get external byteLength: t => int = "byteLength"

/**
`slice(arrayBuffer, ~start, ~end)` returns a new ArrayBuffer whose contents are a copy of this ArrayBuffer's bytes from `start`, inclusive, up to `end`, exclusive.
See [`ArrayBuffer.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer/slice) on MDN.

## Examples

```rescript
let buffer = ArrayBuffer.make(16)
let sliced = buffer->ArrayBuffer.slice(~start=4, ~end=12)
ArrayBuffer.byteLength(sliced) == 8
```
*/
@send external slice: (t, ~start: int=?, ~end: int=?) => t = "slice"

@deprecated({
  reason: "Use `slice` instead.",
  migrate: ArrayBuffer.slice(),
})
@send
external sliceToEnd: (t, ~start: int) => t = "slice"

/**
  `ignore(arrayBuffer)` ignores the provided arrayBuffer and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

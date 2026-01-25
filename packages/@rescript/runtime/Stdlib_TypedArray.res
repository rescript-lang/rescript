@notUndefined
type t<'a>

/**
`get(typedArray, index)` returns the element at `index` of `typedArray`.
Returns `None` if the index does not exist in the typed array. Equivalent to doing `typedArray[index]` in JavaScript.

## Examples

```rescript
let view = Int32Array.fromArray([1, 2, 3])
TypedArray.get(view, 0) == Some(1)
TypedArray.get(view, 10) == None
```
*/
@get_index
external get: (t<'a>, int) => option<'a> = ""

/**
`set(typedArray, index, item)` sets the provided `item` at `index` of `typedArray`.

Beware this will *mutate* the array.

## Examples

```rescript
let view = Int32Array.fromArray([1, 2])
TypedArray.set(view, 1, 5)
TypedArray.get(view, 1) == Some(5)
```
*/
@set_index
external set: (t<'a>, int, 'a) => unit = ""

/**
`buffer(typedArray)` returns the underlying `ArrayBuffer` backing this view.

See [`TypedArray.prototype.buffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/buffer) on MDN.
*/
@get
external buffer: t<'a> => Stdlib_ArrayBuffer.t = "buffer"

/**
`byteLength(typedArray)` returns the length in bytes of the view.

See [`TypedArray.prototype.byteLength`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/byteLength) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([1, 2])
TypedArray.byteLength(view) == 8
```
*/
@get
external byteLength: t<'a> => int = "byteLength"

/**
`byteOffset(typedArray)` returns the offset in bytes from the start of the buffer.

See [`TypedArray.prototype.byteOffset`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/byteOffset) on MDN.
*/
@get
external byteOffset: t<'a> => int = "byteOffset"

/**
`setArray(target, source)` copies the values from `source` into `target`, mutating it.

See [`TypedArray.prototype.set`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/set) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([0, 0])
TypedArray.setArray(view, [1, 2])
TypedArray.toString(view) == "1,2"
```
*/
@send
external setArray: (t<'a>, array<'a>) => unit = "set"

/**
`setArrayFrom(target, source, index)` copies `source` into `target` starting at `index`.

See [`TypedArray.prototype.set`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/set) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([0, 0, 0])
TypedArray.setArrayFrom(view, [5, 6], 1)
TypedArray.toString(view) == "0,5,6"
```
*/
@send
external setArrayFrom: (t<'a>, array<'a>, int) => unit = "set"

/**
`length(typedArray)` returns the number of elements.

See [`TypedArray.prototype.length`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/length) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([1, 2, 3])
TypedArray.length(view) == 3
```
*/
@get
external length: t<'a> => int = "length"

/**
`copyAllWithin(typedArray, ~target)` copies values starting at index `0` over the positions beginning at `target`.

Beware this will *mutate* the typed array.

See [`TypedArray.prototype.copyWithin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/copyWithin) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([10, 20, 30])
let _ = TypedArray.copyAllWithin(view, ~target=1)
TypedArray.toString(view) == "10,10,20"
```
*/
@send external copyAllWithin: (t<'a>, ~target: int) => array<'a> = "copyWithin"
/**
`copyWithinToEnd(typedArray, ~target, ~start)` copies values from `start` through the end of the view into the range beginning at `target`.

Beware this will *mutate* the typed array.

See [`TypedArray.prototype.copyWithin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/copyWithin) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([1, 2, 3, 4])
let _ = TypedArray.copyWithinToEnd(view, ~target=0, ~start=2)
TypedArray.toString(view) == "3,4,3,4"
```
*/
@deprecated({
  reason: "Use `copyWithin` instead",
  migrate: TypedArray.copyWithin(),
})
@send
@send
external copyWithinToEnd: (t<'a>, ~target: int, ~start: int) => array<'a> = "copyWithin"
/**
`copyWithin(typedArray, ~target, ~start, ~end)` copies the section `[start, end)` onto the range beginning at `target`.

Beware this will *mutate* the typed array.

See [`TypedArray.prototype.copyWithin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/copyWithin) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([1, 2, 3, 4])
let _ = TypedArray.copyWithin(view, ~target=1, ~start=2, ~end=4)
TypedArray.toString(view) == "1,3,4,4"
```
*/
@send @send
external copyWithin: (t<'a>, ~target: int, ~start: int, ~end: int=?) => array<'a> = "copyWithin"

/**
`fillAll(typedArray, value)` fills every element with `value`.

Beware this will *mutate* the typed array.

See [`TypedArray.prototype.fill`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/fill) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([1, 2, 3])
let _ = TypedArray.fillAll(view, 9)
TypedArray.toString(view) == "9,9,9"
```
*/
@send external fillAll: (t<'a>, 'a) => t<'a> = "fill"
/**
`fillToEnd(typedArray, value, ~start)` fills from `start` through the end with `value`.

Beware this will *mutate* the typed array.
*/
@deprecated({
  reason: "Use `fill` instead",
  migrate: TypedArray.fill(),
})
@send
@send
external fillToEnd: (t<'a>, 'a, ~start: int) => t<'a> = "fill"

/**
`fill(typedArray, value, ~start, ~end)` fills the half-open interval `[start, end)` with `value`.

Beware this will *mutate* the typed array.

See [`TypedArray.prototype.fill`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/fill) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([1, 2, 3, 4])
let _ = TypedArray.fill(view, 0, ~start=1, ~end=3)
TypedArray.toString(view) == "1,0,0,4"
```
*/
@send external fill: (t<'a>, 'a, ~start: int, ~end: int=?) => t<'a> = "fill"

/**
`reverse(typedArray)` reverses the elements of the view in place.

Beware this will *mutate* the typed array.

See [`TypedArray.prototype.reverse`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/reverse) on MDN.
*/
@send external reverse: t<'a> => unit = "reverse"

/**
`toReversed(typedArray)` returns a new typed array with the elements in reverse order, leaving the original untouched.

See [`TypedArray.prototype.toReversed`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/toReversed) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([1, 2, 3])
let reversed = TypedArray.toReversed(view)
TypedArray.toString(reversed) == "3,2,1"
TypedArray.toString(view) == "1,2,3"
```
*/
@send external toReversed: t<'a> => t<'a> = "toReversed"

/**
`sort(typedArray, comparator)` sorts the values in place using `comparator`.

Beware this will *mutate* the typed array.

See [`TypedArray.prototype.sort`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/sort) on MDN.
*/
@send external sort: (t<'a>, ('a, 'a) => Stdlib_Ordering.t) => unit = "sort"
/**
`toSorted(typedArray, comparator)` returns a new typed array containing the sorted values, leaving the original untouched.

See [`TypedArray.prototype.toSorted`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/toSorted) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([3, 1, 2])
let sorted = TypedArray.toSorted(view, Int.compare)
TypedArray.toString(sorted) == "1,2,3"
TypedArray.toString(view) == "3,1,2"
```
*/
@send external toSorted: (t<'a>, ('a, 'a) => Stdlib_Ordering.t) => t<'a> = "toSorted"

/**
`with(typedArray, index, value)` returns a new typed array where the element at `index` is replaced with `value`.

See [`TypedArray.prototype.with`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/with) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([1, 2, 3])
let updated = TypedArray.with(view, 1, 10)
TypedArray.toString(updated) == "1,10,3"
TypedArray.toString(view) == "1,2,3"
```
*/
@send external with: (t<'a>, int, 'a) => t<'a> = "with"

/**
`includes(typedArray, value)` returns `true` if `value` occurs in the typed array.

See [`TypedArray.prototype.includes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/includes) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([1, 2, 3])
TypedArray.includes(view, 2) == true
TypedArray.includes(view, 10) == false
```
*/
@send external includes: (t<'a>, 'a) => bool = "includes"

/**
`indexOf(typedArray, value)` returns the first index of `value`, or `-1` when not found.

See [`TypedArray.prototype.indexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/indexOf) on MDN.
*/
@send external indexOf: (t<'a>, 'a) => int = "indexOf"
/**
`indexOfFrom(typedArray, value, fromIndex)` searches for `value` starting at `fromIndex`.
*/
@send external indexOfFrom: (t<'a>, 'a, int) => int = "indexOf"

/**
`joinWith(typedArray, separator)` returns a string formed by the elements joined with `separator`.

See [`TypedArray.prototype.join`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/join) on MDN.

## Examples

```rescript
let view = Int32Array.fromArray([1, 2, 3])
TypedArray.joinWith(view, "-") == "1-2-3"
```
*/
@send external joinWith: (t<'a>, string) => string = "join"

/**
`lastIndexOf(typedArray, value)` returns the last index of `value`, or `-1` if not found.

See [`TypedArray.prototype.lastIndexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/lastIndexOf) on MDN.
*/
@send external lastIndexOf: (t<'a>, 'a) => int = "lastIndexOf"
/**
`lastIndexOfFrom(typedArray, value, fromIndex)` searches backwards starting at `fromIndex`.
*/
@send external lastIndexOfFrom: (t<'a>, 'a, int) => int = "lastIndexOf"

/**
`slice(typedArray, ~start, ~end)` returns a new typed array containing the elements in `[start, end)`.

See [`TypedArray.prototype.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/slice) on MDN.
*/
@send external slice: (t<'a>, ~start: int, ~end: int=?) => t<'a> = "slice"
/**
`sliceToEnd(typedArray, ~start)` returns the elements from `start` through the end in a new typed array.
*/
@deprecated({
  reason: "Use `slice` instead",
  migrate: TypedArray.slice(),
})
@send
external sliceToEnd: (t<'a>, ~start: int) => t<'a> = "slice"
/**
`copy(typedArray)` produces a shallow copy of the typed array.
*/
@send external copy: t<'a> => t<'a> = "slice"

/**
`subarray(typedArray, ~start, ~end)` returns a new view referencing the same buffer over `[start, end)`.

See [`TypedArray.prototype.subarray`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/subarray) on MDN.
*/
@send external subarray: (t<'a>, ~start: int, ~end: int=?) => t<'a> = "subarray"
/**
`subarrayToEnd(typedArray, ~start)` returns a new view from `start` to the end of the buffer.
*/
@deprecated({
  reason: "Use `subarray` instead",
  migrate: TypedArray.subarray(),
})
@send
external subarrayToEnd: (t<'a>, ~start: int) => t<'a> = "subarray"

/**
`toString(typedArray)` returns a comma-separated string of the elements.

See [`TypedArray.prototype.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/toString) on MDN.

## Examples

```rescript
Int32Array.fromArray([1, 2])->TypedArray.toString == "1,2"
```
*/
@send external toString: t<'a> => string = "toString"
/**
`toLocaleString(typedArray)` concatenates the elements using locale-aware formatting.

See [`TypedArray.prototype.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/toLocaleString) on MDN.
*/
@send external toLocaleString: t<'a> => string = "toLocaleString"

/**
`every(typedArray, predicate)` returns `true` if `predicate` returns `true` for every element.

See [`TypedArray.prototype.every`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/every) on MDN.
*/
@send external every: (t<'a>, 'a => bool) => bool = "every"
/**
`everyWithIndex(typedArray, checker)` is like `every` but provides the element index to `checker`.
*/
@send external everyWithIndex: (t<'a>, ('a, int) => bool) => bool = "every"

/**
`filter(typedArray, predicate)` returns a new typed array containing only elements that satisfy `predicate`.

See [`TypedArray.prototype.filter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/filter) on MDN.
*/
@send external filter: (t<'a>, 'a => bool) => t<'a> = "filter"
/**
`filterWithIndex(typedArray, predicate)` behaves like `filter` but also passes the index to `predicate`.
*/
@send external filterWithIndex: (t<'a>, ('a, int) => bool) => t<'a> = "filter"

/**
`find(typedArray, predicate)` returns the first element that satisfies `predicate`, or `None` if nothing matches.

See [`TypedArray.prototype.find`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/find) on MDN.
*/
@send external find: (t<'a>, 'a => bool) => option<'a> = "find"
/**
`findWithIndex(typedArray, predicate)` behaves like `find`, but `predicate` also receives the index.
*/
@send external findWithIndex: (t<'a>, ('a, int) => bool) => option<'a> = "find"

/**
`findLast(typedArray, predicate)` returns the last element that satisfies `predicate`.

See [`TypedArray.prototype.findLast`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/findLast) on MDN.
*/
@send external findLast: (t<'a>, 'a => bool) => option<'a> = "findLast"
/**
`findLastWithIndex(typedArray, predicate)` is the indexed variant of `findLast`.
*/
@send external findLastWithIndex: (t<'a>, ('a, int) => bool) => option<'a> = "findLast"

/**
`findIndex(typedArray, predicate)` returns the index of the first element that satisfies `predicate`, or `-1` if none do.

See [`TypedArray.prototype.findIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/findIndex) on MDN.
*/
@send external findIndex: (t<'a>, 'a => bool) => int = "findIndex"
/**
`findIndexWithIndex(typedArray, predicate)` is the indexed variant of `findIndex`.
*/
@send external findIndexWithIndex: (t<'a>, ('a, int) => bool) => int = "findIndex"

/**
`findLastIndex(typedArray, predicate)` returns the index of the last matching element, or `-1` if none do.

See [`TypedArray.prototype.findLastIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/findLastIndex) on MDN.
*/
@send external findLastIndex: (t<'a>, 'a => bool) => int = "findLastIndex"
/**
`findLastIndexWithIndex(typedArray, predicate)` is the indexed variant of `findLastIndex`.
*/
@send external findLastIndexWithIndex: (t<'a>, ('a, int) => bool) => int = "findLastIndex"

/**
`forEach(typedArray, f)` runs `f` for every element in order.

See [`TypedArray.prototype.forEach`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/forEach) on MDN.
*/
@send external forEach: (t<'a>, 'a => unit) => unit = "forEach"
/**
`forEachWithIndex(typedArray, f)` runs `f` for every element, also providing the index.
*/
@send external forEachWithIndex: (t<'a>, ('a, int) => unit) => unit = "forEach"

/**
`map(typedArray, f)` returns a new typed array whose elements are produced by applying `f` to each element.

See [`TypedArray.prototype.map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/map) on MDN.
*/
@send external map: (t<'a>, 'a => 'b) => t<'b> = "map"
/**
`mapWithIndex(typedArray, f)` behaves like `map`, but `f` also receives the index.
*/
@send external mapWithIndex: (t<'a>, ('a, int) => 'b) => t<'b> = "map"

/**
`reduce(typedArray, reducer, initial)` combines the elements from left to right using `reducer`.

See [`TypedArray.prototype.reduce`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/reduce) on MDN.
*/
@send external reduce: (t<'a>, ('b, 'a) => 'b, 'b) => 'b = "reduce"
/**
`reduceWithIndex(typedArray, reducer, initial)` is the indexed variant of `reduce`.
*/
@send external reduceWithIndex: (t<'a>, ('b, 'a, int) => 'b, 'b) => 'b = "reduce"

/**
`reduceRight(typedArray, reducer, initial)` is like `reduce` but processes the elements from right to left.

See [`TypedArray.prototype.reduceRight`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/reduceRight) on MDN.
*/
@send external reduceRight: (t<'a>, ('b, 'a) => 'b, 'b) => 'b = "reduceRight"
/**
`reduceRightWithIndex(typedArray, reducer, initial)` is the indexed variant of `reduceRight`.
*/
@send external reduceRightWithIndex: (t<'a>, ('b, 'a, int) => 'b, 'b) => 'b = "reduceRight"

/**
`some(typedArray, predicate)` returns `true` if `predicate` returns `true` for at least one element.

See [`TypedArray.prototype.some`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray/some) on MDN.
*/
@send external some: (t<'a>, 'a => bool) => bool = "some"
/**
`someWithIndex(typedArray, predicate)` behaves like `some`, but `predicate` also receives the element index.
*/
@send external someWithIndex: (t<'a>, ('a, int) => bool) => bool = "some"

/**
  `ignore(typedArray)` ignores the provided typedArray and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t<'a> => unit = "%ignore"

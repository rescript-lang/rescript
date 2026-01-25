/***
A mutable array.

Compiles to a regular JavaScript array.*/

/**
Type representing an array of value `'a`.
*/
type t<'a> = array<'a>

type arrayLike<'a>

/**
`fromIterator(iterator)` creates an array from the provided `iterator`

## Examples

```rescript
Map.fromArray([("foo", 1), ("bar", 2)])
->Map.values
->Array.fromIterator == [1, 2]
```
*/
@val
external fromIterator: Stdlib_Iterator.t<'a> => array<'a> = "Array.from"

/**
`fromArrayLike(source)` converts an array-like value (anything with indexed items and a `length`) into a regular array.

See [`Array.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from) on MDN.

## Examples

```rescript
let source: Array.arrayLike<int> = %raw(`{0: 10, 1: 20, length: 2}`)
Array.fromArrayLike(source) == [10, 20]
```
*/
@val external fromArrayLike: arrayLike<'a> => array<'a> = "Array.from"

/**
`fromArrayLikeWithMap(source, map)` converts an array-like value into an array and applies `map` to every element.

See [`Array.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from) on MDN.

## Examples

```rescript
let source: Array.arrayLike<int> = %raw(`{0: 1, 1: 2, length: 2}`)
Array.fromArrayLikeWithMap(source, x => x * 10) == [10, 20]
```
*/
@val
external fromArrayLikeWithMap: (arrayLike<'a>, 'a => 'b) => array<'b> = "Array.from"

/**
`fromString(str)` creates an array of each character as a separate string from the provided `str`.

## Examples

```rescript
Array.fromString("abcde") == ["a", "b", "c", "d", "e"]
```
*/
@val external fromString: string => array<string> = "Array.from"

/**
`make(~length, init)` creates an array of length `length` initialized with the value of `init`.

## Examples

```rescript
Array.make(~length=3, #apple) == [#apple, #apple, #apple]
Array.make(~length=6, 7) == [7, 7, 7, 7, 7, 7]
```
*/
let make: (~length: int, 'a) => array<'a>

/**
`fromInitializer(~length, f)`

Creates an array of length `length` initialized with the value returned from `f ` for each index.

## Examples

```rescript
Array.fromInitializer(~length=3, i => i + 3) == [3, 4, 5]

Array.fromInitializer(~length=7, i => i + 3) == [3, 4, 5, 6, 7, 8, 9]
```
*/
let fromInitializer: (~length: int, int => 'a) => array<'a>

/**
`equal(left, right, predicate)` checks if the two arrays contain the same elements according to the equality `predicate`.

## Examples

```rescript
Array.equal([1, 2, 3], [1, 2, 3], Int.equal) == true
Array.equal([1, 2, 3], [1, 3, 2], Int.equal) == false
```
*/
let equal: (array<'a>, array<'a>, ('a, 'a) => bool) => bool

/**
`compare(left, right, comparator)` compares two arrays element by element using `comparator` and returns an `Ordering`.

## Examples

```rescript
Array.compare([1, 3], [1, 2], Int.compare) == Ordering.greater
Array.compare([1, 2], [1, 2], Int.compare) == Ordering.equal
```
*/
let compare: (array<'a>, array<'a>, ('a, 'a) => Stdlib_Ordering.t) => Stdlib_Ordering.t

/**
`isArray(value)` returns `true` when `value` is a JavaScript array and `false` otherwise.

See [`Array.isArray`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/isArray) on MDN.

## Examples

```rescript
Array.isArray([1, 2, 3]) == true
Array.isArray("not an array") == false
```
*/
@val external isArray: 'a => bool = "Array.isArray"

/**
`length(array)` returns the length of (i.e. number of items in) the array.

See [`Array.length`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/length) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]

someArray->Array.length == 2
```
*/
external length: array<'a> => int = "%array_length"

/**
`isEmpty(array)` returns `true` if the array is empty (has length 0), `false` otherwise.

## Examples

```rescript
[]->Array.isEmpty->assertEqual(true)
[1, 2, 3]->Array.isEmpty->assertEqual(false)

let emptyArray = []
emptyArray->Array.isEmpty->assertEqual(true)

let nonEmptyArray = ["hello"]
nonEmptyArray->Array.isEmpty->assertEqual(false)
```
*/
let isEmpty: array<'a> => bool

/**
`isEmpty(array)` returns `true` if the array is empty (has length 0), `false` otherwise.

## Examples

```rescript
let arr = [100, 101, 102, 103, 104]
arr->Array.copyAllWithin(~target=2) == [100, 101, 100, 101, 102]
arr == [100, 101, 100, 101, 102]
```
*/
@deprecated({
  reason: "Use `copyWithin` instead",
  migrate: Array.copyWithin(~start=0),
})
@send external copyAllWithin: (array<'a>, ~target: int) => array<'a> = "copyWithin"

/**
`copyWithinToEnd(array, ~target, ~start)` copies starting at element `start` in the given array to the designated `target` position, returning the resulting array.

Beware this will *mutate* the array.

See [`Array.copyWithin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/copyWithin) on MDN.

## Examples

```rescript
let arr = [100, 101, 102, 103, 104]
arr->Array.copyWithinToEnd(~target=0, ~start=2) == [102, 103, 104, 103, 104]
arr == [102, 103, 104, 103, 104]
```
*/
@deprecated({
  reason: "Use `copyWithin` instead",
  migrate: Array.copyWithin(),
})
@send
external copyWithinToEnd: (array<'a>, ~target: int, ~start: int) => array<'a> = "copyWithin"

/**
`copyWithin(array, ~target, ~start, ~end)` copies starting at element `start` in the given array up to but not including `end` to the designated `target` position, returning the resulting array.

Beware this will *mutate* the array.

See [`Array.copyWithin`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/copyWithin) on MDN.

## Examples

```rescript
let arr = [100, 101, 102, 103, 104, 105]
arr->Array.copyWithin(~target=1, ~start=2, ~end=5) == [100, 102, 103, 104, 104, 105]
arr == [100, 102, 103, 104, 104, 105]
```
*/
@send
external copyWithin: (array<'a>, ~target: int, ~start: int, ~end: int=?) => array<'a> = "copyWithin"

/**
`fillAll(array, value)` fills the entire `array` with `value`.

Beware this will *mutate* the array.

See [`Array.fill`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill) on MDN.

## Examples

```rescript
let myArray = [1, 2, 3, 4]
myArray->Array.fillAll(9)
myArray == [9, 9, 9, 9]
```
*/
@deprecated({
  reason: "Use `fill` instead",
  migrate: Array.fill(),
})
@send
external fillAll: (array<'a>, 'a) => unit = "fill"

/**
`fillToEnd(array, value, ~start)` fills `array` with `value` from the `start` index.

Beware this will *mutate* the array.

See [`Array.fill`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill) on MDN.

## Examples

```rescript
let myArray = [1, 2, 3, 4]
myArray->Array.fillToEnd(9, ~start=1)
myArray == [1, 9, 9, 9]
```
*/
@deprecated({
  reason: "Use `fill` instead",
  migrate: Array.fill(~start=%insert.labelledArgument("start")),
})
@send
external fillToEnd: (array<'a>, 'a, ~start: int) => unit = "fill"

/**
`fill(array, value, ~start, ~end)` fills `array` with `value` from `start` to `end`.

Beware this will *mutate* the array.

See [`Array.fill`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill) on MDN.

## Examples

```rescript
let myArray = [1, 2, 3, 4]

myArray->Array.fill(9)
myArray == [9, 9, 9, 9]

myArray->Array.fill(0, ~start=1)
myArray == [9, 0, 0, 0]

myArray->Array.fill(5, ~start=1, ~end=3)
myArray == [9, 5, 5, 0]
```
*/
@send
external fill: (array<'a>, 'a, ~start: int=?, ~end: int=?) => unit = "fill"

/**
`pop(array)` removes the last item from `array` and returns it.

Beware this will *mutate* the array.

See [`Array.pop`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/pop) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]

someArray->Array.pop == Some("hello")

someArray == ["hi"] // Notice last item is gone.
```
*/
@send
external pop: array<'a> => option<'a> = "pop"

/**
`push(array, item)` appends `item` to the end of `array`.

Beware this will *mutate* the array.

See [`Array.push`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]

someArray->Array.push("yay")

someArray == ["hi", "hello", "yay"]
```
*/
@send
external push: (array<'a>, 'a) => unit = "push"

/**
`pushMany(array, itemsArray)` appends many new items to the end of the array.

Beware this will *mutate* the array.

See [`Array.push`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/push) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]

someArray->Array.pushMany(["yay", "wehoo"])
someArray == ["hi", "hello", "yay", "wehoo"]
```
*/
@variadic @send
external pushMany: (array<'a>, array<'a>) => unit = "push"

/**
`reverse(array)` reverses the order of the items in `array`.

Beware this will *mutate* the array.

See [`Array.reverse`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reverse) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]
someArray->Array.reverse

someArray == ["hello", "hi"]
```
*/
@send
external reverse: array<'a> => unit = "reverse"

/**
`shift(array)` removes the first item in the array, and returns it.

Beware this will *mutate* the array.

See [`Array.shift`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/shift) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]

someArray->Array.shift == Some("hi")

someArray == ["hello"] // Notice first item is gone.
```
*/
@send
external shift: array<'a> => option<'a> = "shift"

/**
`toSorted(array, comparator)` returns a new, sorted array from `array`, using the `comparator` function.

See [`Array.toSorted`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toSorted) on MDN.

## Examples

```rescript
let someArray = [3, 2, 1]

someArray->Array.toSorted(Int.compare) == [1, 2, 3]

someArray == [3, 2, 1] // Original unchanged
```
*/
@send
external toSorted: (array<'a>, ('a, 'a) => Stdlib_Ordering.t) => array<'a> = "toSorted"

/**
`sort(array, comparator)` sorts `array` in-place using the `comparator` function.

Beware this will *mutate* the array.

See [`Array.sort`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort) on MDN.

## Examples

```rescript
let array = [3, 2, 1]
array->Array.sort((a, b) => float(a - b))
array == [1, 2, 3]
```
*/
@send
external sort: (array<'a>, ('a, 'a) => Stdlib_Ordering.t) => unit = "sort"

/**
`splice(array, ~start, ~remove, ~insert)` removes `remove` items starting at `start` and inserts the values from `insert`.

Beware this will *mutate* the array.

See [`Array.splice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/splice) on MDN.

## Examples

```rescript
let items = ["a", "b", "c"]
items->Array.splice(~start=1, ~remove=1, ~insert=["x"])
items == ["a", "x", "c"]
```
*/
@variadic @send
external splice: (array<'a>, ~start: int, ~remove: int, ~insert: array<'a>) => unit = "splice"

/**
`toSpliced(array, ~start, ~remove, ~insert)` returns a new array with the same edits that `splice` would perform, leaving the original unchanged.

See [`Array.toSpliced`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toSpliced) on MDN.

## Examples

```rescript
let original = [1, 2, 3]
let updated = original->Array.toSpliced(~start=1, ~remove=1, ~insert=[10])
updated == [1, 10, 3]
original == [1, 2, 3]
```
*/
@variadic @send
external toSpliced: (array<'a>, ~start: int, ~remove: int, ~insert: array<'a>) => array<'a> =
  "toSpliced"

/**
`removeInPlace(array, index)` removes the item at the specified `index` from `array`.

Beware this will *mutate* the array.

## Examples

```rescript
let array = []
array->Array.removeInPlace(0)
array == [] // Removing from an empty array does nothing

let array2 = ["Hello", "Hi", "Good bye"]
array2->Array.removeInPlace(1)
array2 == ["Hello", "Good bye"] // Removes the item at index 1
```
 */
@send
external removeInPlace: (array<'a>, int, @as(1) _) => unit = "splice"

/**
`with(array, index, value)` returns a copy of `array` where the element at `index` is replaced with `value`.

See [`Array.with`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/with) on MDN.

## Examples

```rescript
let original = ["a", "b", "c"]
let replaced = original->Array.with(1, "x")
replaced == ["a", "x", "c"]
original == ["a", "b", "c"]
```
*/
@send external with: (array<'a>, int, 'a) => array<'a> = "with"

/**
`unshift(array, item)` inserts a new item at the start of the array.

Beware this will *mutate* the array.

See [`Array.unshift`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/unshift) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]
someArray->Array.unshift("yay")
someArray == ["yay", "hi", "hello"]
```
*/
@send
external unshift: (array<'a>, 'a) => unit = "unshift"

/**
`unshiftMany(array, itemsArray)` inserts many new items to the start of the array.

Beware this will *mutate* the array.

See [`Array.push`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/unshift) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]
someArray->Array.unshiftMany(["yay", "wehoo"])
someArray == ["yay", "wehoo", "hi", "hello"]
```
*/
@variadic @send
external unshiftMany: (array<'a>, array<'a>) => unit = "unshift"

/**
`concat(array1, array2)` concatenates the two arrays, creating a new array.

See [`Array.concat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/concat) on MDN.

## Examples

```rescript
let array1 = ["hi", "hello"]
let array2 = ["yay", "wehoo"]

let someArray = array1->Array.concat(array2)

someArray == ["hi", "hello", "yay", "wehoo"]
```
*/
@send
external concat: (array<'a>, array<'a>) => array<'a> = "concat"

/**
`concatMany(array1, arrays)` concatenates array1 with several other arrays, creating a new array.

See [`Array.concat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/concat) on MDN.

## Examples
```rescript
let array1 = ["hi", "hello"]
let array2 = ["yay"]
let array3 = ["wehoo"]

let someArray = array1->Array.concatMany([array2, array3])

Console.log(someArray) // ["hi", "hello", "yay", "wehoo"]
```
*/
@variadic @send
external concatMany: (array<'a>, array<array<'a>>) => array<'a> = "concat"

/**
`flat(arrays)` concatenates an array of arrays into a single array.

See [`Array.flat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/flat) on MDN.

## Examples

```rescript
[[1], [2], [3, 4]]->Array.flat == [1, 2, 3, 4]
```
*/
@send
external flat: array<array<'a>> => array<'a> = "flat"

/**
`includes(array, item)` checks whether `array` includes `item`, by doing a [strict check for equality](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality).

See [`Array.includes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes) on MDN.

## Examples

```rescript
[1, 2]->Array.includes(1) == true
[1, 2]->Array.includes(3) == false

[{"language": "ReScript"}]->Array.includes({"language": "ReScript"}) == false // false, because of strict equality
```
*/
@send
external includes: (array<'a>, 'a) => bool = "includes"

/**
`indexOf(array, item, ~from)` returns the index of the provided `item` in `array`, starting the search at `from`. Uses [strict check for equality](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality) when comparing items.

Returns `-1` if the item isn't found. Check out `Array.indexOfOpt` for a version that returns `None` instead of `-1` if the item does not exist.

See [`Array.indexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/indexOf) on MDN.

## Examples

```rescript
[1, 2]->Array.indexOf(2) == 1
[1, 2]->Array.indexOf(3) == -1
[1, 2, 1, 2]->Array.indexOf(2, ~from=2) == 3

[{"language": "ReScript"}]->Array.indexOf({"language": "ReScript"}) == -1 // -1, because of strict equality
```
*/
@send
external indexOf: (array<'a>, 'a, ~from: int=?) => int = "indexOf"

/**
`indexOfOpt(array, item)` returns an option of the index of the provided `item` in `array`. Uses [strict check for equality](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality) when comparing items.

See [`Array.indexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/indexOf) on MDN.

## Examples

```rescript
[1, 2]->Array.indexOfOpt(2) == Some(1)
[1, 2]->Array.indexOfOpt(3) == None
[{"language": "ReScript"}]->Array.indexOfOpt({"language": "ReScript"}) == None // None, because of strict equality
```
*/
let indexOfOpt: (array<'a>, 'a) => option<int>
@deprecated({
  reason: "Use `indexOf` instead",
  migrate: Array.indexOf(~from=%insert.unlabelledArgument(2)),
})
@send
external indexOfFrom: (array<'a>, 'a, int) => int = "indexOf"

/**
`join(array, separator)` produces a string where all items of `array` are printed, separated by `separator`. Array items must be strings, to join number or other arrays, use `joinUnsafe`. Under the hood this will run JavaScript's `toString` on all the array items.

See [Array.join](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)

## Examples

```rescript
["One", "Two", "Three"]->Array.join(" -- ") == "One -- Two -- Three"
```
*/
@send
external join: (array<string>, string) => string = "join"

/**
`joinWith(array, separator)` produces a string where all items of `array` are printed, separated by `separator`. Array items must be strings, to join number or other arrays, use `joinWithUnsafe`. Under the hood this will run JavaScript's `toString` on all the array items.

## Examples

```rescript
["One", "Two", "Three"]->Array.joinWith(" -- ") == "One -- Two -- Three"
```
*/
@deprecated({
  reason: "Use `join` instead",
  migrate: Array.join(),
})
@send
external joinWith: (array<string>, string) => string = "join"

/**
`joinUnsafe(array, separator)` produces a string where all items of `array` are printed, separated by `separator`. Under the hood this will run JavaScript's `toString` on all the array items.

See [Array.join](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)

## Examples

```rescript
[1, 2, 3]->Array.joinUnsafe(" -- ") == "1 -- 2 -- 3"
```
*/
@send
external joinUnsafe: (array<'a>, string) => string = "join"

/**
`joinWithUnsafe(array, separator)` produces a string where all items of `array` are printed, separated by `separator`. Under the hood this will run JavaScript's `toString` on all the array items.

## Examples

```rescript
[1, 2, 3]->Array.joinWithUnsafe(" -- ") == "1 -- 2 -- 3"
```
*/
@deprecated({
  reason: "Use `joinUnsafe` instead",
  migrate: Array.joinUnsafe(),
})
@send
external joinWithUnsafe: (array<'a>, string) => string = "join"
/**
`lastIndexOf(array, item, ~from)` returns the last index of the provided `item` in `array`, searching backwards from `from`. Uses [strict check for equality](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality) when comparing items.

Returns `-1` if the item isn't found. Check out `Array.lastIndexOfOpt` for a version that returns `None` instead of `-1` if the item does not exist.

See [`Array.lastIndexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/lastIndexOf) on MDN.

## Examples

```rescript
[1, 2, 1, 2]->Array.lastIndexOf(2) == 3
[1, 2]->Array.lastIndexOf(3) == -1
[1, 2, 1, 2]->Array.lastIndexOf(2, ~from=2) == 1

[{"language": "ReScript"}]->Array.lastIndexOf({"language": "ReScript"}) == -1 // -1, because of strict equality
```
*/
@send external lastIndexOf: (array<'a>, 'a, ~from: int=?) => int = "lastIndexOf"
let lastIndexOfOpt: (array<'a>, 'a) => option<int>
@deprecated({
  reason: "Use `lastIndexOf` instead",
  migrate: Array.lastIndexOf(~from=%insert.unlabelledArgument(2)),
})
@send
external lastIndexOfFrom: (array<'a>, 'a, int) => int = "lastIndexOf"

/**
`slice(array, ~start, ~end)` creates a new array of items copied from `array` from `start` until (but not including) `end`.

See [`Array.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice) on MDN.

## Examples

```rescript
[1, 2, 3, 4]->Array.slice(~start=1, ~end=3) == [2, 3]
[1, 2, 3, 4]->Array.slice(~start=1) == [2, 3, 4]
[1, 2, 3, 4]->Array.slice == [1, 2, 3, 4]
```
*/
@send
external slice: (array<'a>, ~start: int=?, ~end: int=?) => array<'a> = "slice"

/**
`sliceToEnd(array, start)` creates a new array from `array`, with all items from `array` starting from `start`.

See [`Array.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice) on MDN.

## Examples

```rescript
[1, 2, 3, 4]->Array.sliceToEnd(~start=1) == [2, 3, 4]
```
*/
@deprecated({
  reason: "Use `slice` instead",
  migrate: Array.slice(),
})
@send
external sliceToEnd: (array<'a>, ~start: int) => array<'a> = "slice"
/**
`copy(array)` makes a copy of the array with the items in it, but does not make copies of the items themselves.

## Examples

```rescript
let myArray = [1, 2, 3]
let copyOfMyArray = myArray->Array.copy

copyOfMyArray == [1, 2, 3]
(myArray === copyOfMyArray) == false
```
*/
@send
external copy: array<'a> => array<'a> = "slice"

/**
`toString(array)` stringifies `array` by running `toString` on all of the array elements and joining them with ",".

See [`Array.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toString) on MDN.

## Examples

```rescript
[1, 2, 3, 4]->Array.toString == "1,2,3,4"
```
*/
@send
external toString: array<'a> => string = "toString"

/**
`toLocaleString(array)` converts each element to a locale-aware string and joins them with commas.

See [`Array.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toLocaleString) on MDN.

## Examples

```rescript
["apple", "banana"]->Array.toLocaleString == "apple,banana"
```
*/
@send external toLocaleString: array<'a> => string = "toLocaleString"

/**
`every(array, predicate)` returns true if `predicate` returns true for all items in `array`.

See [`Array.every`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every) on MDN.

## Examples

```rescript
let array = [1, 2, 3, 4]

array->Array.every(num => num <= 4) == true

array->Array.every(num => num === 1) == false
```
*/
@send
external every: (array<'a>, 'a => bool) => bool = "every"

/**
`everyWithIndex(array, checker)` returns true if all items in `array` returns true when running the provided `checker` function.

See [`Array.every`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every) on MDN.

## Examples

```rescript
let array = [1, 2, 3, 4]

array->Array.everyWithIndex((num, index) => index < 5 && num <= 4) == true

array->Array.everyWithIndex((num, index) => index < 2 && num >= 2) == false
```
*/
@send
external everyWithIndex: (array<'a>, ('a, int) => bool) => bool = "every"

/**
`filter(array, checker)` returns a new array containing all elements from `array` for which the provided `checker` function returns true.

See [`Array.filter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter) on MDN.

## Examples

```rescript
[1, 2, 3, 4]->Array.filter(num => num > 2) == [3, 4]
```
*/
@send
external filter: (array<'a>, 'a => bool) => array<'a> = "filter"

/**
`filterWithIndex(array, checker)` returns a new array containing all elements from `array` for which the provided `checker` function returns true.

See [`Array.filter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter) on MDN.

## Examples

```rescript
[1, 2, 3, 4]->Array.filterWithIndex((num, index) => index === 0 || num === 2) == [1, 2]
```
*/
@send
external filterWithIndex: (array<'a>, ('a, int) => bool) => array<'a> = "filter"

/**
`find(array, checker)` returns the first element of `array` where the provided `checker` function returns true.

See [`Array.find`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/find) on MDN.

## Examples

```rescript
type languages = ReScript | TypeScript | JavaScript

let array = [ReScript, TypeScript, JavaScript]

array->Array.find(item => item == ReScript) == Some(ReScript)
```
*/
@send
external find: (array<'a>, 'a => bool) => option<'a> = "find"

/**
`findWithIndex(array, checker)` returns the first element of `array` where the provided `checker` function returns true.

See [`Array.find`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/find) on MDN.

## Examples

```rescript
type languages = ReScript | TypeScript | JavaScript

let array = [TypeScript, JavaScript, ReScript]

array->Array.findWithIndex((item, index) => index > 1 && item == ReScript) == Some(ReScript)
```
*/
@send
external findWithIndex: (array<'a>, ('a, int) => bool) => option<'a> = "find"

/**
`findLast(array, checker)` returns the last element of `array` where the provided `checker` function returns true.

See [`Array.findLast`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findLast) on MDN.

## Examples

```rescript
let array = [1, 2, 3]

array->Array.findLast(item => item > 0) == Some(3)
```
*/
@send
external findLast: (array<'a>, 'a => bool) => option<'a> = "findLast"

/**
`findLastWithIndex(array, checker)` returns the last element of `array` where the provided `checker` function returns true.

See [`Array.findLast`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findLast) on MDN.

## Examples

```rescript
let array = [1, 2, 3]

array->Array.findLastWithIndex((item, index) => index < 2 && item > 0) == Some(2)
```
*/
@send
external findLastWithIndex: (array<'a>, ('a, int) => bool) => option<'a> = "findLast"

/**
`findIndex(array, checker)` returns the index of the first element of `array` where the provided `checker` function returns true.

Returns `-1` if the item does not exist. Consider using `Array.findIndexOpt` if you want an option instead (where `-1` would be `None`).

See [`Array.findIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex) on MDN.

## Examples

```rescript
type languages = ReScript | TypeScript | JavaScript

let array = [ReScript, JavaScript]

array->Array.findIndex(item => item == ReScript) == 0

array->Array.findIndex(item => item == TypeScript) == -1
```
*/
@send
external findIndex: (array<'a>, 'a => bool) => int = "findIndex"

/**
`findIndexWithIndex(array, checker)` returns the index of the first element of `array` where the provided `checker` function returns true.

Returns `-1` if the item does not exist. Consider using `Array.findIndexOpt` if you want an option instead (where `-1` would be `None`).

See [`Array.findIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex) on MDN.

## Examples

```rescript
type languages = ReScript | TypeScript | JavaScript

let array = [ReScript, JavaScript]

let isReScriptFirst =
  array->Array.findIndexWithIndex((item, index) => index === 0 && item == ReScript)
let isTypeScriptFirst =
  array->Array.findIndexWithIndex((item, index) => index === 0 && item == TypeScript)

isReScriptFirst == 0
isTypeScriptFirst == -1
```
*/
@send
external findIndexWithIndex: (array<'a>, ('a, int) => bool) => int = "findIndex"

/**
`findLastIndex(array, checker)` returns the index of the last element of `array` where the provided `checker` function returns true.

Returns `-1` if the item does not exist. Consider using `Array.findLastIndexOpt` if you want an option instead (where `-1` would be `None`).

See [`Array.findLastIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findLastIndex) on MDN.

## Examples

```rescript
type languages = ReScript | TypeScript | JavaScript

let array = [ReScript, JavaScript, ReScript]

array->Array.findLastIndex(item => item == ReScript) == 2

array->Array.findLastIndex(item => item == TypeScript) == -1
```
*/
@send
external findLastIndex: (array<'a>, 'a => bool) => int = "findLastIndex"

/**
`findLastIndexWithIndex(array, checker)` returns the index of the last element of `array` where the provided `checker` function returns true.

Returns `-1` if the item does not exist. Consider using `Array.findLastIndexOpt` if you want an option instead (where `-1` would be `None`).

See [`Array.findLastIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findLastIndex) on MDN.

## Examples

```rescript
type languages = ReScript | TypeScript | JavaScript

let array = [ReScript, JavaScript, JavaScript, ReScript]

let isReScriptLast =
  array->Array.findLastIndexWithIndex((item, index) => index === 3 && item == ReScript)
let isTypeScriptLast =
  array->Array.findLastIndexWithIndex((item, index) => index === 3 && item == TypeScript)

isReScriptLast == 3
isTypeScriptLast == -1
```
*/
@send
external findLastIndexWithIndex: (array<'a>, ('a, int) => bool) => int = "findLastIndex"

/**
`forEach(array, fn)` runs the provided `fn` on every element of `array`.

See [`Array.forEach`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach) on MDN.

## Examples
```rescript
let array = ["Hello", "Hi", "Good bye"]

array->Array.forEach(item => {
  Console.log(item)
})
```
*/
@send
external forEach: (array<'a>, 'a => unit) => unit = "forEach"

/**
`forEachWithIndex(array, fn)` runs the provided `fn` on every element of `array`.

See [`Array.forEach`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/forEach) on MDN.

## Examples

```rescript
let array = ["Hello", "Hi", "Good bye"]

array->Array.forEachWithIndex((item, index) => {
  Console.log("At item " ++ Int.toString(index) ++ ": " ++ item)
})
```
*/
@send
external forEachWithIndex: (array<'a>, ('a, int) => unit) => unit = "forEach"

/**
`map(array, fn)` returns a new array with all elements from `array`, each element transformed using the provided `fn`.

See [`Array.map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map) on MDN.

## Examples

```rescript
let array = ["Hello", "Hi", "Good bye"]
let mappedArray = array->Array.map(greeting => greeting ++ " to you")

mappedArray == ["Hello to you", "Hi to you", "Good bye to you"]
```
*/
@send
external map: (array<'a>, 'a => 'b) => array<'b> = "map"

/**
`mapWithIndex(array, fn)` returns a new array with all elements from `array`, each element transformed using the provided `fn`.

See [`Array.map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map) on MDN.

## Examples

```rescript
let array = ["Hello", "Hi", "Good bye"]
let mappedArray =
  array->Array.mapWithIndex((greeting, index) => greeting ++ " at position " ++ Int.toString(index))

mappedArray == ["Hello at position 0", "Hi at position 1", "Good bye at position 2"]
```
*/
@send
external mapWithIndex: (array<'a>, ('a, int) => 'b) => array<'b> = "map"

/**
`reduce(xs, init, fn)`

Applies `fn` to each element of `xs` from beginning to end. Function `fn` has two parameters: the item from the list and an "accumulator"; which starts with a value of `init`. `reduce` returns the final value of the accumulator.

## Examples

```rescript
Array.reduce([2, 3, 4], 1, (a, b) => a + b) == 10

Array.reduce(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "abcd"

[1, 2, 3]->Array.reduce(list{}, List.add) == list{3, 2, 1}

Array.reduce([], list{}, List.add) == list{}
```
*/
let reduce: (array<'a>, 'b, ('b, 'a) => 'b) => 'b

/**
`reduceWithIndex(x, init, fn)`

Applies `fn` to each element of `xs` from beginning to end. Function `fn` has three parameters: the item from the array and an "accumulator", which starts with a value of `init` and the index of each element. `reduceWithIndex` returns the final value of the accumulator.

## Examples

```rescript
Array.reduceWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i) == 16

Array.reduceWithIndex([1, 2, 3], list{}, (acc, v, i) => list{v + i, ...acc}) == list{5, 3, 1}

Array.reduceWithIndex([], list{}, (acc, v, i) => list{v + i, ...acc}) == list{}
```
*/
let reduceWithIndex: (array<'a>, 'b, ('b, 'a, int) => 'b) => 'b

/**
`reduceRight(xs, init, fn)`

Works like `Array.reduce`; except that function `fn` is applied to each item of `xs` from the last back to the first.

## Examples

```rescript
Array.reduceRight(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "dcba"

Array.reduceRight([1, 2, 3], list{}, List.add) == list{1, 2, 3}

Array.reduceRight([], list{}, List.add) == list{}
```
*/
let reduceRight: (array<'a>, 'b, ('b, 'a) => 'b) => 'b

/**
`reduceRightWithIndex(xs, init, fn)`

Like `reduceRight`, but with an additional index argument on the callback function.

## Examples

```rescript
Array.reduceRightWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i) == 16

Array.reduceRightWithIndex([], list{}, (acc, v, i) => list{v + i, ...acc}) == list{}
```
*/
let reduceRightWithIndex: (array<'a>, 'b, ('b, 'a, int) => 'b) => 'b

/**
`some(array, predicate)` returns true if `predicate` returns true for any element in `array`.

See [`Array.some`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/some) on MDN.

## Examples

```rescript
let array = ["Hello", "Hi", "Good bye"]

array->Array.some(greeting => greeting === "Hello") == true
```
*/
@send
external some: (array<'a>, 'a => bool) => bool = "some"

/**
`someWithIndex(array, checker)` returns true if running the provided `checker` function on any element in `array` returns true.

See [`Array.some`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/some) on MDN.

## Examples

```rescript
let array = ["Hello", "Hi", "Good bye"]

array->Array.someWithIndex((greeting, index) => greeting === "Hello" && index === 0) == true
```
*/
@send
external someWithIndex: (array<'a>, ('a, int) => bool) => bool = "some"

/**
`get(array, index)` returns the element at `index` of `array`.

Returns `None` if the index does not exist in the array. Equivalent to doing `array[index]` in JavaScript.

## Examples

```rescript
let array = ["Hello", "Hi", "Good bye"]

array->Array.get(0) == Some("Hello")

array->Array.get(3) == None
```
*/
@get_index
external get: (array<'a>, int) => option<'a> = ""

/**
`set(array, index, item)` sets the provided `item` at `index` of `array`.

Beware this will *mutate* the array.

## Examples

```rescript
let array = ["Hello", "Hi", "Good bye"]
array->Array.set(1, "Hello")

array[1] == Some("Hello")
```
*/
@set_index
external set: (array<'a>, int, 'a) => unit = ""
/**
`getSymbol(array, key)` retrieves the value stored under the symbol `key`, if present.

See [`Symbol`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol) on MDN for more details about symbol keys.

## Examples

```rescript
let key = Symbol.make("meta")
let items = []
items->Array.setSymbol(key, "hello")
Array.getSymbol(items, key) == Some("hello")
```
*/
@get_index external getSymbol: (array<'a>, Stdlib_Symbol.t) => option<'b> = ""

/**
`getSymbolUnsafe(array, key)` retrieves the value stored under the symbol `key` without any safety checks.

See [`Symbol`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol) on MDN.

## Examples

```rescript
let key = Symbol.make("meta")
let items = []
items->Array.setSymbol(key, "hello")
Array.getSymbolUnsafe(items, key) == "hello"
```
*/
@get_index external getSymbolUnsafe: (array<'a>, Stdlib_Symbol.t) => 'b = ""

/**
`setSymbol(array, key, value)` stores `value` under the symbol `key` on `array`.

Beware this will *mutate* the array.

See [`Symbol`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol) on MDN.

## Examples

```rescript
let key = Symbol.make("count")
let items = []
items->Array.setSymbol(key, 5)
Array.getSymbol(items, key) == Some(5)
```
*/
@set_index external setSymbol: (array<'a>, Stdlib_Symbol.t, 'b) => unit = ""

/**
`getUnsafe(array, index)` returns the element at `index` of `array`.

This is _unsafe_, meaning it will return `undefined` value if `index` does not exist in `array`.

Use `Array.getUnsafe` only when you are sure the `index` exists (i.e. when using for-loop).

## Examples
```rescript
let array = [1, 2, 3]
for index in 0 to array->Array.length - 1 {
  let value = array->Array.getUnsafe(index)
  Console.log(value)
}
```
*/
external getUnsafe: (array<'a>, int) => 'a = "%array_unsafe_get"

/**
`unsafe_get(array, index)` returns the element at `index` of `array`.

This is _unsafe_, meaning it will return `undefined` value if `index` does not exist in `array`.

Use `Array.unsafe_get` only when you are sure the `index` exists (i.e. when using for-loop).

## Examples

```rescript
let array = [1, 2, 3]
for index in 0 to array->Array.length - 1 {
  let value = array->Array.unsafe_get(index)
  Console.log(value)
}
```
*/
@deprecated({
  reason: "Use getUnsafe instead. This will be removed in v13",
  migrate: Array.getUnsafe(),
})
external unsafe_get: (array<'a>, int) => 'a = "%array_unsafe_get"

/**
`setUnsafe(array, index, item)` sets the provided `item` at `index` of `array`.

Beware this will *mutate* the array, and is *unsafe*.

## Examples

```rescript
let array = ["Hello", "Hi", "Good bye"]
array->Array.setUnsafe(1, "Hello")

array[1] == Some("Hello")
```
*/
external setUnsafe: (array<'a>, int, 'a) => unit = "%array_unsafe_set"

/**
`findIndexOpt(array, checker)` returns the index of the first element of `array` where the provided `checker` function returns true.

Returns `None` if no item matches.

See [`Array.findIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex) on MDN.

## Examples

```rescript
type languages = ReScript | TypeScript | JavaScript

let array = [ReScript, TypeScript, JavaScript]

array->Array.findIndexOpt(item => item == ReScript) == Some(0)
```
*/
let findIndexOpt: (array<'a>, 'a => bool) => option<int>

/**
`findIndexOpt(array, checker)` returns the index of the last element of `array` where the provided `checker` function returns true.

Returns `None` if no item matches.

See [`Array.findLastIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findLastIndex) on MDN.

## Examples

```rescript
let array = ["hello", "world", "!"]

array->Array.findLastIndexOpt(item => item->String.includes("o")) == Some(1)
```
*/
let findLastIndexOpt: (array<'a>, 'a => bool) => option<int>

/**
`toReversed(array)` creates a new array with all items from `array` in reversed order.

See [`Array.toReversed`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toReversed) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]
let reversed = someArray->Array.toReversed

reversed == ["hello", "hi"]
someArray == ["hi", "hello"] // Original unchanged
```
*/
@send
external toReversed: array<'a> => array<'a> = "toReversed"

/**
`filterMap(array, fn)`

Calls `fn` for each element and returns a new array containing results of the `fn` calls which are not `None`.

## Examples

```rescript
["Hello", "Hi", "Good bye"]->Array.filterMap(item =>
  switch item {
  | "Hello" => Some(item->String.length)
  | _ => None
  }
) == [5]

[1, 2, 3, 4, 5, 6]->Array.filterMap(n => mod(n, 2) == 0 ? Some(n * n) : None) == [4, 16, 36]

Array.filterMap([1, 2, 3, 4, 5, 6], _ => None) == []

Array.filterMap([], n => mod(n, 2) == 0 ? Some(n * n) : None) == []
```
*/
let filterMap: (array<'a>, 'a => option<'b>) => array<'b>

/**
`filterMapWithIndex(array, fn)`

Calls `fn` for each element and returns a new array containing results of the `fn` calls which are not `None`.

## Examples

```rescript
["Hello", "Hi", "Good bye"]->Array.filterMapWithIndex((item, index) =>
  switch item {
  | "Hello" => Some(index)
  | _ => None
  }
) == [0]
```
*/
let filterMapWithIndex: (array<'a>, ('a, int) => option<'b>) => array<'b>

/**
`keepSome(arr)`

Returns a new array containing `value` for all elements that are `Some(value)`
and ignoring every value that is `None`

## Examples

```rescript
Array.keepSome([Some(1), None, Some(3)]) == [1, 3]

Array.keepSome([Some(1), Some(2), Some(3)]) == [1, 2, 3]

Array.keepSome([None, None, None]) == []

Array.keepSome([]) == []
```
*/
let keepSome: array<option<'a>> => array<'a>

/**
`toShuffled(array)` returns a new array with all items in `array` in a random order.

## Examples

```rescript
let array = ["Hello", "Hi", "Good bye"]
let shuffledArray = array->Array.toShuffled
Console.log(shuffledArray)

Array.toShuffled([1, 2, 3])->Array.length == 3
```
*/
let toShuffled: array<'a> => array<'a>

/**
`shuffle(array)` randomizes the position of all items in `array`.

Beware this will *mutate* the array.

## Examples

```rescript
let array = ["Hello", "Hi", "Good bye"]
array->Array.shuffle
Console.log(array)

let array2 = [1, 2, 3]
array2->Array.shuffle

array2->Array.length == 3
```
*/
let shuffle: array<'a> => unit

/**
`flatMap(array, mapper)` returns a new array concatenating the arrays returned from running `mapper` on all items in `array`.

## Examples

```rescript
type language = ReScript | TypeScript | JavaScript

let array = [ReScript, TypeScript, JavaScript]

array->Array.flatMap(item =>
  switch item {
  | ReScript => [1, 2, 3]
  | TypeScript => [4, 5, 6]
  | JavaScript => [7, 8, 9]
  }
) == [1, 2, 3, 4, 5, 6, 7, 8, 9]
```
*/
@send
external flatMap: (array<'a>, 'a => array<'b>) => array<'b> = "flatMap"

/**
`flatMapWithIndex(array, mapper)` returns a new array concatenating the arrays returned from running `mapper` on all items in `array`.

## Examples

```rescript
type language = ReScript | TypeScript | JavaScript

let array = [ReScript, TypeScript, JavaScript]

array->Array.flatMapWithIndex((item, index) =>
  switch item {
  | ReScript => [index]
  | TypeScript => [index, index + 1]
  | JavaScript => [index, index + 1, index + 2]
  }
) == [0, 1, 2, 2, 3, 4]
```
*/
@send
external flatMapWithIndex: (array<'a>, ('a, int) => array<'b>) => array<'b> = "flatMap"

/**
`findMap(arr, fn)`

Calls `fn` for each element and returns the first value from `fn` that is `Some(_)`.
Otherwise returns `None`

## Examples

```rescript
Array.findMap([1, 2, 3], n => mod(n, 2) == 0 ? Some(n - 2) : None) == Some(0)

Array.findMap([1, 2, 3, 4, 5, 6], n => mod(n, 2) == 0 ? Some(n - 8) : None) == Some(-6)

Array.findMap([1, 2, 3, 4, 5, 6], _ => None) == None

Array.findMap([], n => mod(n, 2) == 0 ? Some(n * n) : None) == None
```
*/
let findMap: (array<'a>, 'a => option<'b>) => option<'b>

/**
`at(array, index)`

Get an element by its index. Negative indices count backwards from the last item.

## Examples

```rescript
["a", "b", "c"]->Array.at(0) == Some("a")
["a", "b", "c"]->Array.at(2) == Some("c")
["a", "b", "c"]->Array.at(3) == None
["a", "b", "c"]->Array.at(-1) == Some("c")
["a", "b", "c"]->Array.at(-3) == Some("a")
["a", "b", "c"]->Array.at(-4) == None
```
*/
@send
external at: (array<'a>, int) => option<'a> = "at"

/**
`last(array)` returns the last element of `array`.

Returns `None` if the array is empty.

## Examples

```rescript
["Hello", "Hi", "Good bye"]->Array.last == Some("Good bye")

[]->Array.last == None
```
*/
let last: array<'a> => option<'a>

/**
  `ignore(array)` ignores the provided array and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: array<'a> => unit = "%ignore"

/**
`entries(array)` returns a new array iterator object that contains the key/value pairs for each index in the array.

See [Array.prototype.entries](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/entries) on MDN.

## Examples

```rescript
let array = [5, 6, 7]
let iterator: Iterator.t<(int, int)> = array->Array.entries
iterator->Iterator.next == {done: false, value: Some((0, 5))}
iterator->Iterator.next == {done: false, value: Some((1, 6))}
```
*/
@send
external entries: array<'a> => Stdlib_Iterator.t<(int, 'a)> = "entries"

/**
`values(array)` returns a new array iterator object that contains the values for each index in the array.

See [Array.prototype.values](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/values) on MDN.

## Examples

```rescript
let array = [5, 6, 7]
let iterator: Iterator.t<int> = array->Array.values
iterator->Iterator.next == {done: false, value: Some(5)}
iterator->Iterator.next == {done: false, value: Some(6)}
```
 */
@send
external values: array<'a> => Stdlib_Iterator.t<'a> = "values"

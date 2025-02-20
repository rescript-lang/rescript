/***
A mutable array.

Compiles to a regular JavaScript array.*/

/**
Type representing an array of value `'a`.
*/
type t<'a> = array<'a>

type arrayLike<'a>

/**
`fromIterator(iterator)`

Creates an array from the provided `iterator`

## Examples

```rescript
Map.fromArray([("foo", 1), ("bar", 2)])
->Map.values
->Array.fromIterator
->assertEqual([1, 2])
```
*/
@val
external fromIterator: Stdlib_Iterator.t<'a> => array<'a> = "Array.from"

// TODO: Docs
@val external fromArrayLike: arrayLike<'a> => array<'a> = "Array.from"

// TODO: Docs
@val
external fromArrayLikeWithMap: (arrayLike<'a>, 'a => 'b) => array<'b> = "Array.from"

/**
`make(~length, init)`

Creates an array of length `length` initialized with the value of `init`.

## Examples

```rescript
Array.make(~length=3, #apple)->assertEqual([#apple, #apple, #apple])
Array.make(~length=6, 7)->assertEqual([7, 7, 7, 7, 7, 7])
```
*/
let make: (~length: int, 'a) => array<'a>

/**
`fromInitializer(~length, f)`

Creates an array of length `length` initialized with the value returned from `f ` for each index.

## Examples

```rescript
Array.fromInitializer(~length=3, i => i + 3)->assertEqual([3, 4, 5])

Array.fromInitializer(~length=7, i => i + 3)->assertEqual([3, 4, 5, 6, 7, 8, 9])
```
*/
let fromInitializer: (~length: int, int => 'a) => array<'a>

let equal: (array<'a>, array<'a>, ('a, 'a) => bool) => bool

let compare: (array<'a>, array<'a>, ('a, 'a) => Stdlib_Ordering.t) => Stdlib_Ordering.t

@val external isArray: 'a => bool = "Array.isArray"

/**
`length(array)` returns the length of (i.e. number of items in) the array.

See [`Array.length`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/length) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]

someArray
->Array.length
->assertEqual(2)
```
*/
@get
external length: array<'a> => int = "length"

// TODO: Docs
@send external copyAllWithin: (array<'a>, ~target: int) => array<'a> = "copyWithin"

// TODO: Docs
@send
external copyWithinToEnd: (array<'a>, ~target: int, ~start: int) => array<'a> = "copyWithin"

// TODO: Docs
@send
external copyWithin: (array<'a>, ~target: int, ~start: int, ~end: int) => array<'a> = "copyWithin"

/**
`fillAll(array, value)` fills the entire `array` with `value`.

Beware this will *mutate* the array.

See [`Array.fill`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill) on MDN.

## Examples

```rescript
let myArray = [1, 2, 3, 4]
myArray->Array.fillAll(9)
myArray->assertEqual([9, 9, 9, 9])
```
*/
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
myArray->assertEqual([1, 9, 9, 9])
```
*/
@send
external fillToEnd: (array<'a>, 'a, ~start: int) => unit = "fill"

/**
`fill(array, value, ~start, ~end)` fills `array` with `value` from `start` to `end`.

Beware this will *mutate* the array.

See [`Array.fill`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/fill) on MDN.

## Examples

```rescript
let myArray = [1, 2, 3, 4]

myArray->Array.fill(9, ~start=1, ~end=3)

myArray->assertEqual([1, 9, 9, 4])
```
*/
@send
external fill: (array<'a>, 'a, ~start: int, ~end: int) => unit = "fill"

/**
`pop(array)` removes the last item from `array` and returns it.

Beware this will *mutate* the array.

See [`Array.pop`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/pop) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]

someArray
->Array.pop
->assertEqual(Some("hello"))

someArray->assertEqual(["hi"]) // Notice last item is gone.
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

someArray->assertEqual(["hi", "hello", "yay"])
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
someArray->assertEqual(["hi", "hello", "yay", "wehoo"])
```
*/
@variadic
@send
external pushMany: (array<'a>, array<'a>) => unit = "push"

/**
`reverse(array)` reverses the order of the items in `array`.

Beware this will *mutate* the array.

See [`Array.reverse`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reverse) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]
someArray->Array.reverse

someArray->assertEqual(["hello", "hi"])
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

someArray
->Array.shift
->assertEqual(Some("hi"))

someArray->assertEqual(["hello"]) // Notice first item is gone.
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

someArray
->Array.toSorted(Int.compare)
->assertEqual([1, 2, 3])

someArray->assertEqual([3, 2, 1]) // Original unchanged
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
array->assertEqual([1, 2, 3])
```
*/
@send
external sort: (array<'a>, ('a, 'a) => Stdlib_Ordering.t) => unit = "sort"

@variadic @send
external splice: (array<'a>, ~start: int, ~remove: int, ~insert: array<'a>) => unit = "splice"

@variadic @send
external toSpliced: (array<'a>, ~start: int, ~remove: int, ~insert: array<'a>) => array<'a> =
  "toSpliced"

@send external with: (array<'a>, int, 'a) => array<'a> = "with"

/**
`unshift(array, item)` inserts a new item at the start of the array.

Beware this will *mutate* the array.

See [`Array.unshift`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/unshift) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]
someArray->Array.unshift("yay")
someArray->assertEqual(["yay", "hi", "hello"])
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
someArray->assertEqual(["yay", "wehoo", "hi", "hello"])
```
*/
@variadic
@send
external unshiftMany: (array<'a>, array<'a>) => unit = "unshift"

/**
`concat(array1, array2)` concatenates the two arrays, creating a new array.

See [`Array.concat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/concat) on MDN.

## Examples

```rescript
let array1 = ["hi", "hello"]
let array2 = ["yay", "wehoo"]

let someArray = array1->Array.concat(array2)

someArray->assertEqual(["hi", "hello", "yay", "wehoo"])
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
@variadic
@send
external concatMany: (array<'a>, array<array<'a>>) => array<'a> = "concat"

/**
`flat(arrays)` concatenates an array of arrays into a single array.

See [`Array.flat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/flat) on MDN.

## Examples

```rescript
[[1], [2], [3, 4]]
->Array.flat
->assertEqual([1, 2, 3, 4])
```
*/
@send
external flat: array<array<'a>> => array<'a> = "flat"

/**
`includes(array, item)` checks whether `array` includes `item`, by doing a [strict check for equality](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality).

See [`Array.includes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/includes) on MDN.

## Examples

```rescript
[1, 2]->Array.includes(1)->assertEqual(true)
[1, 2]->Array.includes(3)->assertEqual(false)

[{"language": "ReScript"}]
->Array.includes({"language": "ReScript"})
->assertEqual(false) // false, because of strict equality
```
*/
@send
external includes: (array<'a>, 'a) => bool = "includes"

/**
`indexOf(array, item)` returns the index of the provided `item` in `array`. Uses [strict check for equality](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality) when comparing items.

Returns `-1` if the item doesn not exist. Check out `Array.indexOfOpt` for a version that returns `None` instead of `-1` if the item does not exist.

See [`Array.indexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/indexOf) on MDN.

## Examples

```rescript
[1, 2]->Array.indexOf(2)->assertEqual(1)
[1, 2]->Array.indexOf(3)->assertEqual(-1)

[{"language": "ReScript"}]
->Array.indexOf({"language": "ReScript"})
->assertEqual(-1) // -1, because of strict equality
```
*/
@send
external indexOf: (array<'a>, 'a) => int = "indexOf"

/**
`indexOfOpt(array, item)` returns an option of the index of the provided `item` in `array`. Uses [strict check for equality](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Strict_equality) when comparing items.

See [`Array.indexOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/indexOf) on MDN.

## Examples

```rescript
[1, 2]->Array.indexOfOpt(2)->assertEqual(Some(1))
[1, 2]->Array.indexOfOpt(3)->assertEqual(None)
[{"language": "ReScript"}]
->Array.indexOfOpt({"language": "ReScript"})
->assertEqual(None) // None, because of strict equality
```
*/
let indexOfOpt: (array<'a>, 'a) => option<int>
@send external indexOfFrom: (array<'a>, 'a, int) => int = "indexOf"

/**
`join(array, separator)` produces a string where all items of `array` are printed, separated by `separator`. Array items must be strings, to join number or other arrays, use `joinUnsafe`. Under the hood this will run JavaScript's `toString` on all the array items.

See [Array.join](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)

## Examples

```rescript
["One", "Two", "Three"]
->Array.join(" -- ")
->assertEqual("One -- Two -- Three")
```
*/
@send
external join: (array<string>, string) => string = "join"

/**
`joinWith(array, separator)` produces a string where all items of `array` are printed, separated by `separator`. Array items must be strings, to join number or other arrays, use `joinWithUnsafe`. Under the hood this will run JavaScript's `toString` on all the array items.

## Examples

```rescript
["One", "Two", "Three"]
->Array.joinWith(" -- ")
->assertEqual("One -- Two -- Three")
```
*/
@deprecated("Use `join` instead")
@send
external joinWith: (array<string>, string) => string = "join"

/**
`joinUnsafe(array, separator)` produces a string where all items of `array` are printed, separated by `separator`. Under the hood this will run JavaScript's `toString` on all the array items.

See [Array.join](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)

## Examples

```rescript
[1, 2, 3]
->Array.joinUnsafe(" -- ")
->assertEqual("1 -- 2 -- 3")
```
*/
@send
external joinUnsafe: (array<'a>, string) => string = "join"

/**
`joinWithUnsafe(array, separator)` produces a string where all items of `array` are printed, separated by `separator`. Under the hood this will run JavaScript's `toString` on all the array items.

## Examples

```rescript
[1, 2, 3]
->Array.joinWithUnsafe(" -- ")
->assertEqual("1 -- 2 -- 3")
```
*/
@deprecated("Use `joinUnsafe` instead")
@send
external joinWithUnsafe: (array<'a>, string) => string = "join"
@send external lastIndexOf: (array<'a>, 'a) => int = "lastIndexOf"
let lastIndexOfOpt: (array<'a>, 'a) => option<int>
@send external lastIndexOfFrom: (array<'a>, 'a, int) => int = "lastIndexOf"

/**
`slice(array, ~start, ~end)` creates a new array of items copied from `array` from `start` until (but not including) `end`.

See [`Array.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice) on MDN.

## Examples

```rescript
[1, 2, 3, 4]
->Array.slice(~start=1, ~end=3)
->assertEqual([2, 3])
```
*/
@send
external slice: (array<'a>, ~start: int, ~end: int) => array<'a> = "slice"

/**
`sliceToEnd(array, start)` creates a new array from `array`, with all items from `array` starting from `start`.

See [`Array.slice`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/slice) on MDN.

## Examples

```rescript
[1, 2, 3, 4]
->Array.sliceToEnd(~start=1)
->assertEqual([2, 3, 4])
```
*/
@send
external sliceToEnd: (array<'a>, ~start: int) => array<'a> = "slice"
/**
`copy(array)` makes a copy of the array with the items in it, but does not make copies of the items themselves.

## Examples

```rescript
let myArray = [1, 2, 3]
let copyOfMyArray = myArray->Array.copy

copyOfMyArray->assertEqual([1, 2, 3])
assertEqual(myArray === copyOfMyArray, false)
```
*/
@send
external copy: array<'a> => array<'a> = "slice"

/**
`toString(array)` stringifies `array` by running `toString` on all of the array elements and joining them with ",".

See [`Array.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toString) on MDN.

## Examples

```rescript
[1, 2, 3, 4]
->Array.toString
->assertEqual("1,2,3,4")
```
*/
@send
external toString: array<'a> => string = "toString"

@send external toLocaleString: array<'a> => string = "toLocaleString"

/**
`every(array, predicate)` returns true if `predicate` returns true for all items in `array`.

See [`Array.every`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/every) on MDN.

## Examples

```rescript
let array = [1, 2, 3, 4]

array
->Array.every(num => num <= 4)
->assertEqual(true)

array
->Array.every(num => num === 1)
->assertEqual(false)
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

array
->Array.everyWithIndex((num, index) => index < 5 && num <= 4)
->assertEqual(true)

array
->Array.everyWithIndex((num, index) => index < 2 && num >= 2)
->assertEqual(false)
```
*/
@send
external everyWithIndex: (array<'a>, ('a, int) => bool) => bool = "every"

/**
`filter(array, checker)` returns a new array containing all elements from `array` for which the provided `checker` function returns true.

See [`Array.filter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter) on MDN.

## Examples

```rescript
[1, 2, 3, 4]
->Array.filter(num => num > 2)
->assertEqual([3, 4])
```
*/
@send
external filter: (array<'a>, 'a => bool) => array<'a> = "filter"

/**
`filterWithIndex(array, checker)` returns a new array containing all elements from `array` for which the provided `checker` function returns true.

See [`Array.filter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter) on MDN.

## Examples

```rescript
[1, 2, 3, 4]
->Array.filterWithIndex((num, index) => index === 0 || num === 2)
->assertEqual([1, 2])
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

array
->Array.find(item => item == ReScript)
->assertEqual(Some(ReScript))
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

array
->Array.findWithIndex((item, index) => index > 1 && item == ReScript)
->assertEqual(Some(ReScript))
```
*/
@send
external findWithIndex: (array<'a>, ('a, int) => bool) => option<'a> = "find"

/**
`findIndex(array, checker)` returns the index of the first element of `array` where the provided `checker` function returns true.

Returns `-1` if the item does not exist. Consider using `Array.findIndexOpt` if you want an option instead (where `-1` would be `None`).

See [`Array.findIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/findIndex) on MDN.

## Examples

```rescript
type languages = ReScript | TypeScript | JavaScript

let array = [ReScript, JavaScript]

array
->Array.findIndex(item => item == ReScript)
->assertEqual(0)

array->Array.findIndex(item => item == TypeScript)
->assertEqual(-1)
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

let isReScriptFirst = array->Array.findIndexWithIndex((item, index) => index === 0 && item == ReScript)
let isTypeScriptFirst = array->Array.findIndexWithIndex((item, index) => index === 0 && item == TypeScript)

assertEqual(isReScriptFirst, 0)
assertEqual(isTypeScriptFirst, -1)
```
*/
@send
external findIndexWithIndex: (array<'a>, ('a, int) => bool) => int = "findIndex"

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

assertEqual(mappedArray, ["Hello to you", "Hi to you", "Good bye to you"])
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
  array->Array.mapWithIndex((greeting, index) =>
    greeting ++ " at position " ++ Int.toString(index)
  )

assertEqual(mappedArray, ["Hello at position 0", "Hi at position 1", "Good bye at position 2"])
```
*/
@send
external mapWithIndex: (array<'a>, ('a, int) => 'b) => array<'b> = "map"

/**
`reduce(xs, init, fn)`

Applies `fn` to each element of `xs` from beginning to end. Function `fn` has two parameters: the item from the list and an “accumulator”; which starts with a value of `init`. `reduce` returns the final value of the accumulator.

## Examples

```rescript
Array.reduce([2, 3, 4], 1, (a, b) => a + b)->assertEqual(10)

Array.reduce(["a", "b", "c", "d"], "", (a, b) => a ++ b)->assertEqual("abcd")

[1, 2, 3]
->Array.reduce(list{}, List.add)
->assertEqual(list{3, 2, 1})

Array.reduce([], list{}, List.add)->assertEqual(list{})
```
*/
let reduce: (array<'a>, 'b, ('b, 'a) => 'b) => 'b

/**
`reduceWithIndex(x, init, fn)`

Applies `fn` to each element of `xs` from beginning to end. Function `fn` has three parameters: the item from the array and an “accumulator”, which starts with a value of `init` and the index of each element. `reduceWithIndex` returns the final value of the accumulator.

## Examples

```rescript
Array.reduceWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i)->assertEqual(16)

Array.reduceWithIndex([1, 2, 3], list{}, (acc, v, i) => list{v + i, ...acc})->assertEqual(list{5, 3, 1})

Array.reduceWithIndex([], list{}, (acc, v, i) => list{v + i, ...acc})->assertEqual(list{})
```
*/
let reduceWithIndex: (array<'a>, 'b, ('b, 'a, int) => 'b) => 'b

/**
`reduceRight(xs, init, fn)`

Works like `Array.reduce`; except that function `fn` is applied to each item of `xs` from the last back to the first.

## Examples

```rescript
Array.reduceRight(["a", "b", "c", "d"], "", (a, b) => a ++ b)->assertEqual("dcba")

Array.reduceRight([1, 2, 3], list{}, List.add)->assertEqual(list{1, 2, 3})

Array.reduceRight([], list{}, List.add)->assertEqual(list{})
```
*/
let reduceRight: (array<'a>, 'b, ('b, 'a) => 'b) => 'b

/**
`reduceRightWithIndex(xs, init, fn)`

Like `reduceRight`, but with an additional index argument on the callback function.

## Examples

```rescript
Array.reduceRightWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i)->assertEqual(16)

Array.reduceRightWithIndex([], list{}, (acc, v, i) => list{v + i, ...acc})->assertEqual(list{})
```
*/
let reduceRightWithIndex: (array<'a>, 'b, ('b, 'a, int) => 'b) => 'b

/**
`some(array, predicate)` returns true if `predicate` returns true for any element in `array`.

See [`Array.some`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/some) on MDN.

## Examples

```rescript
let array = ["Hello", "Hi", "Good bye"]

array
->Array.some(greeting => greeting === "Hello")
->assertEqual(true)
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

array
->Array.someWithIndex((greeting, index) => greeting === "Hello" && index === 0)
->assertEqual(true)
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

array
->Array.get(0)
->assertEqual(Some("Hello"))

array
->Array.get(3)
->assertEqual(None)
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

array[1]->assertEqual(Some("Hello"))
```
*/
@set_index
external set: (array<'a>, int, 'a) => unit = ""
@get_index external getSymbol: (array<'a>, Stdlib_Symbol.t) => option<'b> = ""
@get_index external getSymbolUnsafe: (array<'a>, Stdlib_Symbol.t) => 'b = ""
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
@deprecated("Use getUnsafe instead. This will be removed in v13")
external unsafe_get: (array<'a>, int) => 'a = "%array_unsafe_get"

/**
`setUnsafe(array, index, item)` sets the provided `item` at `index` of `array`.

Beware this will *mutate* the array, and is *unsafe*.

## Examples

```rescript
let array = ["Hello", "Hi", "Good bye"]
array->Array.setUnsafe(1, "Hello")

assertEqual(array[1], Some("Hello"))
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

array
->Array.findIndexOpt(item => item == ReScript)
->assertEqual(Some(0))
```
*/
let findIndexOpt: (array<'a>, 'a => bool) => option<int>

/**
`toReversed(array)` creates a new array with all items from `array` in reversed order.

See [`Array.toReversed`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/toReversed) on MDN.

## Examples

```rescript
let someArray = ["hi", "hello"]
let reversed = someArray->Array.toReversed

reversed->assertEqual(["hello", "hi"])
someArray->assertEqual(["hi", "hello"]) // Original unchanged
```
*/
@send
external toReversed: array<'a> => array<'a> = "toReversed"

/**
`filterMap(array, fn)`

Calls `fn` for each element and returns a new array containing results of the `fn` calls which are not `None`.

## Examples

```rescript
["Hello", "Hi", "Good bye"]
->Array.filterMap(item =>
  switch item {
  | "Hello" => Some(item->String.length)
  | _ => None
  }
)
->assertEqual([5])

[1, 2, 3, 4, 5, 6]
->Array.filterMap(n => mod(n, 2) == 0 ? Some(n * n) : None)
->assertEqual([4, 16, 36])

Array.filterMap([1, 2, 3, 4, 5, 6], _ => None)->assertEqual([])

Array.filterMap([], n => mod(n, 2) == 0 ? Some(n * n) : None)->assertEqual([])
```
*/
let filterMap: (array<'a>, 'a => option<'b>) => array<'b>

/**
`keepSome(arr)`

Returns a new array containing `value` for all elements that are `Some(value)`
and ignoring every value that is `None`

## Examples

```rescript
Array.keepSome([Some(1), None, Some(3)])->assertEqual([1, 3])

Array.keepSome([Some(1), Some(2), Some(3)])->assertEqual([1, 2, 3])

Array.keepSome([None, None, None])->assertEqual([])

Array.keepSome([])->assertEqual([])
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

Array.toShuffled([1, 2, 3])
->Array.length
->assertEqual(3)
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

array2
->Array.length
->assertEqual(3)
```
*/
let shuffle: array<'a> => unit

/**
`flatMap(array, mapper)` returns a new array concatenating the arrays returned from running `mapper` on all items in `array`.

## Examples

```rescript
type language = ReScript | TypeScript | JavaScript

let array = [ReScript, TypeScript, JavaScript]

array
->Array.flatMap(item =>
  switch item {
  | ReScript => [1, 2, 3]
  | TypeScript => [4, 5, 6]
  | JavaScript => [7, 8, 9]
  }
)
->assertEqual([1, 2, 3, 4, 5, 6, 7, 8, 9])
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


array
->Array.flatMapWithIndex((item, index) =>
  switch item {
  | ReScript => [index]
  | TypeScript => [index, index + 1]
  | JavaScript => [index, index + 1, index + 2]
  }
)
->assertEqual([0, 1, 2, 2, 3, 4])
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
Array.findMap([1, 2, 3], n => mod(n, 2) == 0 ? Some(n - 2) : None)->assertEqual(Some(0))

Array.findMap([1, 2, 3, 4, 5, 6], n => mod(n, 2) == 0 ? Some(n - 8) : None)->assertEqual(Some(-6))

Array.findMap([1, 2, 3, 4, 5, 6], _ => None)->assertEqual(None)

Array.findMap([], n => mod(n, 2) == 0 ? Some(n * n) : None)->assertEqual(None)
```
*/
let findMap: (array<'a>, 'a => option<'b>) => option<'b>

/**
`at(array, index)`

Get an element by its index. Negative indices count backwards from the last item.

## Examples

```rescript
["a", "b", "c"]->Array.at(0)->assertEqual(Some("a"))
["a", "b", "c"]->Array.at(2)->assertEqual(Some("c"))
["a", "b", "c"]->Array.at(3)->assertEqual(None)
["a", "b", "c"]->Array.at(-1)->assertEqual(Some("c"))
["a", "b", "c"]->Array.at(-3)->assertEqual(Some("a"))
["a", "b", "c"]->Array.at(-4)->assertEqual(None)
```
*/
@send
external at: (array<'a>, int) => option<'a> = "at"

/**
`last(array)` returns the last element of `array`.

Returns `None` if the array is empty.

## Examples

```rescript
["Hello", "Hi", "Good bye"]
->Array.last
->assertEqual(Some("Good bye"))

[]
->Array.last
->assertEqual(None)
```
*/
let last: array<'a> => option<'a>

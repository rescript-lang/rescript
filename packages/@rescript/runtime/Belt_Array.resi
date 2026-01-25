/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the GNU Library General Public License, with */
/* the special exception on linking described in file ../LICENSE. */
/*  */
/* ********************************************************************* */
/* Adapted significantly by Authors of ReScript */

/***
Utilities for `Array` functions.
*/

type t<'a> = array<'a>

/**
Return the size of the array

## Examples

```rescript
Belt.Array.length(["test"]) == 1
```
*/
external length: t<'a> => int = "%array_length"

/** See [`Belt.Array.length`]() */
external size: t<'a> => int = "%array_length"

/**
If `i <= 0 <= length(arr)` returns `Some(value)` where `value` is the item at index `i`.
If `i` is out of range returns `None`.

## Examples

```rescript
Belt.Array.get(["a", "b", "c"], 0) == Some("a")
Belt.Array.get(["a", "b", "c"], 3) == None
Belt.Array.get(["a", "b", "c"], -1) == None
```
*/
let get: (t<'a>, int) => option<'a>

/**
Throw an exception if `i` is out of range.
Otherwise return the value at index `i` in `arr`.
*/
let getExn: (t<'a>, int) => 'a

/**
Throw an exception if `i` is out of range.
Otherwise return the value at index `i` in `arr`.
*/
let getOrThrow: (t<'a>, int) => 'a

/**
`getUnsafe(arr, i)`

**Unsafe**

no bounds checking; this would cause type error if `i` does not stay within range
*/
external getUnsafe: (t<'a>, int) => 'a = "%array_unsafe_get"

/**
`getUndefined(arr, i)`

It does the samething in the runtime as [`getUnsafe`]();
it is _type safe_ since the return type still track whether it is
in range or not
*/
external getUndefined: (t<'a>, int) => Js.undefined<'a> = "%array_unsafe_get"

/**
`set(arr, n, x)` modifies `arr` in place; it replaces the nth element of `arr`
with `x`. Returning `false` means not updated due to out of range.
*/
let set: (t<'a>, int, 'a) => bool

/**
`setExn(arr, i, x)` throw an exception if `i` is out of range.
*/
let setExn: (t<'a>, int, 'a) => unit

/**
`setOrThrow(arr, i, x)` throw an exception if `i` is out of range.
*/
let setOrThrow: (t<'a>, int, 'a) => unit

external setUnsafe: (t<'a>, int, 'a) => unit = "%array_unsafe_set"

/**
`shuffleInPlace(arr)` randomly re-orders the items in `arr`
*/
let shuffleInPlace: t<'a> => unit

/** Returns a fresh array with items in original array randomly shuffled. */
let shuffle: t<'a> => t<'a>

/**
`reverseInPlace(arr)` reverses items in `arr` in place.

## Examples

```rescript
let arr = [10, 11, 12, 13, 14]

let () = Belt.Array.reverseInPlace(arr)

arr == [14, 13, 12, 11, 10]
```
*/
let reverseInPlace: t<'a> => unit

/**
`reverse(arr)` returns a fresh array with items in arr in reverse order.

## Examples

```rescript
Belt.Array.reverse([10, 11, 12, 13, 14]) == [14, 13, 12, 11, 10]
```
*/
let reverse: t<'a> => t<'a>

/**
`makeUninitialized(n)` creates an array of length `n` filled with the undefined
value. You must specify the type of data that will eventually fill the array.

## Examples

```rescript
let arr: array<Js.undefined<string>> = Belt.Array.makeUninitialized(5)

Belt.Array.getExn(arr, 0) == Js.undefined
```
*/
@new
external makeUninitialized: int => array<Js.undefined<'a>> = "Array"

/**
**Unsafe**

## Examples

```rescript
let arr = Belt.Array.makeUninitializedUnsafe(5)

Js.log(Belt.Array.getExn(arr, 0)) // undefined

Belt.Array.setExn(arr, 0, "example")

Belt.Array.getExn(arr, 0) == "example"
```
*/
@new
external makeUninitializedUnsafe: int => t<'a> = "Array"

/**
`make(n, e)` return an array of size `n` filled with value `e`.
Returns an empty array when `n` is negative.
*/
let make: (int, 'a) => t<'a>

/**
`range(start, finish)` create an inclusive array.

## Examples

```rescript
Belt.Array.range(0, 3) == [0, 1, 2, 3]

Belt.Array.range(3, 0) == []

Belt.Array.range(3, 3) == [3]
```
*/
let range: (int, int) => array<int>

/**
`rangeBy(start, finish, ~step)` returns empty array when step is 0 or negative.
It also return an empty array when `start > finish`.

## Examples

```rescript
Belt.Array.rangeBy(0, 10, ~step=3) == [0, 3, 6, 9]

Belt.Array.rangeBy(0, 12, ~step=3) == [0, 3, 6, 9, 12]

Belt.Array.rangeBy(33, 0, ~step=1) == []

Belt.Array.rangeBy(33, 0, ~step=-1) == []

Belt.Array.rangeBy(3, 12, ~step=-1) == []

Belt.Array.rangeBy(3, 3, ~step=0) == []

Belt.Array.rangeBy(3, 3, ~step=1) == [3]
```
*/
let rangeBy: (int, int, ~step: int) => array<int>

@deprecated("Use `makeBy` instead")
let makeByU: (int, int => 'a) => t<'a>
/**
`makeBy(n, f)` return an empty array when n is negative return an array of size
n populated by `f(i)` start from `0` to `n - 1`.

## Examples

```rescript
Belt.Array.makeBy(5, i => i) == [0, 1, 2, 3, 4]

Belt.Array.makeBy(5, i => i * i) == [0, 1, 4, 9, 16]
```
*/
let makeBy: (int, int => 'a) => t<'a>

@deprecated("Use `makeByAndShuffle` instead")
let makeByAndShuffleU: (int, int => 'a) => t<'a>
/**
Equivalent to `shuffle(makeBy(n, f))`
*/
let makeByAndShuffle: (int, int => 'a) => t<'a>

/**
`zip(a, b)` create an array of pairs from corresponding elements of a and b.
Stop with the shorter array.

## Examples

```rescript
Belt.Array.zip([1, 2], [3, 4, 5]) == [(1, 3), (2, 4)]
```
*/
let zip: (t<'a>, array<'b>) => array<('a, 'b)>

@deprecated("Use `zipBy` instead")
let zipByU: (t<'a>, array<'b>, ('a, 'b) => 'c) => array<'c>
/**
`zipBy(xs, ys, f)` create an array by applying `f` to corresponding elements of
`xs` and `ys`. Stops with shorter array.

Equivalent to `map(zip(xs, ys), ((a, b)) => f(a, b))`

## Examples

```rescript
Belt.Array.zipBy([1, 2, 3], [4, 5], (a, b) => 2 * a + b) == [6, 9]
```
*/
let zipBy: (t<'a>, array<'b>, ('a, 'b) => 'c) => array<'c>

/**
`unzip(a)` takes an array of pairs and creates a pair of arrays. The first array
contains all the first items of the pairs; the second array contains all the
second items.

## Examples

```rescript
Belt.Array.unzip([(1, 2), (3, 4)]) == ([1, 3], [2, 4])

Belt.Array.unzip([(1, 2), (3, 4), (5, 6), (7, 8)]) == ([1, 3, 5, 7], [2, 4, 6, 8])
```
*/
let unzip: array<('a, 'b)> => (t<'a>, array<'b>)

/**
`concat(xs, ys)` returns a fresh array containing the concatenation of the arrays
`v1` and `v2`, so even if `v1` or `v2` is empty; it can not be shared.

## Examples

```rescript
Belt.Array.concat([1, 2, 3], [4, 5]) == [1, 2, 3, 4, 5]

Belt.Array.concat([], ["a", "b", "c"]) == ["a", "b", "c"]
```
*/
let concat: (t<'a>, t<'a>) => t<'a>

/**
`concatMany(xss)` returns a fresh array as the concatenation of `xss` (an array of arrays)

## Examples

```rescript
Belt.Array.concatMany([[1, 2, 3], [4, 5, 6], [7, 8]]) == [1, 2, 3, 4, 5, 6, 7, 8]
```
*/
let concatMany: array<t<'a>> => t<'a>

/**
`slice(xs, offset, len)` creates a new array with the len elements of `xs`
starting at `offset` for `offset` can be negative;and is evaluated as
`length(xs) - offset(slice, xs) - 1(1)` means get the last element as a
singleton array `slice(xs, ~-len, len)` will return a copy of the array if the
array does not have enough data; `slice` extracts through the end of sequence.

if `len` is negative; returns the empty array.

## Examples

```rescript
Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=2, ~len=3) == [12, 13, 14]

Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=-4, ~len=3) == [13, 14, 15]

Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=4, ~len=9) == [14, 15, 16]
```
*/
let slice: (t<'a>, ~offset: int, ~len: int) => t<'a>

/**
`sliceToEnd(xs, offset)` creates a new array with the elements of `xs` starting
at `offset`

`offset` can be negative; and is evaluated as `length(xs) - offset(sliceToEnd, xs) - 1`
means get the last element as a singleton array

`sliceToEnd(xs, 0)` will return a copy of the array

## Examples

```rescript
Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], 2) == [12, 13, 14, 15, 16]

Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], -4) == [13, 14, 15, 16]
```
*/
let sliceToEnd: (t<'a>, int) => t<'a>

/**
`copy(a)` returns a copy of `a`; that is; a fresh array containing the same
elements as `a`.
*/
@send
external copy: (t<'a>, @as(0) _) => t<'a> = "slice"

/**
`fill(arr, ~offset, ~len, x)` modifies `arr` in place, storing `x` in elements
number `offset` to `offset + len - 1`. `offset` can be negative; and is evaluated
as `length(arr - offset)`.

`fill(arr, ~offset=-1, ~len=1)` means fill the last element, if the array does not have enough data; `fill` will ignore it

## Examples

```rescript
let arr = Belt.Array.makeBy(5, i => i)

Belt.Array.fill(arr, ~offset=2, ~len=2, 9)

arr == [0, 1, 9, 9, 4]

Belt.Array.fill(arr, ~offset=7, ~len=2, 8)

arr == [0, 1, 9, 9, 4]
```
*/
let fill: (t<'a>, ~offset: int, ~len: int, 'a) => unit

/**
`blit(~src=v1, ~srcOffset=o1, ~dst=v2, ~dstOffset=o2, ~len)` copies `len` elements
from array `v1`;starting at element number `o1`;to array `v2`, starting at element
number `o2`. It works correctly even if `v1` and `v2` are the same array and the
source and destination chunks overlap.

`offset` can be negative; `-1` means `len - 1`; if `len + offset` is still negative;it will be set as 0

For each of the examples;presume that `v1 == [10, 11, 12, 13, 14, 15, 16, 17]` and `v2 == [20, 21, 22, 23, 24, 25, 26, 27]`. The result shown is the content of the destination array.

## Examples

```rescript
let v1 = [10, 11, 12, 13, 14, 15, 16, 17]
let v2 = [20, 21, 22, 23, 24, 25, 26, 27]

Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v2, ~dstOffset=2, ~len=3)
v2 == [20, 21, 14, 15, 16, 25, 26, 27]

Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v1, ~dstOffset=2, ~len=3)
v1 == [10, 11, 14, 15, 16, 15, 16, 17]
```
*/
let blit: (~src: t<'a>, ~srcOffset: int, ~dst: t<'a>, ~dstOffset: int, ~len: int) => unit

/**
Unsafe blit without bounds checking.
*/
let blitUnsafe: (~src: t<'a>, ~srcOffset: int, ~dst: t<'a>, ~dstOffset: int, ~len: int) => unit

@deprecated("Use `forEach` instead")
let forEachU: (t<'a>, 'a => unit) => unit
/**
`forEach(xs, f)`

Call `f` on each element of `xs` from the beginning to end. `f` returns `unit`
so no new array is created. Use `forEach` when you are primarily concerned with
repetitively creating side effects.

## Examples

```rescript
Belt.Array.forEach(["a", "b", "c"], x => Js.log("Item: " ++ x))

/*
  prints:
  Item: a
  Item: b
  Item: c
*/
let total = ref(0)

Belt.Array.forEach([1, 2, 3, 4], x => total := total.contents + x)

total.contents == 1 + 2 + 3 + 4
```
*/
let forEach: (t<'a>, 'a => unit) => unit

@deprecated("Use `map` instead")
let mapU: (t<'a>, 'a => 'b) => array<'b>
/**
`map(xs, f)` returns a new array by calling `f` for each element of `xs` from
the beginning to end.

## Examples

```rescript
Belt.Array.map([1, 2], x => x + 1) == [2, 3]
```
*/
let map: (t<'a>, 'a => 'b) => array<'b>

@deprecated("Use `flatMap` instead")
let flatMapU: (t<'a>, 'a => array<'b>) => array<'b>
/**
`flatMap(xs, f)` returns a new array by calling `f` for each element of `xs` from
the beginning to end, concatenating the results.

## Examples

```rescript
Belt.Array.flatMap([1, 2], x => [x + 10, x + 20]) == [11, 21, 12, 22]
```
*/
let flatMap: (t<'a>, 'a => array<'b>) => array<'b>

@deprecated("Use `getBy` instead")
let getByU: (t<'a>, 'a => bool) => option<'a>
/**
`getBy(xs, p)` returns `Some(value)` for the first value in `xs` that satisifies
the predicate function `p`; returns `None` if no element satisifies the function.

## Examples

```rescript
Belt.Array.getBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(4)
Belt.Array.getBy([15, 13, 11], x => mod(x, 2) == 0) == None
```
*/
let getBy: (t<'a>, 'a => bool) => option<'a>

@deprecated("Use `getIndexBy` instead")
let getIndexByU: (t<'a>, 'a => bool) => option<int>
/**
`getIndexBy(xs, p)` returns `Some(index)` for the first value in `xs` that
satisifies the predicate function `p`; returns `None` if no element satisifies
the function.

## Examples

```rescript
Belt.Array.getIndexBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(1)
Belt.Array.getIndexBy([15, 13, 11], x => mod(x, 2) == 0) == None
```
*/
let getIndexBy: (t<'a>, 'a => bool) => option<int>

@deprecated("Use `keep` instead")
let keepU: (t<'a>, 'a => bool) => t<'a>
/**
`keep(xs, p)` returns a new array that keep all elements satisfy `p`.
*/
let keep: (t<'a>, 'a => bool) => t<'a>

@deprecated("Use `keepWithIndex` instead")
let keepWithIndexU: (t<'a>, ('a, int) => bool) => t<'a>
/**
`keepWithIndex(xs, p)` returns a new array that keep all elements satisfy `p`.

## Examples

```rescript
Belt.Array.keepWithIndex([1, 2, 3], (_x, i) => i == 1) == [2]
```
*/
let keepWithIndex: (t<'a>, ('a, int) => bool) => t<'a>

@deprecated("Use `keepMap` instead")
let keepMapU: (t<'a>, 'a => option<'b>) => array<'b>
/**
`keepMap(xs, p)` returns a new array that keep all elements that return a non
None applied `p`.

## Examples

```rescript
Belt.Array.keepMap([1, 2, 3], x =>
  if mod(x, 2) == 0 {
    Some(x)
  } else {
    None
  }
) == [2]
```
*/
let keepMap: (t<'a>, 'a => option<'b>) => array<'b>

@deprecated("Use `forEachWithIndex` instead")
let forEachWithIndexU: (t<'a>, (int, 'a) => unit) => unit
/**
`forEachWithIndex(xs, f)` same as `Belt.Array.forEach`, except that `f` is
supplied two arguments: the index starting from 0 and the element from `xs`.

## Examples

```rescript
Belt.Array.forEachWithIndex(["a", "b", "c"], (i, x) =>
  Js.log("Item " ++ Belt.Int.toString(i) ++ " is " ++ x)
)

/*
  prints:
  Item 0 is a
  Item 1 is b
  Item 2 is cc
*/
let total = ref(0)

Belt.Array.forEachWithIndex([10, 11, 12, 13], (i, x) => total := total.contents + x + i)

total.contents == 0 + 10 + 1 + 11 + 2 + 12 + 3 + 13
```
*/
let forEachWithIndex: (t<'a>, (int, 'a) => unit) => unit

@deprecated("Use `mapWithIndex` instead")
let mapWithIndexU: (t<'a>, (int, 'a) => 'b) => array<'b>
/**
`mapWithIndex(xs, f)` applies `f` to each element of `xs`. Function `f` takes
two arguments: the index starting from 0 and the element from `xs`.

## Examples

```rescript
Belt.Array.mapWithIndex([1, 2, 3], (i, x) => i + x) == [0 + 1, 1 + 2, 2 + 3]
```
*/
let mapWithIndex: (t<'a>, (int, 'a) => 'b) => array<'b>

@deprecated("Use `partition` instead")
let partitionU: (t<'a>, 'a => bool) => (t<'a>, t<'a>)
/**
`partition(f, a)` split array into tuple of two arrays based on predicate `f`;
first of tuple where predicate cause true, second where predicate cause false

## Examples

```rescript
Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) == 0) == ([2, 4], [1, 3, 5])

Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) != 0) == ([1, 3, 5], [2, 4])
```
*/
let partition: (t<'a>, 'a => bool) => (t<'a>, t<'a>)

@deprecated("Use `reduce` instead")
let reduceU: (array<'b>, 'a, ('a, 'b) => 'a) => 'a
/**
`reduce(xs, init, f)` applies `f` to each element of `xs` from beginning to end.
Function `f` has two parameters: the item from the list and an “accumulator”;
which starts with a value of `init`. `reduce` returns the final value of the
accumulator.

## Examples

```rescript
Belt.Array.reduce([2, 3, 4], 1, (a, b) => a + b) == 10

Belt.Array.reduce(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "abcd"
```
*/
let reduce: (array<'b>, 'a, ('a, 'b) => 'a) => 'a

@deprecated("Use `reduceReverse` instead")
let reduceReverseU: (array<'b>, 'a, ('a, 'b) => 'a) => 'a
/**
`reduceReverse(xs, init, f)` works like `Belt.Array.reduce` except that
function `f` is applied to each item of `xs` from the last back to the first.

## Examples

```rescript
Belt.Array.reduceReverse(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "dcba"
```
*/
let reduceReverse: (array<'b>, 'a, ('a, 'b) => 'a) => 'a

@deprecated("Use `reduceReverse2` instead")
let reduceReverse2U: (t<'a>, array<'b>, 'c, ('c, 'a, 'b) => 'c) => 'c
/**
`reduceReverse2(xs, ys, init, f)` reduces two arrays xs and ys;taking items
starting at `min(length(xs), length(ys))` down to and including zero.

## Examples

```rescript
Belt.Array.reduceReverse2([1, 2, 3], [1, 2], 0, (acc, x, y) => acc + x + y) == 6
```
*/
let reduceReverse2: (t<'a>, array<'b>, 'c, ('c, 'a, 'b) => 'c) => 'c

@deprecated("Use `reduceWithIndex` instead")
let reduceWithIndexU: (t<'a>, 'b, ('b, 'a, int) => 'b) => 'b
/**
Applies `f` to each element of `xs` from beginning to end. Function `f` has
three parameters: the item from the array and an “accumulator”, which starts
with a value of `init` and the index of each element. `reduceWithIndex` returns
the final value of the accumulator.

## Examples

```rescript
Belt.Array.reduceWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i) == 16
```
*/
let reduceWithIndex: (t<'a>, 'b, ('b, 'a, int) => 'b) => 'b

@deprecated("Use `joinWith` instead")
let joinWithU: (t<'a>, string, 'a => string) => string
/**
`joinWith(xs, sep, toString)`

Concatenates all the elements of `xs` converted to string with `toString`, each
separated by `sep`, the string given as the second argument, into a single string.
If the array has only one element, then that element will be returned without
using the separator. If the array is empty, the empty string will be returned.

## Examples

```rescript
Belt.Array.joinWith([0, 1], ", ", Js.Int.toString) == "0, 1"
Belt.Array.joinWith([], " ", Js.Int.toString) == ""
Belt.Array.joinWith([1], " ", Js.Int.toString) == "1"
```
*/
let joinWith: (t<'a>, string, 'a => string) => string

@deprecated("Use `some` instead")
let someU: (t<'a>, 'a => bool) => bool
/**
`some(xs, p)` returns true if at least one of the elements in `xs` satifies `p`;
where `p` is a predicate: a function taking an element and returning a `bool`.

## Examples

```rescript
Belt.Array.some([2, 3, 4], x => mod(x, 2) == 1) == true

Belt.Array.some([-1, -3, -5], x => x > 0) == false
```
*/
let some: (t<'a>, 'a => bool) => bool

@deprecated("Use `every` instead")
let everyU: (t<'a>, 'a => bool) => bool
/**
`every(xs, p)` returns `true` if all elements satisfy `p`; where `p` is a
predicate: a function taking an element and returning a `bool`.

## Examples

```rescript
Belt.Array.every([1, 3, 5], x => mod(x, 2) == 1) == true

Belt.Array.every([1, -3, 5], x => x > 0) == false
```
*/
let every: (t<'a>, 'a => bool) => bool

@deprecated("Use `every2` instead")
let every2U: (t<'a>, array<'b>, ('a, 'b) => bool) => bool
/**
`every2(xs, ys, p)` returns true if `p(xi, yi)` is true for all pairs of
elements up to the shorter length (i.e. `min(length(xs), length(ys))`)

## Examples

```rescript
Belt.Array.every2([1, 2, 3], [0, 1], (a, b) => a > b) == true

Belt.Array.every2([], [1], (x, y) => x > y) == true

Belt.Array.every2([2, 3], [1], (x, y) => x > y) == true

Belt.Array.every2([0, 1], [5, 0], (x, y) => x > y) == false
```
*/
let every2: (t<'a>, array<'b>, ('a, 'b) => bool) => bool

@deprecated("Use `some2` instead")
let some2U: (t<'a>, array<'b>, ('a, 'b) => bool) => bool
/**
`some2(xs, ys, p)` returns true if `p(xi, yi)` is true for any pair of elements
up to the shorter length (i.e. `min(length(xs), length(ys))`)

## Examples

```rescript
Belt.Array.some2([0, 2], [1, 0, 3], (a, b) => a > b) == true

Belt.Array.some2([], [1], (x, y) => x > y) == false

Belt.Array.some2([2, 3], [1, 4], (x, y) => x > y) == true
```
*/
let some2: (t<'a>, array<'b>, ('a, 'b) => bool) => bool

@deprecated("Use `cmp` instead")
let cmpU: (t<'a>, t<'a>, ('a, 'a) => int) => int
/**
`cmp(xs, ys, f)` compared by length if `length(xs) != length(ys)`; returning `-1`
if `length(xs) < length(ys)` or 1 if `length(xs) > length(ys)`. Otherwise
compare one by one `f(x, y)`. `f` returns a negative number if `x` is “less than” `y`
zero if `x` is “equal to” `y` a positive number if `x` is “greater than”
`y`. The comparison returns the first non-zero result of `f`; or zero if `f`
returns zero for all `x` and `y`.

## Examples

```rescript
Belt.Array.cmp([1, 3, 5], [1, 4, 2], (a, b) => compare(a, b)) == -1

Belt.Array.cmp([1, 3, 5], [1, 2, 3], (a, b) => compare(a, b)) == 1

Belt.Array.cmp([1, 3, 5], [1, 3, 5], (a, b) => compare(a, b)) == 0
```
*/
let cmp: (t<'a>, t<'a>, ('a, 'a) => int) => int

@deprecated("Use `eq` instead")
let eqU: (t<'a>, t<'a>, ('a, 'a) => bool) => bool
/**
`eq(xs, ys)` return `false` if length is not the same otherwise compare items
one by one using `f(xi, yi)`; and return true if all results are true false otherwise

## Examples

```rescript
Belt.Array.eq([1, 2, 3], [-1, -2, -3], (a, b) => abs(a) == abs(b)) == true
```
*/
let eq: (t<'a>, t<'a>, ('a, 'a) => bool) => bool

/**
Unsafe `truncateToLengthUnsafe(xs, n)` sets length of array `xs` to `n`. If `n`
is greater than the length of `xs`; the extra elements are set to `Js.Null_undefined.null`.
If `n` is less than zero; throws a `RangeError`.

## Examples

```rescript
let arr = ["ant", "bee", "cat", "dog", "elk"]

Belt.Array.truncateToLengthUnsafe(arr, 3)

arr == ["ant", "bee", "cat"]
```
*/
@set
external truncateToLengthUnsafe: (t<'a>, int) => unit = "length"

@deprecated("Use `init` instead")
let initU: (int, int => 'a) => t<'a>
let init: (int, int => 'a) => t<'a>

/**
`arr->push(item)` pushes an element `item` into an array `arr`.
*/
@send
external push: (t<'a>, 'a) => unit = "push"

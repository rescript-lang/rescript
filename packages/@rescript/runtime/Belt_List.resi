/* Copyright (C) 2017 Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/***
Collection functions for manipulating the `list` data structures, a singly-linked list.

**Prefer Array** if you need any of the following:

- Random access of element
- Better interop with JavaScript
- Better memory usage & performance.
*/

/** `'a t` is compatible with built-in `list` type */
type t<'a> = list<'a>

/**
Returns the length of a list.

## Examples

```rescript
Belt.List.length(list{1, 2, 3}) == 3
```
*/
let length: t<'a> => int

/** See `Belt.List.length` */
let size: t<'a> => int

/**
Returns `Some(value)` where `value` is the first element in the list, or
`None` if `someList` is an empty list.

## Examples

```rescript
Belt.List.head(list{}) == None
Belt.List.head(list{1, 2, 3}) == Some(1)
```
*/
let head: t<'a> => option<'a>

/**
Same as `Belt.List.head` but throws an exception if `someList` is empty. Use
with care.

## Examples

```rescript
Belt.List.headExn(list{1, 2, 3}) == 1

switch Belt.List.headExn(list{}) {
// Throws an Error
| exception _ => assert(true)
| _ => assert(false)
}
```
*/
let headExn: t<'a> => 'a

/**
Same as `Belt.List.head` but throws an exception if `someList` is empty. Use
with care.

## Examples

```rescript
Belt.List.headOrThrow(list{1, 2, 3}) == 1

switch Belt.List.headOrThrow(list{}) {
// Throws an Error
| exception _ => assert(true)
| _ => assert(false)
}
```
*/
let headOrThrow: t<'a> => 'a

/**
Returns `None` if `someList` is empty, otherwise it returns `Some(tail)`
where `tail` is everything except the first element of `someList`.

## Examples

```rescript
Belt.List.tail(list{1, 2, 3}) == Some(list{2, 3})

Belt.List.tail(list{}) == None
```
*/
let tail: t<'a> => option<t<'a>>

/**
Same as `Belt.List.tail` but throws an exception if `someList` is empty. Use
with care.

## Examples

```rescript
Belt.List.tailExn(list{1, 2, 3}) == list{2, 3}

switch Belt.List.tailExn(list{}) {
// Throws an Error
| exception _ => assert(true)
| _ => assert(false)
}
```
*/
let tailExn: t<'a> => t<'a>

/**
Same as `Belt.List.tail` but throws an exception if `someList` is empty. Use
with care.

## Examples

```rescript
Belt.List.tailOrThrow(list{1, 2, 3}) == list{2, 3}

switch Belt.List.tailOrThrow(list{}) {
// Throws an Error
| exception _ => assert(true)
| _ => assert(false)
}
```
*/
let tailOrThrow: t<'a> => t<'a>

/**
Adds `value` to the beginning of `someList`.

## Examples

```rescript
Belt.List.add(list{2, 3}, 1) // list{1, 2, 3}

Belt.List.add(list{"World", "!"}, "Hello") == list{"Hello", "World", "!"}
```
*/
let add: (t<'a>, 'a) => t<'a>

/**
Return the nth element in `someList`, or `None` if `index` is larger than the
length.

## Examples

```rescript
let abc = list{"A", "B", "C"}

abc->Belt.List.get(1) == Some("B")

abc->Belt.List.get(4) == None
```
*/
let get: (t<'a>, int) => option<'a>

/**
Same as `Belt.List.get` but throws an exception if `index` is larger than the
length. Use with care.

## Examples

```rescript
let abc = list{"A", "B", "C"}

abc->Belt.List.getExn(1) == "B"

switch abc->Belt.List.getExn(4) {
// Throws an Error
| exception _ => assert(true)
| _ => assert(false)
}
```
*/
let getExn: (t<'a>, int) => 'a

/**
Same as `Belt.List.get` but throws an exception if `index` is larger than the
length. Use with care.

## Examples

```rescript
let abc = list{"A", "B", "C"}

abc->Belt.List.getOrThrow(1) == "B"

switch abc->Belt.List.getOrThrow(4) {
// Throws an Error
| exception _ => assert(true)
| _ => assert(false)
}
```
*/
let getOrThrow: (t<'a>, int) => 'a

/**
Returns a list of length `numItems` with each element filled with value `v`. Returns an empty list if `numItems` is negative.

## Examples

```rescript
Belt.List.make(3, 1) == list{1, 1, 1}
```
*/
let make: (int, 'a) => t<'a>

/** Uncurried version of [makeBy](#makeBy) */
@deprecated("Use `makeBy` instead")
let makeByU: (int, int => 'a) => t<'a>

/**
Return a list of length `numItems` with element `i` initialized with `f(i)`.
Returns an empty list if `numItems` is negative.

## Examples

```rescript
Belt.List.makeBy(5, i => i) == list{0, 1, 2, 3, 4}

Belt.List.makeBy(5, i => i * i) == list{0, 1, 4, 9, 16}
```
*/
let makeBy: (int, int => 'a) => t<'a>

/**
Returns a new list in random order.

## Examples

```rescript
Belt.List.shuffle(list{1, 2, 3}) // list{2, 1, 3}
```
*/
let shuffle: t<'a> => t<'a>

/**
Return a new list, dropping the first `n` elements. Returns `None` if `someList` has fewer than `n` elements.

## Examples

```rescript
list{1, 2, 3}->Belt.List.drop(2) == Some(list{3})

list{1, 2, 3}->Belt.List.drop(3) == Some(list{})

list{1, 2, 3}->Belt.List.drop(4) == None
```
*/
let drop: (t<'a>, int) => option<t<'a>>

/**
Returns a list with the first `n` elements from `someList`, or `None` if `someList` has fewer than `n` elements.

## Examples

```rescript
list{1, 2, 3}->Belt.List.take(1) == Some(list{1})

list{1, 2, 3}->Belt.List.take(2) == Some(list{1, 2})

list{1, 2, 3}->Belt.List.take(4) == None
```
*/
let take: (t<'a>, int) => option<t<'a>>

/**
Split the list `someList` at `index`. Returns `None` when the length of `someList` is less than `index`.

## Examples

```rescript
list{"Hello", "World"}->Belt.List.splitAt(1) == Some((list{"Hello"}, list{"World"}))

list{0, 1, 2, 3, 4}->Belt.List.splitAt(2) == Some((list{0, 1}, list{2, 3, 4}))
```
*/
let splitAt: (t<'a>, int) => option<(list<'a>, list<'a>)>

/**
Returns the list obtained by adding `secondList` after `firstList`.

## Examples

```rescript
Belt.List.concat(list{1, 2, 3}, list{4, 5}) == list{1, 2, 3, 4, 5}
```
*/
let concat: (t<'a>, t<'a>) => t<'a>

/**
Returns the list obtained by concatenating all the lists in array `a`, in
order.

## Examples

```rescript
Belt.List.concatMany([list{1, 2, 3}, list{}, list{3}]) == list{1, 2, 3, 3}
```
*/
let concatMany: array<t<'a>> => t<'a>

/**
Equivalent to writing: `concat(reverse(firstList, secondList)`

## Examples

```rescript
Belt.List.reverseConcat(list{1, 2}, list{3, 4}) == list{2, 1, 3, 4}
```
*/
let reverseConcat: (t<'a>, t<'a>) => t<'a>

/**
Return the list obtained by concatenating all the lists in list `ls`, in order.

## Examples

```rescript
Belt.List.flatten(list{list{1, 2, 3}, list{}, list{3}}) == list{1, 2, 3, 3}
```
*/
let flatten: t<t<'a>> => t<'a>

/** Uncurried version of [map](#map). */
@deprecated("Use `map` instead")
let mapU: (t<'a>, 'a => 'b) => t<'b>

/**
Returns a new list with `f` applied to each element of `someList`.

## Examples

```rescript
list{1, 2}->Belt.List.map(x => x + 1) == list{2, 3}
```
*/
let map: (t<'a>, 'a => 'b) => t<'b>

/**
Returns a list of pairs from the two lists with the length of the shorter list.

## Examples

```rescript
Belt.List.zip(list{1, 2}, list{3, 4, 5}) == list{(1, 3), (2, 4)}
```
*/
let zip: (t<'a>, t<'b>) => t<('a, 'b)>

/** Uncurried version of [zipBy](#zipBy). */
@deprecated("Use `zipBy` instead")
let zipByU: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>

/**
See [Belt.List.zip](#zip)

## Examples

```rescript
Belt.List.zipBy(list{1, 2, 3}, list{4, 5}, (a, b) => 2 * a + b) == list{6, 9}
```
*/
let zipBy: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>

/** Uncurried version of [mapWithIndex](#mapWithIndex). */
@deprecated("Use `mapWithIndex` instead")
let mapWithIndexU: (t<'a>, (int, 'a) => 'b) => t<'b>

/**
Applies `f` to each element of `someList`.
Function `f` takes two arguments: the index starting from 0 and the element from `someList`, in that order.

## Examples

```rescript
list{1, 2, 3}->Belt.List.mapWithIndex((index, x) => index + x) == list{1, 3, 5}
```
*/
let mapWithIndex: (t<'a>, (int, 'a) => 'b) => t<'b>

/**
Converts the given array to a list.

## Examples

```rescript
Belt.List.fromArray([1, 2, 3]) == list{1, 2, 3}
```
*/
let fromArray: array<'a> => t<'a>

/**
Converts the given list to an array.

## Examples

```rescript
Belt.List.toArray(list{1, 2, 3}) == [1, 2, 3]
```
*/
let toArray: t<'a> => array<'a>

/* type json = Js_json.t */

/* val toJson : 'a t -> ('a -> json  [@bs]) -> json */
/* val fromJson : json -> (json -> 'a [@bs]) -> 'a t */

/**
Returns a new list whose elements are those of `someList` in reversed order.

## Examples

```rescript
Belt.List.reverse(list{1, 2, 3}) == list{3, 2, 1}
```
*/
let reverse: t<'a> => t<'a>

/** Uncurried version of [mapReverse](#mapReverse). */
@deprecated("Use `mapReverse` instead")
let mapReverseU: (t<'a>, 'a => 'b) => t<'b>

/**
Equivalent to `Belt.List.map(someList, f)->Belt.List.reverse`

## Examples

```rescript
list{3, 4, 5}->Belt.List.mapReverse(x => x * x) == list{25, 16, 9}
```
*/
let mapReverse: (t<'a>, 'a => 'b) => t<'b>

/** Uncurried version of [forEach](#forEach). */
@deprecated("Use `forEach` instead")
let forEachU: (t<'a>, 'a => 'b) => unit

/**
Call `f` on each element of `someList` from the beginning to end.
`f` returns `unit`, so no new array is created. Use `forEach` when you are primarily concerned with repetitively creating side effects.

## Examples

```rescript
Belt.List.forEach(list{"a", "b", "c"}, x => Js.log("Item: " ++ x))
/*
  prints:
  Item: a
  Item: b
  Item: c
*/
```
*/
let forEach: (t<'a>, 'a => 'b) => unit

/** Uncurried version of [forEachWithIndex](#forEachWithIndex). */
@deprecated("Use `forEachWithIndex` instead")
let forEachWithIndexU: (t<'a>, (int, 'a) => 'b) => unit

/**
Call `f` on each element of `someList` from beginning to end.
Function `f` takes two arguments: the index starting from 0 and the element from `someList`. `f` returns `unit`.

## Examples

```rescript
Belt.List.forEachWithIndex(list{"a", "b", "c"}, (index, x) => {
  Js.log("Item " ++ Belt.Int.toString(index) ++ " is " ++ x)
})
/*
  prints:
  Item 0 is a
  Item 1 is b
  Item 2 is cc
*/
```
*/
let forEachWithIndex: (t<'a>, (int, 'a) => 'b) => unit

/** Uncurried version of [reduce](#reduce). */
@deprecated("Use `reduce` instead")
let reduceU: (t<'a>, 'b, ('b, 'a) => 'b) => 'b

/**
Applies `f` to each element of `someList` from beginning to end. Function `f` has two parameters: the item from the list and an “accumulator”, which starts with a value of `initialValue`. reduce returns the final value of the accumulator.

## Examples

```rescript
list{1, 2, 3, 4}->Belt.List.reduce(0, (a, b) => a + b) == 10

/* same as */

list{1, 2, 3, 4}->Belt.List.reduce(0, (acc, item) => acc + item) == 10
```
*/
let reduce: (t<'a>, 'b, ('b, 'a) => 'b) => 'b

/** Uncurried version of [reduceWithIndex](#reduceWithIndex). */
@deprecated("Use `reduceWithIndex` instead")
let reduceWithIndexU: (t<'a>, 'b, ('b, 'a, int) => 'b) => 'b

/**
Applies `f` to each element of `someList` from beginning to end. Function `f` has three parameters: the item from the list and an “accumulator”, which starts with a value of `initialValue` and the index of each element. `reduceWithIndex` returns the final value of the accumulator.

## Examples

```rescript
list{1, 2, 3, 4}->Belt.List.reduceWithIndex(0, (acc, item, index) => acc + item + index) == 16
```
*/
let reduceWithIndex: (t<'a>, 'b, ('b, 'a, int) => 'b) => 'b

/** Uncurried version of [reduceReverse](#reduceReverse). */
@deprecated("Use `reduceReverse` instead")
let reduceReverseU: (t<'a>, 'b, ('b, 'a) => 'b) => 'b

/**
Works like [reduce](#reduce), except that function `f` is applied to each
item of `someList` from the last back to the first.

## Examples

```rescript
list{1, 2, 3, 4}->Belt.List.reduceReverse(0, (a, b) => a + b) == 10

list{1, 2, 3, 4}->Belt.List.reduceReverse(10, (a, b) => a - b) == 0

list{1, 2, 3, 4}->Belt.List.reduceReverse(list{}, Belt.List.add) == list{1, 2, 3, 4}
```
*/
let reduceReverse: (t<'a>, 'b, ('b, 'a) => 'b) => 'b

/** Uncurried version of [mapReverse2](#mapReverse2). */
@deprecated("Use `mapReverse2` instead")
let mapReverse2U: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>

/**
Equivalent to: `zipBy(xs, ys, f)->reverse`

## Examples

```rescript
Belt.List.mapReverse2(list{1, 2, 3}, list{1, 2}, (a, b) => a + b) == list{4, 2}
```
*/
let mapReverse2: (t<'a>, t<'b>, ('a, 'b) => 'c) => t<'c>

/** Uncurried version of [forEach2](#forEach2). */
@deprecated("Use `forEach2` instead")
let forEach2U: (t<'a>, t<'b>, ('a, 'b) => 'c) => unit

/**
Stops at the length of the shorter list.

## Examples

```rescript
Belt.List.forEach2(list{"Z", "Y"}, list{"A", "B", "C"}, (x, y) => Js.log2(x, y))

/*
  prints:
  "Z" "A"
  "Y" "B"
*/
```
*/
let forEach2: (t<'a>, t<'b>, ('a, 'b) => 'c) => unit

/** Uncurried version of [reduce2](#reduce2). */
@deprecated("Use `reduce2` instead")
let reduce2U: (t<'b>, t<'c>, 'a, ('a, 'b, 'c) => 'a) => 'a

/**
Applies `f` to each element of `firstList` and `secondList` from beginning to end. Stops with the shorter list. Function `f` has three parameters: an “accumulator” which starts with a value of `initialValue`, an item from `firstList`, and an item from `secondList`. `reduce2` returns the final value of the accumulator.

## Examples

```rescript
Belt.List.reduce2(list{1, 2, 3}, list{4, 5}, 0, (acc, x, y) => acc + x * x + y) ==
  0 + (1 * 1 + 4) + (2 * 2 + 5)
```
*/
let reduce2: (t<'b>, t<'c>, 'a, ('a, 'b, 'c) => 'a) => 'a

/** Uncurried version of [reduceReverse2](#reduceReverse2). */
@deprecated("Use `reduceReverse2` instead")
let reduceReverse2U: (t<'a>, t<'b>, 'c, ('c, 'a, 'b) => 'c) => 'c

/**
Applies `f` to each element of `firstList` and `secondList` from end to
beginning. Stops with the shorter list. Function `f` has three parameters: an
“accumulator” which starts with a value of init, an item from `firstList`,
and an item from `secondList`. `reduce2` returns the final value of the
accumulator.

## Examples

```rescript
Belt.List.reduceReverse2(list{1, 2, 3}, list{4, 5}, 0, (acc, x, y) => acc + x * x + y) ==
  0 + (1 * 1 + 4) + (2 * 2 + 5)
```
*/
let reduceReverse2: (t<'a>, t<'b>, 'c, ('c, 'a, 'b) => 'c) => 'c

/** Uncurried version of [every](#every). */
@deprecated("Use `every` instead")
let everyU: (t<'a>, 'a => bool) => bool

/**
Returns `true` if all elements satisfy `pred`, where `pred` is a predicate: a function taking an element and returning a bool.

## Examples

```rescript
let isBelow10 = value => value < 10

list{1, 9, 8, 2}->Belt.List.every(isBelow10) == true

list{1, 99, 8, 2}->Belt.List.every(isBelow10) == false
```
*/
let every: (t<'a>, 'a => bool) => bool

/** Uncurried version of [some](#some). */
@deprecated("Use `some` instead")
let someU: (t<'a>, 'a => bool) => bool

/**
Returns `true` if at least _one_ of the elements in `someList` satisfies
`pred`, where `pred` is a predicate: a function taking an element and
returning a bool.

## Examples

```rescript
let isAbove100 = value => value > 100

list{101, 1, 2, 3}->Belt.List.some(isAbove100) == true

list{1, 2, 3, 4}->Belt.List.some(isAbove100) == false
```
*/
let some: (t<'a>, 'a => bool) => bool

/** Uncurried version of [every2](#every2). */
@deprecated("Use `every2` instead")
let every2U: (t<'a>, t<'b>, ('a, 'b) => bool) => bool

/**
Returns `true` if predicate `pred(a, b)` is `true` for all pairs of elements
up to the shorter length (i.e. `min(length(firstList), length(secondList))`)

## Examples

```rescript
Belt.List.every2(list{1, 2, 3}, list{0, 1}, (a, b) => a > b) == true

Belt.List.every2(list{}, list{1}, (a, b) => a > b) == true

Belt.List.every2(list{2, 3}, list{1}, (a, b) => a > b) == true

Belt.List.every2(list{0, 1}, list{5, 0}, (a, b) => a > b) == false
```
*/
let every2: (t<'a>, t<'b>, ('a, 'b) => bool) => bool

/** Uncurried version of [some2](#some2). */
@deprecated("Use `some2` instead")
let some2U: (t<'a>, t<'b>, ('a, 'b) => bool) => bool

/**
Returns `true` if predicate `pred(a, b)` is true for any pair of elements up
to the shorter length (i.e. `min(length(firstList), length(secondList))`)

## Examples

```rescript
Belt.List.some2(list{1, 2, 3}, list{0, 1}, (a, b) => a > b) == true

Belt.List.some2(list{}, list{1}, (a, b) => a > b) == false

Belt.List.some2(list{2, 3}, list{1}, (a, b) => a > b) == true

Belt.List.some2(list{0, 1}, list{5, 0}, (a, b) => a > b) == true
```
*/
let some2: (t<'a>, t<'b>, ('a, 'b) => bool) => bool

/**
Compare two lists solely by length. Returns `-1` if `length(firstList)` is
less than `length(secondList)`, `0` if `length(firstList)` equals
`length(secondList)`, and `1` if `length(firstList)` is greater than
`length(secondList)`.

## Examples

```rescript
Belt.List.cmpByLength(list{1, 2}, list{3, 4, 5, 6}) == -1

Belt.List.cmpByLength(list{1, 2, 3}, list{4, 5, 6}) == 0

Belt.List.cmpByLength(list{1, 2, 3, 4}, list{5, 6}) == 1
```
*/
let cmpByLength: (t<'a>, t<'a>) => int

/** Uncurried version of [cmp](#cmp). */
@deprecated("Use `cmp` instead")
let cmpU: (t<'a>, t<'a>, ('a, 'a) => int) => int

/**
Compare elements one by one `compareFn(a, b)`. `compareFn` returns a negative number if `a` is "less than" `b`, zero if `a` is "equal to" `b`, a positive number if `a` is "greater than" `b`.

The comparison returns the first non-zero result of `compareFn`, or zero if `compareFn` returns zero for all `a` and `b`.

If all items have compared equal, but `firstList` is exhausted first, return `-1`. (`firstList` is shorter).
If all items have compared equal, but `secondList` is exhausted first, return `1` (`firstList` is longer).

## Examples

```rescript
Belt.List.cmp(list{3}, list{3, 7}, (a, b) => compare(a, b)) == -1

Belt.List.cmp(list{5, 3}, list{5}, (a, b) => compare(a, b)) == 1

Belt.List.cmp(list{1, 3, 5}, list{1, 4, 2}, (a, b) => compare(a, b)) == -1

Belt.List.cmp(list{1, 3, 5}, list{1, 2, 3}, (a, b) => compare(a, b)) == 1

Belt.List.cmp(list{1, 3, 5}, list{1, 3, 5}, (a, b) => compare(a, b)) == 0
```

**Please note:** The total ordering of List is different from Array,
for Array, we compare the length first and, only if the lengths are equal, elements one by one.
For lists, we just compare elements one by one.
*/
let cmp: (t<'a>, t<'a>, ('a, 'a) => int) => int

/** Uncurried version of [eq](#eq). */
@deprecated("Use `eq` instead")
let eqU: (t<'a>, t<'a>, ('a, 'a) => bool) => bool

/**
Check equality of `firstList` and `secondList` using `eqElem` for equality on
elements, where `eqElem` is a function that returns `true` if items `x` and
`y` meet some criterion for equality, `false` otherwise. eq `false` if length
of `firstList` and `secondList` are not the same.

## Examples

```rescript
Belt.List.eq(list{1, 2, 3}, list{1, 2}, (a, b) => a == b) == false

Belt.List.eq(list{1, 2}, list{1, 2}, (a, b) => a == b) == true

Belt.List.eq(list{1, 2, 3}, list{-1, -2, -3}, (a, b) => abs(a) == abs(b)) == true
```
*/
let eq: (t<'a>, t<'a>, ('a, 'a) => bool) => bool

/** Uncurried version of [has](#has). */
@deprecated("Use `has` instead")
let hasU: (t<'a>, 'b, ('a, 'b) => bool) => bool

/**
Returns `true` if the list contains at least one element for which
`eqFunction(x)` returns true.

## Examples

```rescript
list{1, 2, 3}->Belt.List.has(2, (a, b) => a == b) == true

list{1, 2, 3}->Belt.List.has(4, (a, b) => a == b) == false

list{-1, -2, -3}->Belt.List.has(2, (a, b) => abs(a) == abs(b)) == true
```
*/
let has: (t<'a>, 'b, ('a, 'b) => bool) => bool

/** Uncurried version of [getBy](#getBy). */
@deprecated("Use `getBy` instead")
let getByU: (t<'a>, 'a => bool) => option<'a>

/**
Returns `Some(value)` for the first value in `someList` that satisfies the
predicate function `pred`. Returns `None` if no element satisfies the function.

## Examples

```rescript
Belt.List.getBy(list{1, 4, 3, 2}, x => x > 3) == Some(4)

Belt.List.getBy(list{1, 4, 3, 2}, x => x > 4) == None
```
*/
let getBy: (t<'a>, 'a => bool) => option<'a>

/** Uncurried version of [keep](#keep). */
@deprecated("Use `keep` instead")
let keepU: (t<'a>, 'a => bool) => t<'a>

/**
Returns a list of all elements in `someList` which satisfy the predicate function `pred`.

## Examples

```rescript
let isEven = x => mod(x, 2) == 0

Belt.List.keep(list{1, 2, 3, 4}, isEven) == list{2, 4}

Belt.List.keep(list{None, Some(2), Some(3), None}, Belt.Option.isSome) == list{Some(2), Some(3)}
```
*/
let keep: (t<'a>, 'a => bool) => t<'a>

/**
Returns a list of all elements in `someList` which satisfy the predicate function `pred`.

## Examples

```rescript
let isEven = x => mod(x, 2) == 0

Belt.List.filter(list{1, 2, 3, 4}, isEven) == list{2, 4}

Belt.List.filter(list{None, Some(2), Some(3), None}, Belt.Option.isSome) == list{Some(2), Some(3)}
```
*/
@deprecated("This function will soon be deprecated. Please, use `List.keep` instead.")
let filter: (t<'a>, 'a => bool) => t<'a>

/** Uncurried version of [keepWithIndex](#keepWithIndex). */
@deprecated("Use `keepWithIndex` instead")
let keepWithIndexU: (t<'a>, ('a, int) => bool) => t<'a>

/**
Returns a list of all elements in `someList` which satisfy the predicate function `pred`.

## Examples

```rescript
let isEven = x => mod(x, 2) == 0

Belt.List.keepWithIndex(list{1, 2, 3, 4}, (_x, index) => isEven(index)) == list{1, 3}
```
*/
let keepWithIndex: (t<'a>, ('a, int) => bool) => t<'a>

/**
Returns a list of all elements in `someList` which satisfy the predicate function `pred`.

## Examples

```rescript
let isEven = x => mod(x, 2) == 0

Belt.List.filterWithIndex(list{1, 2, 3, 4}, (_x, index) => isEven(index)) == list{1, 3}
```
*/
@deprecated(
  "This function will soon be deprecated. Please, use `List.keepWithIndex` \
     instead."
)
let filterWithIndex: (t<'a>, ('a, int) => bool) => t<'a>

/** Uncurried version of [keepMap](#keepMap). */
@deprecated("Use `keepMap` instead")
let keepMapU: (t<'a>, 'a => option<'b>) => t<'b>

/**
Applies `f` to each element of `someList`. If `f(x)` returns `Some(value)`, then `value` is _kept_ in the resulting list.
If `f(x)` returns `None`, the element is _not_ retained in the result.

## Examples

```rescript
let isEven = x => mod(x, 2) == 0

list{1, 2, 3, 4}->Belt.List.keepMap(x =>
  if isEven(x) {
    Some(x)
  } else {
    None
  }
) /* list{2, 4} */

list{Some(1), Some(2), None}->Belt.List.keepMap(x => x) == list{1, 2}
```
*/
let keepMap: (t<'a>, 'a => option<'b>) => t<'b>

/** Uncurried version of [partition](#partition). */
@deprecated("Use `partition` instead")
let partitionU: (t<'a>, 'a => bool) => (t<'a>, t<'a>)

/**
Creates a pair of lists; the first list consists of all elements of `someList` that satisfy the predicate function `pred`; the second list consists of all elements of `someList` that _do not_ satisfy \`pred.

In other words:

```
(elementsThatSatisfies, elementsThatDoesNotSatisfy)
```

## Examples

```rescript
list{1, 2, 3, 4}->Belt.List.partition(x => x > 2) == (list{3, 4}, list{1, 2})
```
*/
let partition: (t<'a>, 'a => bool) => (t<'a>, t<'a>)

/**
Takes a list of pairs and creates a pair of lists. The first list contains all the first items of the pairs; the second list contains all the second items.

## Examples

```rescript
Belt.List.unzip(list{(1, 2), (3, 4)}) == (list{1, 3}, list{2, 4})

Belt.List.unzip(list{("H", "W"), ("e", "o"), ("l", "r"), ("l", "l"), ("o", "d"), (" ", "!")}) ==
  (list{"H", "e", "l", "l", "o", " "}, list{"W", "o", "r", "l", "d", "!"})
```
*/
let unzip: t<('a, 'b)> => (t<'a>, t<'b>)

/** Uncurried version of [getAssoc](#getAssoc). */
@deprecated("Use `getAssoc` instead")
let getAssocU: (t<('a, 'c)>, 'b, ('a, 'b) => bool) => option<'c>

/**
Return the second element of a pair in `someList` where the first element equals `k` as per the predicate function `eqFunction`, or `None` if not found.

## Examples

```rescript
list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.getAssoc(3, (a, b) => a == b) /* Some("c") */

list{(9, "morning"), (15, "afternoon"), (22, "night")}->Belt.List.getAssoc(15, (k, item) =>
  k /* 15 */ == item
) == Some("afternoon")
/* 9, 5, 22 */
```
*/
let getAssoc: (t<('a, 'c)>, 'b, ('a, 'b) => bool) => option<'c>

/** Uncurried version of [hasAssoc](#hasAssoc). */
@deprecated("Use `hasAssoc` instead")
let hasAssocU: (t<('a, 'c)>, 'b, ('a, 'b) => bool) => bool

/**
Returns `true` if there is a pair in `someList` where the first element equals `k` as per the predicate function `eqFunction`.

## Examples

```rescript
list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.hasAssoc(1, (a, b) => a == b) == true

list{(9, "morning"), (15, "afternoon"), (22, "night")}->Belt.List.hasAssoc(25, (k, item) =>
  k /* 25 */ == item
) == false
/* 9, 5, 22 */
```
*/
let hasAssoc: (t<('a, 'c)>, 'b, ('a, 'b) => bool) => bool

/** Uncurried version of [removeAssoc](#removeAssoc). */
@deprecated("Use `removeAssoc` instead")
let removeAssocU: (t<('a, 'c)>, 'b, ('a, 'b) => bool) => t<('a, 'c)>

/**
Return a list after removing the first pair whose first value is `k` per the equality predicate `eqFunction`; if not found, return a new list identical to `someList`.

## Examples

```rescript
list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.removeAssoc(1, (a, b) => a == b) ==
  list{(2, "b"), (3, "c")}

list{(9, "morning"), (15, "afternoon"), (22, "night")}->Belt.List.removeAssoc(9, (k, item) =>
  k /* 9 */ == item
) == list{(15, "afternoon"), (22, "night")}
/* 9, 5, 22 */
```
*/
let removeAssoc: (t<('a, 'c)>, 'b, ('a, 'b) => bool) => t<('a, 'c)>

/** Uncurried version of [setAssoc](#setAssoc). */
@deprecated("Use `setAssoc` instead")
let setAssocU: (t<('a, 'c)>, 'a, 'c, ('a, 'a) => bool) => t<('a, 'c)>

/**
If `k` exists in `someList` by satisfying the `eqFunction` predicate, return a new list with the key and value replaced by the new `k` and `v`; otherwise, return a new list with the pair `k`, `v` added to the head of `someList`.

## Examples

```rescript
list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.setAssoc(2, "x", (a, b) => a == b) ==
  list{(1, "a"), (2, "x"), (3, "c")}

list{(1, "a"), (3, "c")}->Belt.List.setAssoc(2, "b", (a, b) => a == b) ==
  list{(2, "b"), (1, "a"), (3, "c")}

list{(9, "morning"), (3, "morning?!"), (22, "night")}->Belt.List.setAssoc(15, "afternoon", (a, b) =>
  mod(a, 12) == mod(b, 12)
) == list{(9, "morning"), (15, "afternoon"), (22, "night")}
```

**Please note**

In the last example, since: `15 mod 12` equals `3 mod 12`

Both the key _and_ the value are replaced in the list.
*/
let setAssoc: (t<('a, 'c)>, 'a, 'c, ('a, 'a) => bool) => t<('a, 'c)>

/** Uncurried version of [sort](#sort). */
@deprecated("Use `sort` instead")
let sortU: (t<'a>, ('a, 'a) => int) => t<'a>

/**
Returns a sorted list.

## Examples

```rescript
Belt.List.sort(list{5, 4, 9, 3, 7}, (a, b) => a - b) == list{3, 4, 5, 7, 9}
```
*/
let sort: (t<'a>, ('a, 'a) => int) => t<'a>

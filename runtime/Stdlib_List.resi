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

/**
Collection functions for manipulating the `list` data structures, a singly-linked list.

**Prefer Array** if you need any of the following:

- Random access of element
- Better interop with JavaScript
- Better memory usage & performance.
*/
/**
Type representing a list of value `'a`.
*/
type t<'a> = list<'a>

/**
`length(list)` returns the length of `list`.

## Examples

```rescript
List.length(list{1, 2, 3}) // 3
```
*/
let length: list<'a> => int

/**
`size(list)`. See [`length`](#length)

## Examples

```rescript
List.size(list{1, 2, 3}) // 3
```
*/
let size: list<'a> => int

/**
`head(list)` returns `Some(value)` where `value` is the first element in the
list, or `None` if `list` is an empty list.

## Examples

```rescript
List.head(list{}) // None
List.head(list{1, 2, 3}) // Some(1)
```
*/
let head: list<'a> => option<'a>

/**
`headExn(list)` same as [`head`](#head).

## Examples

```rescript
List.headExn(list{1, 2, 3})->assertEqual(1)

switch List.headExn(list{}) {
| exception Not_found => assert(true)
| _ => assert(false)
}
```

## Exceptions

- Raises an Error if list is empty.

*/
let headExn: list<'a> => 'a

/**
`tail(list)` returns `None` if `list` is empty, otherwise it returns `Some(tail)`
where `tail` is everything except the first element of `list`.

## Examples

```rescript
List.tail(list{1, 2, 3}) // Some(list{2, 3})

List.tail(list{}) // None
```
*/
let tail: list<'a> => option<list<'a>>

/**
`tailExn(list)` same as [`tail`](#tail).

## Examples

```rescript
List.tailExn(list{1, 2, 3})->assertEqual(list{2, 3})

switch List.tailExn(list{}) {
| exception Not_found => assert(true)
| _ => assert(false)
}
```

## Exceptions

- Raises an Error if list is empty.
*/
let tailExn: list<'a> => list<'a>

/**
`add(list, value)` adds a `value` to the beginning of list `list`.

## Examples

```rescript
List.add(list{2, 3}, 1) // list{1, 2, 3}

List.add(list{"World", "!"}, "Hello") // list{"Hello", "World", "!"}
```
*/
let add: (list<'a>, 'a) => list<'a>

/**
`get(list, index)` return the `index` element in `list`, or `None` if `index`
is larger than the length of list `list`.

## Examples

```rescript
let abc = list{"A", "B", "C"}

abc->List.get(1) // Some("B")

abc->List.get(4) // None
```
*/
let get: (list<'a>, int) => option<'a>

/**
`getExn(list, index)` same as [`get`](#get).

## Examples

```rescript
let abc = list{"A", "B", "C"}

abc
->List.getExn(1)
->assertEqual("B")

switch abc->List.getExn(4) {
| exception Not_found => assert(true)
| _ => assert(false)
}
```

## Exceptions

- Raises an Error if `index` is larger than the length of list.
*/
let getExn: (list<'a>, int) => 'a

/**
`make(length, value)` returns a list of length `length` with each element filled
with `value`. Returns an empty list if `value` is negative.

## Examples

```rescript
List.make(~length=3, 1) // list{1, 1, 1}
```
*/
let make: (~length: int, 'a) => list<'a>

/**
`fromInitializer(length, f)` return a list of length `length` with element initialized
with `f`. Returns an empty list if `length` is negative.

## Examples

```rescript
List.fromInitializer(~length=5, i => i) // list{0, 1, 2, 3, 4}

List.fromInitializer(~length=5, i => i * i) // list{0, 1, 4, 9, 16}
```
*/
let fromInitializer: (~length: int, int => 'a) => list<'a>

/**
`shuffle(list)` returns a new list in random order.

## Examples

```rescript
List.shuffle(list{1, 2, 3}) // list{2, 1, 3}
```
*/
let shuffle: list<'a> => list<'a>

/**
`toShuffled(list)` returns a new list in random order.

## Examples

```rescript
List.toShuffled(list{1, 2, 3}) // list{2, 1, 3}
```
*/
@deprecated("Use `shuffle` instead")
let toShuffled: list<'a> => list<'a>

/**
`drop(list, value)` return a new list, dropping the first `value` element.
Returns `None` if `list` has fewer than `value` elements.

## Examples

```rescript
list{1, 2, 3}->List.drop(2) // Some(list{3})

list{1, 2, 3}->List.drop(3) // Some(list{})

list{1, 2, 3}->List.drop(4) // None
```
*/
let drop: (list<'a>, int) => option<list<'a>>

/**
`take(list, value)` returns a list with the first `value` elements from `list`,
or `None` if `list` has fewer than `value` elements.

## Examples

```rescript
list{1, 2, 3}->List.take(1) // Some(list{1})

list{1, 2, 3}->List.take(2) // Some(list{1, 2})

list{1, 2, 3}->List.take(4) // None
```
*/
let take: (list<'a>, int) => option<list<'a>>

/**
`splitAt(list, n)` split the list `list` at `n`. Returns `None` when the length
of `list` is less than `n`.

## Examples

```rescript
list{"Hello", "World"}->List.splitAt(1) // Some((list{"Hello"}, list{"World"}))

list{0, 1, 2, 3, 4}->List.splitAt(2) // Some((list{0, 1}, list{2, 3, 4}))
```
*/
let splitAt: (list<'a>, int) => option<(list<'a>, list<'a>)>

/**
`concat(list1, list2)` returns the list obtained by adding `list1` after `list2`.

## Examples

```rescript
List.concat(list{1, 2, 3}, list{4, 5}) // list{1, 2, 3, 4, 5}
```
*/
let concat: (list<'a>, list<'a>) => list<'a>

/**
`concatMany(arr)` returns the list obtained by concatenating all the lists in
array `arr`, in order.

## Examples

```rescript
List.concatMany([list{1, 2, 3}, list{}, list{3}]) // list{1, 2, 3, 3}
```
*/
let concatMany: array<list<'a>> => list<'a>

/**
`reverseConcat(list1, list2)` is equivalent to writing: `concat(reverse(list1, list2)`

## Examples

```rescript
List.reverseConcat(list{1, 2}, list{3, 4}) // list{2, 1, 3, 4}
```
*/
let reverseConcat: (list<'a>, list<'a>) => list<'a>

/**
`flat(list)` return the list obtained by concatenating all the lists in
`list`, in order.

## Examples

```rescript
List.flat(list{list{1, 2, 3}, list{}, list{3}}) // list{1, 2, 3, 3}
```
*/
let flat: list<list<'a>> => list<'a>

/**
`map(list, f)` returns a new list with `f` applied to each element of `list`.

## Examples

```rescript
list{1, 2}->List.map(x => x + 1) // list{3, 4}
```
*/
let map: (list<'a>, 'a => 'b) => list<'b>

/**
`zip(list1, list2)` returns a list of pairs from the two lists with the length
of the shorter list.

## Examples

```rescript
List.zip(list{1, 2}, list{3, 4, 5}) // list{(1, 3), (2, 4)}
```
*/
let zip: (list<'a>, list<'b>) => list<('a, 'b)>

/**
`zipBy(list1, list2, f)`. See [`zip`](#zip)

## Examples

```rescript
List.zipBy(list{1, 2, 3}, list{4, 5}, (a, b) => 2 * a + b) // list{6, 9}
```
*/
let zipBy: (list<'a>, list<'b>, ('a, 'b) => 'c) => list<'c>

/**
`mapWithIndex(list, f)` applies `f` to each element of `list`. Function `f`
takes two arguments: the index starting from 0 and the element from `list`, in
that order.

## Examples

```rescript
list{1, 2, 3}->List.mapWithIndex((x, index) => index + x) // list{1, 3, 5}
```
*/
let mapWithIndex: (list<'a>, ('a, int) => 'b) => list<'b>

/**
`fromArray(arr)` converts the given array `arr` to a list.

## Examples

```rescript
List.fromArray([1, 2, 3]) // list{1, 2, 3}
```
*/
let fromArray: array<'a> => list<'a>

/**
`toArray(list)` converts the given list `list` to an array.

## Examples

```rescript
List.toArray(list{1, 2, 3}) // [1, 2, 3]
```
*/
let toArray: list<'a> => array<'a>

/**
`reverse(list)` returns a new list whose elements are those of `list` in
reversed order.

## Examples

```rescript
List.reverse(list{1, 2, 3}) // list{3, 2, 1}
```
*/
let reverse: list<'a> => list<'a>

/**
`mapReverse(list, f)` is equivalent to `map` function.

## Examples

```rescript
let f = x => x * x
let l = list{3, 4, 5}

let withMap = List.map(l, f)->List.reverse
let withMapReverse = l->List.mapReverse(f)

Console.log(withMap == withMapReverse) // true
```
*/
let mapReverse: (list<'a>, 'a => 'b) => list<'b>

/**
`forEach(list, f)` call `f` on each element of `list` from the beginning to end.
`f` returns `unit`, so no new array is created. Use `forEach` when you are primarily
concerned with repetitively creating side effects.

## Examples

```rescript
List.forEach(list{"a", "b", "c"}, x => Console.log("Item: " ++ x))
/*
  prints:
  Item: a
  Item: b
  Item: c
*/
```
*/
let forEach: (list<'a>, 'a => unit) => unit

/**
`forEachWithIndex(list, f, index)` call `f` on each element of `list` from beginning
to end. Function `f` takes two arguments: the `index` starting from 0 and the
element from `list`. `f` returns `unit`.

## Examples

```rescript
List.forEachWithIndex(list{"a", "b", "c"}, (x, index) => {
  Console.log("Item " ++ Int.toString(index) ++ " is " ++ x)
})
/*
  prints:
  Item 0 is a
  Item 1 is b
  Item 2 is cc
*/
```
*/
let forEachWithIndex: (list<'a>, ('a, int) => unit) => unit

/**
`reduce(list, initialValue, f)` applies `f` to each element of `list` from
beginning to end. Function `f` has two parameters: the item from the list and
an "accumulator", which starts with a value of `initialValue`. `reduce` returns
the final value of the accumulator.

## Examples

```rescript
list{1, 2, 3, 4}->List.reduce(0, (a, b) => a + b) // 10

// same as

list{1, 2, 3, 4}->List.reduce(0, (acc, item) => acc + item) // 10
```
*/
let reduce: (list<'a>, 'b, ('b, 'a) => 'b) => 'b

/**
`reduceWithIndex(list, initialValue, f)` applies `f` to each element of `list`
from beginning to end. Function `f` has three parameters: the item from the list
and an "accumulator", which starts with a value of `initialValue` and the index
of each element. `reduceWithIndex` returns the final value of the accumulator.

## Examples

```rescript
list{1, 2, 3, 4}->List.reduceWithIndex(0, (acc, item, index) => acc + item + index) // 16
```
*/
let reduceWithIndex: (list<'a>, 'b, ('b, 'a, int) => 'b) => 'b

/**
`reduceReverse(list, initialValue, f)` works like `reduce`, except that
function `f` is applied to each item of `list` from the last back to the first.

## Examples

```rescript
list{1, 2, 3, 4}->List.reduceReverse(0, (a, b) => a + b) // 10

list{1, 2, 3, 4}->List.reduceReverse(10, (a, b) => a - b) // 0

list{1, 2, 3, 4}->List.reduceReverse(list{}, List.add) // list{1, 2, 3, 4}
```
*/
let reduceReverse: (list<'a>, 'b, ('b, 'a) => 'b) => 'b

/**
`mapReverse2(list1, list2, f)` is equivalent to `List.zipBy(list1, list2, f)->List.reverse`.

## Examples

```rescript
List.mapReverse2(list{1, 2, 3}, list{1, 2}, (a, b) => a + b) // list{4, 2}
```
*/
let mapReverse2: (list<'a>, list<'b>, ('a, 'b) => 'c) => list<'c>

/**
`forEach2(list1, list2, f)` is similar to `forEach`, but accepts two lists and
stops at the length of the shorter list.

## Examples

```rescript
List.forEach2(list{"Z", "Y"}, list{"A", "B", "C"}, (x, y) => Console.log2(x, y))

/*
  prints:
  "Z" "A"
  "Y" "B"
*/
```
*/
let forEach2: (list<'a>, list<'b>, ('a, 'b) => 'c) => unit

/**
`reduce2(list1, list2, initialValue, f)` applies `f` to each element of `list1`
and `list2` from beginning to end. Stops with the shorter list. Function `f` has
three parameters: an accumulator which starts with a value of `initialValue`, an
item from `l1`, and an item from `l2`. `reduce2` returns the final value of the
accumulator.

## Examples

```rescript
List.reduce2(list{1, 2, 3}, list{4, 5}, 0, (acc, x, y) => acc + x * x + y) // 0 + (1 * 1 + 4) + (2 * 2 + 5)
```
*/
let reduce2: (list<'b>, list<'c>, 'a, ('a, 'b, 'c) => 'a) => 'a

/**
`reduceReverse2(list1, list2, initialValue, f)` applies `f` to each element of
`list1` and `list2`from end to beginning. Stops with the shorter list. Function
`f` has three parameters: an accumulator which starts with a value of
`initialValue`, an item from `l1`, and an item from `l2`. `reduce2` returns the
final value of the accumulator.

## Examples

```rescript
List.reduceReverse2(list{1, 2, 3}, list{4, 5}, 0, (acc, x, y) => acc + x * x + y) //  + (1 * 1 + 4) + (2 * 2 + 5)
```
*/
let reduceReverse2: (list<'a>, list<'b>, 'c, ('c, 'a, 'b) => 'c) => 'c

/**
`every(list, f)` returns `true` if all elements in `list` satisfy `f`, where `f`
is a predicate: a function taking an element and returning a bool.

## Examples

```rescript
let isBelow10 = value => value < 10

list{1, 9, 8, 2}->List.every(isBelow10) // true

list{1, 99, 8, 2}->List.every(isBelow10) // false
```
*/
let every: (list<'a>, 'a => bool) => bool

/**
`some(list, f)` returns `true` if at least _one_ of the elements in `list`
satisfies `f`, where `f` is a predicate: a function taking an element and
returning a bool.

## Examples

```rescript
let isAbove100 = value => value > 100

list{101, 1, 2, 3}->List.some(isAbove100) // true

list{1, 2, 3, 4}->List.some(isAbove100) // false
```
*/
let some: (list<'a>, 'a => bool) => bool

/**
`every2(list1, list2, f)` returns `true` if predicate `f` is `true` for all
pairs of elements up to the shorter length (i.e. `min(length(list1), length(list2))`)

## Examples

```rescript
List.every2(list{1, 2, 3}, list{0, 1}, (a, b) => a > b) // true

List.every2(list{}, list{1}, (a, b) => a > b) // true

List.every2(list{2, 3}, list{1}, (a, b) => a > b) // true

List.every2(list{0, 1}, list{5, 0}, (a, b) => a > b) // false
```
*/
let every2: (list<'a>, list<'b>, ('a, 'b) => bool) => bool

/**
`some2(list1, list2, f)` returns `true` if predicate `f` is `true` for any pair
of elements up to the shorter length (i.e. `min(length(list1), length(list2))`)

## Examples

```rescript
List.some2(list{1, 2, 3}, list{0, 1}, (a, b) => a > b) // true

List.some2(list{}, list{1}, (a, b) => a > b) // false

List.some2(list{2, 3}, list{1}, (a, b) => a > b) // true

List.some2(list{0, 1}, list{5, 0}, (a, b) => a > b) // true
```
*/
let some2: (list<'a>, list<'b>, ('a, 'b) => bool) => bool

/**
`compareLength(list1, list2)` compare two lists solely by length. Returns `-1.` if
`length(list1)` is less than `length(list2)`, `0.` if `length(list1)` equals
`length(list2)`, and `1.` if `length(list1)` is greater than `length(list2)`.

## Examples

```rescript
List.compareLength(list{1, 2}, list{3, 4, 5, 6}) // -1.

List.compareLength(list{1, 2, 3}, list{4, 5, 6}) // 0.

List.compareLength(list{1, 2, 3, 4}, list{5, 6}) // 1.
```
*/
let compareLength: (list<'a>, list<'a>) => Stdlib_Ordering.t

/**
`compare(list1, list2, f)` compare elements one by one `f`. `f` returns a negative
number if `list1` is "less than" `list2`, zero if `list1` is "equal to" `list2`,
a positive number if `list1` is "greater than" `list2`.

The comparison returns the first non-zero result of `f`, or zero if `f` returns
zero for all `list1` and `list2`.

- If all items have compared equal, but `list1` is exhausted first, return `-1.`. (`list1` is shorter).
- If all items have compared equal, but `list2` is exhausted first, return `1.` (`list1` is longer).

## Examples

```rescript
List.compare(list{3}, list{3, 7}, (a, b) => Int.compare(a, b)) // -1.
List.compare(list{5, 3}, list{5}, (a, b) => Int.compare(a, b)) // 1.
List.compare(list{1, 3, 5}, list{1, 4, 2}, (a, b) => Int.compare(a, b)) // -1.
List.compare(list{1, 3, 5}, list{1, 2, 3}, (a, b) => Int.compare(a, b)) // 1.
List.compare(list{1, 3, 5}, list{1, 3, 5}, (a, b) => Int.compare(a, b)) // 0.
```

**Please note:** The total ordering of List is different from Array,
for Array, we compare the length first and, only if the lengths are equal, elements one by one.
For lists, we just compare elements one by one.
*/
let compare: (list<'a>, list<'a>, ('a, 'a) => Stdlib_Ordering.t) => Stdlib_Ordering.t

/**
`equal(list1, list2, f)` check equality of `list2` and `list2` using `f` for
equality on elements, where `f` is a function that returns `true` if items `x` and
`y` meet some criterion for equality, `false` otherwise. equal `false` if length
of `list1` and `list2` are not the same.

## Examples

```rescript
List.equal(list{1, 2, 3}, list{1, 2}, (a, b) => a == b) // false

List.equal(list{1, 2}, list{1, 2}, (a, b) => a == b) // true

List.equal(list{1, 2, 3}, list{(-1), (-2), (-3)}, (a, b) => abs(a) == abs(b)) // true
```
*/
let equal: (list<'a>, list<'a>, ('a, 'a) => bool) => bool

/**
`has(list, element, f)` returns `true` if the list contains at least one
`element` for which `f` returns `true'.

## Examples

```rescript
list{1, 2, 3}->List.has(2, (a, b) => a == b) // true

list{1, 2, 3}->List.has(4, (a, b) => a == b) // false

list{(-1), (-2), (-3)}->List.has(2, (a, b) => abs(a) == abs(b)) // true
```
*/
let has: (list<'a>, 'b, ('a, 'b) => bool) => bool

/**
`find(list, f)` returns `Some(value)` for the first value in `list` that
satisfies the predicate function `f`. Returns `None` if no element satisfies
the function.

## Examples

```rescript
List.find(list{1, 4, 3, 2}, x => x > 3) // Some(4)

List.find(list{1, 4, 3, 2}, x => x > 4) // None
```
*/
let find: (list<'a>, 'a => bool) => option<'a>

/**
`filter(list, f)` returns a list of all elements in `list` which satisfy the
predicate function `f`.

## Examples

```rescript
let isEven = x => mod(x, 2) == 0

List.filter(list{1, 2, 3, 4}, isEven) // list{2, 4}

List.filter(list{None, Some(2), Some(3), None}, Option.isSome) // list{Some(2), Some(3)}
```
*/
let filter: (list<'a>, 'a => bool) => list<'a>

/**
`filterWithIndex(list, f)` returns a list of all elements in `list` which
satisfy the predicate function `f`.

## Examples

```rescript
let isEven = x => mod(x, 2) == 0

List.filterWithIndex(list{1, 2, 3, 4}, (_x, index) => isEven(index)) // list{1, 3}
```
*/
let filterWithIndex: (list<'a>, ('a, int) => bool) => list<'a>

/**
`filterMap(list, f)` applies `f` to each element of `list`. If `f` returns
`Some(value)`, then `value` is _kept_ in the resulting list. If `f` returns
`None`, the element is _not_ retained in the result.

## Examples

```rescript
let isEven = x => mod(x, 2) == 0

list{1, 2, 3, 4}
->List.filterMap(x =>
    if (isEven(x)) {
      Some(x)
    } else {
      None
    }
  ) // list{2, 4}

list{Some(1), Some(2), None}->List.filterMap(x => x) // list{1, 2}
```
*/
let filterMap: (list<'a>, 'a => option<'b>) => list<'b>

/**
`partition(list, f)` creates a pair of lists; the first list consists of all
elements of `list` that satisfy the predicate function `f`, the second list
consists of all elements of `list` that _do not_ satisfy `f`.

## Examples

```rescript
// (elementsThatSatisfies, elementsThatDoesNotSatisfy)

List.partition(list{1, 2, 3, 4}, x => x > 2) // (list{3, 4}, list{1, 2})
```
*/
let partition: (list<'a>, 'a => bool) => (list<'a>, list<'a>)

/**
`unzip(list)` takes a list of pairs and creates a pair of lists. The first list
contains all the first items of the pairs, the second list contains all the
second items.

## Examples

```rescript
List.unzip(list{(1, 2), (3, 4)}) // (list{1, 3}, list{2, 4})

List.unzip(list{("H", "W"), ("e", "o"), ("l", "r"), ("l", "l"), ("o", "d"), (" ", "!")})
// (list{"H", "e", "l", "l", "o", " "}, list{"W", "o", "r", "l", "d", "!"})
```
*/
let unzip: list<('a, 'b)> => (list<'a>, list<'b>)

/**
`getAssoc(list, k, f)` return the second element of a pair in `list` where
the first element equals `k` as per the predicate function `f`, or `None` if
not found.

## Examples

```rescript
list{(1, "a"), (2, "b"), (3, "c")}->List.getAssoc(3, (a, b) => a == b) // Some("c")

list{(9, "morning"), (15, "afternoon"), (22, "night")}
->List.getAssoc(15, (k, item) => k /* 15 */ == item /* 9, 5, 22 */)
// Some("afternoon")
```
*/
@deprecated("Use a `Map` instead")
let getAssoc: (list<('a, 'c)>, 'b, ('a, 'b) => bool) => option<'c>

/**
`hasAssoc(list, k, f)` returns `true` if there is a pair in `list` where the
first element equals `k` as per the predicate function `f`.

## Examples

```rescript
list{(1, "a"), (2, "b"), (3, "c")}->List.hasAssoc(1, (a, b) => a == b) // true

list{(9, "morning"), (15, "afternoon"), (22, "night")}
->List.hasAssoc(25, (k, item) => k /* 25 */ == item /* 9, 5, 22 */) // false
```
*/
@deprecated("Use a `Map` instead")
let hasAssoc: (list<('a, 'c)>, 'b, ('a, 'b) => bool) => bool

/**
`removeAssoc(list, k, f)` return a list after removing the first pair whose
first value is `k` per the equality predicate `f`, if not found, return a new
list identical to `list`.

## Examples

```rescript
list{(1, "a"), (2, "b"), (3, "c")}->List.removeAssoc(1, (a, b) => a == b) // list{(2, "b"), (3, "c")}

list{(9, "morning"), (15, "afternoon"), (22, "night")}
->List.removeAssoc(9, (k, item) => k /* 9 */ == item /* 9, 5, 22 */)
// list{(15, "afternoon"), (22, "night")}
```
*/
@deprecated("Use a `Map` instead")
let removeAssoc: (list<('a, 'c)>, 'b, ('a, 'b) => bool) => list<('a, 'c)>

/**
`setAssoc(list, k, v, f)`. If `k` exists in `list` by satisfying the `f`
predicate, return a new list with the key and value replaced by the new `k` and
`v`, otherwise, return a new list with the pair `k`, `v` added to the head of
`list`.

## Examples

```rescript
list{(1, "a"), (2, "b"), (3, "c")}->List.setAssoc(2, "x", (a, b) => a == b) // list{(1, "a"), (2, "x"), (3, "c")}

list{(1, "a"), (3, "c")}->List.setAssoc(2, "b", (a, b) => a == b) // list{(2, "b"), (1, "a"), (3, "c")}

list{(9, "morning"), (3, "morning?!"), (22, "night")}
->List.setAssoc(15, "afternoon", (a, b) => mod(a, 12) == mod(b, 12))
// list{(9, "morning"), (15, "afternoon"), (22, "night")}
```

**Please note**: In the last example, since: `15 mod 12` equals `3 mod 12`. Both
the key _and_ the value are replaced in the list.
*/
@deprecated("Use a `Map` instead")
let setAssoc: (list<('a, 'c)>, 'a, 'c, ('a, 'a) => bool) => list<('a, 'c)>

/**
`sort(list, f)` returns a sorted list.

## Examples

```rescript
List.sort(list{5, 4, 9, 3, 7}, Int.compare) // list{3, 4, 5, 7, 9}
```
*/
let sort: (list<'a>, ('a, 'a) => Stdlib_Ordering.t) => list<'a>

/**
  `ignore(list)` ignores the provided list and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: list<'a> => unit = "%ignore"

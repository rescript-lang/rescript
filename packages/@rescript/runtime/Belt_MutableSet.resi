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
A **mutable** sorted set module which allows customized compare behavior.
The implementation uses balanced binary trees, and therefore searching and insertion take time logarithmic in the size of the map.

It also has two specialized inner modules [Belt.MutableSet.Int](mutable-set-int) and [Belt.MutableSet.String](mutable-set-string) - This module separates data from function which is more verbose but slightly more efficient

## Examples

```rescript
module PairComparator = Belt.Id.MakeComparable({
  type t = (int, int)
  let cmp = ((a0, a1), (b0, b1)) =>
    switch Pervasives.compare(a0, b0) {
    | 0 => Pervasives.compare(a1, b1)
    | c => c
    }
})

let mySet = Belt.MutableSet.make(~id=module(PairComparator))
mySet->Belt.MutableSet.add((1, 2))
```
*/

/** Specialized when key type is `int`, more efficient than the generic type */
module Int = Belt_MutableSetInt

/** Specialized when key type is `string`, more efficient than the generic type */
module String = Belt_MutableSetString

/**
`'value` is the element type

`'identity` the identity of the collection
*/
type t<'value, 'identity>

/**
The identity needed for making a set from scratch
*/
type id<'value, 'id> = Belt_Id.comparable<'value, 'id>

/**
Creates a new set by taking in the comparator
*/
let make: (~id: id<'value, 'id>) => t<'value, 'id>

/**
Creates new set from array of elements.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([1, 3, 2, 4], ~id=module(IntCmp))

s0->Belt.MutableSet.toArray == [1, 2, 3, 4]
```
*/
let fromArray: (array<'value>, ~id: id<'value, 'id>) => t<'value, 'id>

/**
The same as [fromArray][#fromarray] except it is after assuming the input array is already sorted.
*/
let fromSortedArrayUnsafe: (array<'value>, ~id: id<'value, 'id>) => t<'value, 'id>

/**
Returns copy of a set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([1, 3, 2, 4], ~id=module(IntCmp))

let copied = s0->Belt.MutableSet.copy
copied->Belt.MutableSet.toArray == [1, 2, 3, 4]
```
*/
let copy: t<'value, 'id> => t<'value, 'id>

/**
Checks if set is empty.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let empty = Belt.MutableSet.fromArray([], ~id=module(IntCmp))
let notEmpty = Belt.MutableSet.fromArray([1], ~id=module(IntCmp))

Belt.MutableSet.isEmpty(empty) == true
Belt.MutableSet.isEmpty(notEmpty) == false
```
*/
let isEmpty: t<_> => bool

/**
Checks if element exists in set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let set = Belt.MutableSet.fromArray([1, 4, 2, 5], ~id=module(IntCmp))

set->Belt.MutableSet.has(3) == false
set->Belt.MutableSet.has(1) == true
```
*/
let has: (t<'value, 'id>, 'value) => bool

/**
Adds element to set. If element existed in set, value is unchanged.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.make(~id=module(IntCmp))
s0->Belt.MutableSet.add(1)
s0->Belt.MutableSet.add(2)
s0->Belt.MutableSet.add(2)

s0->Belt.MutableSet.toArray == [1, 2]
```
*/
let add: (t<'value, 'id>, 'value) => unit

let addCheck: (t<'value, 'id>, 'value) => bool

/**
Adds each element of array to set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let set = Belt.MutableSet.make(~id=module(IntCmp))

set->Belt.MutableSet.mergeMany([5, 4, 3, 2, 1])
set->Belt.MutableSet.toArray == [1, 2, 3, 4, 5]
```
*/
let mergeMany: (t<'value, 'id>, array<'value>) => unit

/**
Removes element from set. If element did not exist in set, value is unchanged.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([2, 3, 1, 4, 5], ~id=module(IntCmp))
s0->Belt.MutableSet.remove(1)
s0->Belt.MutableSet.remove(3)
s0->Belt.MutableSet.remove(3)

s0->Belt.MutableSet.toArray == [2, 4, 5]
```
*/
let remove: (t<'value, 'id>, 'value) => unit

let removeCheck: (t<'value, 'id>, 'value) => bool
/* `b = removeCheck s e` `b` is true means one element removed */

/**
Removes each element of array from set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let set = Belt.MutableSet.fromArray([1, 2, 3, 4], ~id=module(IntCmp))

set->Belt.MutableSet.removeMany([5, 4, 3, 2, 1])
set->Belt.MutableSet.toArray == []
```
*/
let removeMany: (t<'value, 'id>, array<'value>) => unit

/**
Returns union of two sets.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
let union = Belt.MutableSet.union(s0, s1)
union->Belt.MutableSet.toArray == [1, 2, 3, 4, 5, 6]
```
*/
let union: (t<'value, 'id>, t<'value, 'id>) => t<'value, 'id>

/**
Returns intersection of two sets.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
let intersect = Belt.MutableSet.intersect(s0, s1)
intersect->Belt.MutableSet.toArray == [2, 3, 5]
```
*/
let intersect: (t<'value, 'id>, t<'value, 'id>) => t<'value, 'id>

/**
Returns elements from first set, not existing in second set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
Belt.MutableSet.toArray(Belt.MutableSet.diff(s0, s1)) == [6]
Belt.MutableSet.toArray(Belt.MutableSet.diff(s1, s0)) == [1, 4]
```
*/
let diff: (t<'value, 'id>, t<'value, 'id>) => t<'value, 'id>

/**
Checks if second set is subset of first set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
let s2 = Belt.MutableSet.intersect(s0, s1)
Belt.MutableSet.subset(s2, s0) == true
Belt.MutableSet.subset(s2, s1) == true
Belt.MutableSet.subset(s1, s0) == false
```
*/
let subset: (t<'value, 'id>, t<'value, 'id>) => bool

/**
Total ordering between sets. Can be used as the ordering function for doing sets of sets.
It compares size first and then iterates over each element following the order of elements.
*/
let cmp: (t<'value, 'id>, t<'value, 'id>) => int

/**
Checks if two sets are equal.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([5, 2, 3], ~id=module(IntCmp))
let s1 = Belt.MutableSet.fromArray([3, 2, 5], ~id=module(IntCmp))

Belt.MutableSet.eq(s0, s1) == true
```
*/
let eq: (t<'value, 'id>, t<'value, 'id>) => bool

/**
  Same as `Belt.MutableSet.forEach` but takes uncurried functon.
*/
@deprecated("Use `forEach` instead")
let forEachU: (t<'value, 'id>, 'value => unit) => unit

/**
Applies function `f` in turn to all elements of set in increasing order.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
let acc = ref(list{})
s0->Belt.MutableSet.forEach(x => acc := Belt.List.add(acc.contents, x))
acc.contents == list{6, 5, 3, 2}
```
*/
let forEach: (t<'value, 'id>, 'value => unit) => unit

@deprecated("Use `reduce` instead")
let reduceU: (t<'value, 'id>, 'a, ('a, 'value) => 'a) => 'a

/**
Applies function `f` to each element of set in increasing order. Function `f` has two parameters: the item from the set and an “accumulator”, which starts with a value of `initialValue`. `reduce` returns the final value of the accumulator.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
s0->Belt.MutableSet.reduce(list{}, (acc, element) => acc->Belt.List.add(element)) ==
  list{6, 5, 3, 2}
```
*/
let reduce: (t<'value, 'id>, 'a, ('a, 'value) => 'a) => 'a

@deprecated("Use `every` instead")
let everyU: (t<'value, 'id>, 'value => bool) => bool

/**
Checks if all elements of the set satisfy the predicate. Order unspecified.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let isEven = x => mod(x, 2) == 0

let s0 = Belt.MutableSet.fromArray([2, 4, 6, 8], ~id=module(IntCmp))
s0->Belt.MutableSet.every(isEven) == true
```
*/
let every: (t<'value, 'id>, 'value => bool) => bool

@deprecated("Use `some` instead")
let someU: (t<'value, 'id>, 'value => bool) => bool

/**
Checks if at least one element of the set satisfies the predicate.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let isOdd = x => mod(x, 2) != 0

let s0 = Belt.MutableSet.fromArray([1, 2, 4, 6, 8], ~id=module(IntCmp))
s0->Belt.MutableSet.some(isOdd) == true
```
*/
let some: (t<'value, 'id>, 'value => bool) => bool

@deprecated("Use `keep` instead")
let keepU: (t<'value, 'id>, 'value => bool) => t<'value, 'id>

/**
Returns the set of all elements that satisfy the predicate.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let isEven = x => mod(x, 2) == 0

let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))
let s1 = s0->Belt.MutableSet.keep(isEven)

s1->Belt.MutableSet.toArray == [2, 4]
```
*/
let keep: (t<'value, 'id>, 'value => bool) => t<'value, 'id>

@deprecated("Use `partition` instead")
let partitionU: (t<'value, 'id>, 'value => bool) => (t<'value, 'id>, t<'value, 'id>)

/**
## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let isOdd = x => mod(x, 2) != 0

let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))
let (s1, s2) = s0->Belt.MutableSet.partition(isOdd)

s1->Belt.MutableSet.toArray == [1, 3, 5]
s2->Belt.MutableSet.toArray == [2, 4]
```
*/
let partition: (t<'value, 'id>, 'value => bool) => (t<'value, 'id>, t<'value, 'id>)

/**
Returns size of the set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4], ~id=module(IntCmp))

s0->Belt.MutableSet.size == 4
```
*/
let size: t<'value, 'id> => int

/**
Returns list of ordered set elements.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

s0->Belt.MutableSet.toList == list{1, 2, 3, 5}
```
*/
let toList: t<'value, 'id> => list<'value>

/**
Returns array of ordered set elements.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

s0->Belt.MutableSet.toArray == [1, 2, 3, 5]
```
*/
let toArray: t<'value, 'id> => array<'value>

/**
Returns minimum value of the collection. `None` if collection is empty.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.make(~id=module(IntCmp))
let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

s0->Belt.MutableSet.minimum == None
s1->Belt.MutableSet.minimum == Some(1)
```
*/
let minimum: t<'value, 'id> => option<'value>

/**
Returns minimum value of the collection. `undefined` if collection is empty.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.make(~id=module(IntCmp))
let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

s0->Belt.MutableSet.minUndefined == Js.undefined
s1->Belt.MutableSet.minUndefined == Js.Undefined.return(1)
```
*/
let minUndefined: t<'value, 'id> => Js.undefined<'value>

/**
Returns maximum value of the collection. `None` if collection is empty.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.make(~id=module(IntCmp))
let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

s0->Belt.MutableSet.maximum == None
s1->Belt.MutableSet.maximum == Some(5)
```
*/
let maximum: t<'value, 'id> => option<'value>

/**
Returns maximum value of the collection. `undefined` if collection is empty.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.make(~id=module(IntCmp))
let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

s0->Belt.MutableSet.maxUndefined == Js.undefined
s1->Belt.MutableSet.maxUndefined == Js.Undefined.return(5)
```
*/
let maxUndefined: t<'value, 'id> => Js.undefined<'value>

/**
Returns the reference of the value which is equivalent to value using the comparator specifiecd by this collection. Returns `None` if element does not exist.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))

s0->Belt.MutableSet.get(3) == Some(3)
s0->Belt.MutableSet.get(20) == None
```
*/
let get: (t<'value, 'id>, 'value) => option<'value>

/**
Same as `Belt.MutableSet.get` but returns `undefined` when element does not exist.
*/
let getUndefined: (t<'value, 'id>, 'value) => Js.undefined<'value>

/**
Same as `Belt.MutableSet.get` but throw when element does not exist.
*/
let getExn: (t<'value, 'id>, 'value) => 'value

/**
Same as `Belt.MutableSet.get` but throw when element does not exist.
*/
let getOrThrow: (t<'value, 'id>, 'value) => 'value

/**
Returns a tuple `((smaller, larger), present)`, `present` is true when element exist in set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))

let ((smaller, larger), present) = s0->Belt.MutableSet.split(3)

present == true
smaller->Belt.MutableSet.toArray == [1, 2]
larger->Belt.MutableSet.toArray == [4, 5]
```
*/
let split: (t<'value, 'id>, 'value) => ((t<'value, 'id>, t<'value, 'id>), bool)

/**
**throw** when invariant is not held
*/
let checkInvariantInternal: t<_> => unit

/*
  `add0` was not exposed for various reasons:
  1. such api is dangerious
  [ cmp: ('value,'id) Belt_Cmp.cmp ->
    ('value, 'id) t0 -> 'value ->
    ('value, 'id) t0]
  2. It is not really significantly more */

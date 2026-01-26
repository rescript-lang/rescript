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
This module separates identity from data. It is a bit more verbose but slightly
more efficient due to the fact that there is no need to pack identity and data
back after each operation.
*/

/**
`'value` is the element type

`'identity` the identity of the collection
*/
type t<'value, 'identity>

/**
Type of compare function.
*/
type cmp<'value, 'id> = Belt_Id.cmp<'value, 'id>

/**
## Examples

```rescript
let s0 = Belt.Set.Dict.empty
```
*/
let empty: t<'value, 'id>

/**
Creates new set from array of elements.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.Set.Dict.fromArray([1, 3, 2, 4], ~cmp=IntCmp.cmp)

s0->Belt.Set.Dict.toArray == [1, 2, 3, 4]
```
*/
let fromArray: (array<'value>, ~cmp: cmp<'value, 'id>) => t<'value, 'id>

/**
The same as [fromArray][#fromarray] except it is after assuming the input array
is already sorted.
*/
let fromSortedArrayUnsafe: array<'value> => t<'value, 'id>

/**
Checks if set is empty.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let empty = Belt.Set.Dict.fromArray([], ~cmp=IntCmp.cmp)
let notEmpty = Belt.Set.Dict.fromArray([1], ~cmp=IntCmp.cmp)

Belt.Set.Dict.isEmpty(empty) == true
Belt.Set.Dict.isEmpty(notEmpty) == false
```
*/
let isEmpty: t<_> => bool

/**
Checks if an element exists in the set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let set = Belt.Set.Dict.fromArray([1, 4, 2, 5], ~cmp=IntCmp.cmp)

set->Belt.Set.Dict.has(3, ~cmp=IntCmp.cmp) == false
set->Belt.Set.Dict.has(1, ~cmp=IntCmp.cmp) == true
```
*/
let has: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => bool

/**
Adds element to set. If element existed in set, value is unchanged.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.Set.Dict.empty
let s1 = s0->Belt.Set.Dict.add(1, ~cmp=IntCmp.cmp)
let s2 = s1->Belt.Set.Dict.add(2, ~cmp=IntCmp.cmp)
let s3 = s2->Belt.Set.Dict.add(2, ~cmp=IntCmp.cmp)
s0->Belt.Set.Dict.toArray == []
s1->Belt.Set.Dict.toArray == [1]
s2->Belt.Set.Dict.toArray == [1, 2]
s3->Belt.Set.Dict.toArray == [1, 2]
s2 == s3
```
*/
let add: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => t<'value, 'id>

/**
Adds each element of array to set. Unlike [add](#add), the reference of return value might be changed even if all values in array already exist in set

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let set = Belt.Set.Dict.empty

let newSet = set->Belt.Set.Dict.mergeMany([5, 4, 3, 2, 1], ~cmp=IntCmp.cmp)
newSet->Belt.Set.Dict.toArray == [1, 2, 3, 4, 5]
```
*/
let mergeMany: (t<'value, 'id>, array<'value>, ~cmp: cmp<'value, 'id>) => t<'value, 'id>

/**
Removes element from set. If element did not exist in set, value is unchanged.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.Set.Dict.fromArray([2, 3, 1, 4, 5], ~cmp=IntCmp.cmp)
let s1 = s0->Belt.Set.Dict.remove(1, ~cmp=IntCmp.cmp)
let s2 = s1->Belt.Set.Dict.remove(3, ~cmp=IntCmp.cmp)
let s3 = s2->Belt.Set.Dict.remove(3, ~cmp=IntCmp.cmp)

s1->Belt.Set.Dict.toArray == [2, 3, 4, 5]
s2->Belt.Set.Dict.toArray == [2, 4, 5]
s2 == s3
```
*/
let remove: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => t<'value, 'id>

/**
Removes each element of array from set. Unlike [remove](#remove), the reference of return value might be changed even if any values in array not existed in set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let set = Belt.Set.Dict.fromArray([1, 2, 3, 4], ~cmp=IntCmp.cmp)

let newSet = set->Belt.Set.Dict.removeMany([5, 4, 3, 2, 1], ~cmp=IntCmp.cmp)
newSet->Belt.Set.Dict.toArray == []
```
*/
let removeMany: (t<'value, 'id>, array<'value>, ~cmp: cmp<'value, 'id>) => t<'value, 'id>

/**
Returns union of two sets.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
let union = Belt.Set.Dict.union(s0, s1, ~cmp=IntCmp.cmp)
union->Belt.Set.Dict.toArray == [1, 2, 3, 4, 5, 6]
```
*/
let union: (t<'value, 'id>, t<'value, 'id>, ~cmp: cmp<'value, 'id>) => t<'value, 'id>

/**
Returns intersection of two sets.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
let intersect = Belt.Set.Dict.intersect(s0, s1, ~cmp=IntCmp.cmp)
intersect->Belt.Set.Dict.toArray == [2, 3, 5]
```
*/
let intersect: (t<'value, 'id>, t<'value, 'id>, ~cmp: cmp<'value, 'id>) => t<'value, 'id>

/**
Returns elements from first set, not existing in second set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)

let diff1 = Belt.Set.Dict.diff(s0, s1, ~cmp=IntCmp.cmp)
let diff2 = Belt.Set.Dict.diff(s1, s0, ~cmp=IntCmp.cmp)

diff1->Belt.Set.Dict.toArray == [6]
diff2->Belt.Set.Dict.toArray == [1, 4]
```
*/
let diff: (t<'value, 'id>, t<'value, 'id>, ~cmp: cmp<'value, 'id>) => t<'value, 'id>

/**
Checks if second set is subset of first set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
let s2 = Belt.Set.Dict.intersect(s0, s1, ~cmp=IntCmp.cmp)
Belt.Set.Dict.subset(s2, s0, ~cmp=IntCmp.cmp) == true
Belt.Set.Dict.subset(s2, s1, ~cmp=IntCmp.cmp) == true
Belt.Set.Dict.subset(s1, s0, ~cmp=IntCmp.cmp) == false
```
*/
let subset: (t<'value, 'id>, t<'value, 'id>, ~cmp: cmp<'value, 'id>) => bool

/**
Total ordering between sets. Can be used as the ordering function for doing sets
of sets. It compares size first and then iterates over each element following the
order of elements.
*/
let cmp: (t<'value, 'id>, t<'value, 'id>, ~cmp: cmp<'value, 'id>) => int

/**
Checks if two sets are equal.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.Set.Dict.fromArray([5, 2, 3], ~cmp=IntCmp.cmp)
let s1 = Belt.Set.Dict.fromArray([3, 2, 5], ~cmp=IntCmp.cmp)

Belt.Set.Dict.eq(s0, s1, ~cmp=IntCmp.cmp) == true
```
*/
let eq: (t<'value, 'id>, t<'value, 'id>, ~cmp: cmp<'value, 'id>) => bool

/**
Same as [forEach](##forEach) but takes uncurried functon.
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

let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
let acc = ref(list{})
s0->Belt.Set.Dict.forEach(x => acc := Belt.List.add(acc.contents, x))
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

let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
s0->Belt.Set.Dict.reduce(list{}, (acc, element) => acc->Belt.List.add(element)) == list{6, 5, 3, 2}
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

let s0 = Belt.Set.Dict.fromArray([2, 4, 6, 8], ~cmp=IntCmp.cmp)
s0->Belt.Set.Dict.every(isEven) == true
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

let s0 = Belt.Set.Dict.fromArray([1, 2, 4, 6, 8], ~cmp=IntCmp.cmp)
s0->Belt.Set.Dict.some(isOdd) == true
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

let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)
let s1 = s0->Belt.Set.Dict.keep(isEven)

s1->Belt.Set.Dict.toArray == [2, 4]
```
*/
let keep: (t<'value, 'id>, 'value => bool) => t<'value, 'id>

@deprecated("Use `partition` instead")
let partitionU: (t<'value, 'id>, 'value => bool) => (t<'value, 'id>, t<'value, 'id>)

/**
Returns a pair of sets, where first is the set of all the elements of set that satisfy the predicate, and second is the set of all the elements of set that do not satisfy the predicate.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let isOdd = x => mod(x, 2) != 0

let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)
let (s1, s2) = s0->Belt.Set.Dict.partition(isOdd)

s1->Belt.Set.Dict.toArray == [1, 3, 5]
s2->Belt.Set.Dict.toArray == [2, 4]
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

let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4], ~cmp=IntCmp.cmp)

s0->Belt.Set.Dict.size == 4
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

let s0 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

s0->Belt.Set.Dict.toList == list{1, 2, 3, 5}
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

let s0 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

s0->Belt.Set.Dict.toArray == [1, 2, 3, 5]
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

let s0 = Belt.Set.Dict.empty
let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

s0->Belt.Set.Dict.minimum == None
s1->Belt.Set.Dict.minimum == Some(1)
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

let s0 = Belt.Set.Dict.empty
let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

s0->Belt.Set.Dict.minUndefined == Js.undefined
s1->Belt.Set.Dict.minUndefined == Js.Undefined.return(1)
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

let s0 = Belt.Set.Dict.empty
let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

s0->Belt.Set.Dict.maximum == None
s1->Belt.Set.Dict.maximum == Some(5)
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

let s0 = Belt.Set.Dict.empty
let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

s0->Belt.Set.Dict.maxUndefined == Js.undefined
s1->Belt.Set.Dict.maxUndefined == Js.Undefined.return(5)
```
*/
let maxUndefined: t<'value, 'id> => Js.undefined<'value>

/**
Returns the reference of the value which is equivalent to value using the comparator
specifiecd by this collection. Returns `None` if element does not exist.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)

s0->Belt.Set.Dict.get(3, ~cmp=IntCmp.cmp) == Some(3)
s0->Belt.Set.Dict.get(20, ~cmp=IntCmp.cmp) == None
```
*/
let get: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => option<'value>

/**
Same as [get](#get) but returns `undefined` when element does not exist.
*/
let getUndefined: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => Js.undefined<'value>

/**
Same as [get](#get) but throw when element does not exist.
*/
let getExn: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => 'value

/**
Same as [get](#get) but throw when element does not exist.
*/
let getOrThrow: (t<'value, 'id>, 'value, ~cmp: cmp<'value, 'id>) => 'value

/**
Returns a tuple `((smaller, larger), present)`, `present` is true when element exist in set.

## Examples

```rescript
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)

let ((smaller, larger), present) = s0->Belt.Set.Dict.split(3, ~cmp=IntCmp.cmp)

present == true
smaller->Belt.Set.Dict.toArray == [1, 2]
larger->Belt.Set.Dict.toArray == [4, 5]
```
*/
let split: (
  t<'value, 'id>,
  'value,
  ~cmp: cmp<'value, 'id>,
) => ((t<'value, 'id>, t<'value, 'id>), bool)

/**
**throw** when invariant is not held
*/
let checkInvariantInternal: t<_> => unit

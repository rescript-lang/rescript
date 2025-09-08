/***
This module provides functions to work with pairs, which are 2-element tuples.
*/

type t<'a, 'b> = ('a, 'b)

/**
`first(pair)` returns the first element of a pair.

## Examples

```rescript
Pair.first((1, 2)) == 1
```
*/
external first: (('a, 'b)) => 'a = "%field0"

/**
`second(pair)` returns the second element of a pair.

## Examples

```rescript
Pair.second((1, 2)) == 2
```
*/
external second: (('a, 'b)) => 'b = "%field1"

/**
  `ignore(pair)` ignores the provided pair and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: ('a, 'b) => unit = "%ignore"

/**
`equal(pair1, pair2, f1, f2)` check equality of `pair2` and `pair2` using `f1` for
equality on the first element and `f2` for equality on the second element.

## Examples

```rescript
Pair.equal((1, "test"), (1, "test"), Int.equal, String.equal) == true

Pair.equal((1, "test"), (2, "test"), Int.equal, String.equal) == false
```
*/
let equal = ((a1, a2), (b1, b2), eq1, eq2) => eq1(a1, b1) && eq2(a2, b2)

/**
`compare(pair1, pair2, f1, f2)` compares two pairs, using `f1` to compare the first element
and `f2` to compare the second element. Ordering is based on the first element,
if they are equal, the second element is compared.

## Examples

```rescript
Pair.compare((1, "a"), (1, "a"), Int.compare, String.compare) == Ordering.equal
Pair.compare((1, "a"), (1, "b"), Int.compare, String.compare) == Ordering.less
Pair.compare((2, "a"), (1, "b"), Int.compare, String.compare) == Ordering.greater
```
*/
let compare = ((a1, a2), (b1, b2), cmp1, cmp2) =>
  switch cmp1(a1, b1) {
  | 0. => cmp2(a2, b2)
  | result => result
  }

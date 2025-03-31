/***
This module provides functions to work with pairs, which are 2-element tuples.
*/

type t<'a, 'b> = ('a, 'b)

/**
`first(pair)` returns the first element of a pair.

## Examples

```rescript
Pair.first((1, 2))->assertEqual(1)
```
*/
external first: (('a, 'b)) => 'a = "%field0"

/**
`second(pair)` returns the second element of a pair.

## Examples

```rescript
Pair.second((1, 2))->assertEqual(2)
```
*/
external second: (('a, 'b)) => 'b = "%field1"

/**
  `ignore(option)` ignores the provided pair and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: ('a, 'b) => unit = "%ignore"

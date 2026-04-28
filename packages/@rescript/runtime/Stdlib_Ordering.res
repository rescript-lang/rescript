/**
Ordering values represent the result of a comparison: `less`, `equal`, or `greater`.
*/
type t = float

/**
`less` is the ordering value returned when the left operand is smaller than the right operand.

## Examples

```rescript
(1)->Int.compare(2) == Ordering.less
```
*/
@inline let less = -1.

/**
`equal` is the ordering value returned when two values compare the same.

## Examples

```rescript
(2)->Int.compare(2) == Ordering.equal
```
*/
@inline let equal = 0.

/**
`greater` is the ordering value returned when the left operand is larger than the right operand.

## Examples

```rescript
(3)->Int.compare(2) == Ordering.greater
```
*/
@inline let greater = 1.

/**
`isLess(ordering)` returns `true` when `ordering` equals `Ordering.less`.

## Examples

```rescript
Ordering.isLess(Ordering.less) == true
Ordering.isLess(Ordering.equal) == false
```
*/
let isLess = ord => ord < equal

/**
`isEqual(ordering)` returns `true` when `ordering` equals `Ordering.equal`.

## Examples

```rescript
Ordering.isEqual(Ordering.equal) == true
Ordering.isEqual(Ordering.greater) == false
```
*/
let isEqual = ord => ord == equal

/**
`isGreater(ordering)` returns `true` when `ordering` equals `Ordering.greater`.

## Examples

```rescript
Ordering.isGreater(Ordering.greater) == true
Ordering.isGreater(Ordering.less) == false
```
*/
let isGreater = ord => ord > equal

/**
`invert(ordering)` flips the ordering result (less becomes greater and vice versa).

## Examples

```rescript
Ordering.invert(Ordering.less) == Ordering.greater
Ordering.invert(Ordering.equal) == Ordering.equal
```
*/
let invert = ord => -.ord

/**
`fromInt(n)` converts an integer comparison result into an ordering.

## Examples

```rescript
Ordering.fromInt(-5) == Ordering.less
Ordering.fromInt(0) == Ordering.equal
Ordering.fromInt(3) == Ordering.greater
```
*/
let fromInt = n => n < 0 ? less : n > 0 ? greater : equal

/**
  `ignore(ordering)` ignores the provided ordering and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

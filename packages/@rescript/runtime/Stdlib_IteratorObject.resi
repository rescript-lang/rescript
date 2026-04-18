/***
A JavaScript iterator object that inherits from `Iterator.prototype`.

This models a helper-enabled iterator object that conforms to the iterator
protocol and also inherits from `Iterator.prototype`, providing the standard
iterator helper methods. This is stricter than `IterableIterator.t`, which only
models the structural protocol shape.

See [`Iterator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator) on MDN.
*/

/**
Type representing an `IteratorObject` instance.
*/
type t<'yield, 'return, 'next> = private Stdlib_Iterable.t<'yield>

/**
Result returned by `IteratorObject.next`.
*/
type result<'yield, 'return> = Stdlib_Iterator.result<'yield, 'return>

/**
`asIterator(iteratorObject)` views `iteratorObject` as a bare `Iterator.t`.
*/
external asIterator: t<'yield, 'return, 'next> => Stdlib_Iterator.t<'yield, 'return, 'next> =
  "%identity"

/**
`asIterable(iteratorObject)` views `iteratorObject` as an `Iterable.t`.
*/
external asIterable: t<'yield, 'return, 'next> => Stdlib_Iterable.t<'yield> = "%identity"

/**
`next(iteratorObject)` advances `iteratorObject` without sending a value back in.
*/
let next: t<'yield, 'return, 'next> => Stdlib_Iterator.result<'yield, 'return>

/**
`nextValue(iteratorObject, value)` advances `iteratorObject`, sending `value` back into it.
*/
let nextValue: (t<'yield, 'return, 'next>, 'next) => Stdlib_Iterator.result<'yield, 'return>

/**
`toArray(iteratorObject)` consumes `iteratorObject` and returns its yielded values as an array.
*/
@send
external toArray: t<'yield, 'return, 'next> => array<'yield> = "toArray"

/**
`toArrayWithMapper(iteratorObject, mapper)` consumes `iteratorObject`, applies `mapper`
to each yielded value, and returns the mapped values as an array.
*/
@val
external toArrayWithMapper: (t<'yield, 'return, 'next>, 'yield => 'mapped) => array<'mapped> =
  "Array.from"

/**
`forEach(iteratorObject, f)` consumes `iteratorObject` and runs `f` for each yielded value.
*/
@send
external forEach: (t<'yield, 'return, 'next>, 'yield => unit) => unit = "forEach"

/**
`ignore(iteratorObject)` marks `iteratorObject` as intentionally unused.
*/
external ignore: t<'yield, 'return, 'next> => unit = "%ignore"

/**
`drop(iteratorObject, count)` returns a new iterator helper that skips the first `count`
yielded values.
*/
@send
external drop: (t<'yield, 'return, 'next>, int) => t<'yield, unit, unknown> = "drop"

/**
`every(iteratorObject, predicate)` returns `true` if `predicate` is true for every yielded value.
*/
@send
external every: (t<'yield, 'return, 'next>, 'yield => bool) => bool = "every"

/**
`filter(iteratorObject, predicate)` returns a new iterator helper containing only the
yielded values that satisfy `predicate`.
*/
@send
external filter: (t<'yield, 'return, 'next>, 'yield => bool) => t<'yield, unit, unknown> = "filter"

/**
`find(iteratorObject, predicate)` returns the first yielded value that satisfies
`predicate`, or `None` if nothing matches.
*/
@send
external find: (t<'yield, 'return, 'next>, 'yield => bool) => option<'yield> = "find"

/**
`flatMap(iteratorObject, f)` maps each yielded value to another iterable and flattens the result.
*/
@send
external flatMap: (
  t<'yield, 'return, 'next>,
  'yield => Stdlib_Iterable.t<'mapped>,
) => t<'mapped, unit, unknown> = "flatMap"

/**
`map(iteratorObject, f)` returns a new iterator helper whose yielded values are
produced by applying `f`.
*/
@send
external map: (t<'yield, 'return, 'next>, 'yield => 'mapped) => t<'mapped, unit, unknown> = "map"

/**
`reduce(iteratorObject, reducer, ~initialValue)` combines all yielded values using
`reducer`.

If `~initialValue` is omitted, the first yielded value becomes the initial accumulator,
so the accumulator type must match the yielded value type.
*/
@send
external reduce: (
  t<'yield, 'return, 'next>,
  ('acc, 'yield) => 'acc,
  ~initialValue: 'acc=?,
) => 'acc = "reduce"

/**
`some(iteratorObject, predicate)` returns `true` if at least one yielded value
satisfies `predicate`.
*/
@send
external some: (t<'yield, 'return, 'next>, 'yield => bool) => bool = "some"

/**
`take(iteratorObject, count)` returns a new iterator helper that yields at most `count`
values.
*/
@send
external take: (t<'yield, 'return, 'next>, int) => t<'yield, unit, unknown> = "take"

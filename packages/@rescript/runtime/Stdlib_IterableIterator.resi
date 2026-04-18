/***
A JavaScript iterable iterator.

This models values that satisfy both the iterable and iterator protocols
without assuming iterator helper methods are available.

Unlike `IteratorObject.t`, this is only the iterable iterator protocol shape. It
does not imply that iterator helper methods such as `map`, `filter`, or
`toArray` are available.

See [Iteration protocols](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols) on MDN.
*/

/**
Type representing an iterable iterator.
*/
type t<'yield, 'return, 'next> = private Stdlib_Iterable.t<'yield>

/**
`make(nextFn)` creates an iterable iterator from a function returning the next result.

The returned value is both an iterator and an iterable.

## Examples

```rescript
let current = ref(0)

let iterableIterator = IterableIterator.make(() => {
  let value = current.contents
  current := value + 1

  if value >= 2 {
    Iterator.doneWithValue("done")
  } else {
    Iterator.value(value)
  }
})

iterableIterator->IterableIterator.next == Yield({value: 0})
```
*/
let make: (unit => Stdlib_Iterator.result<'yield, 'return>) => t<'yield, 'return, unit>

/**
`asIterator(value)` views an iterable iterator as an `Iterator.t`.
*/
external asIterator: t<'yield, 'return, 'next> => Stdlib_Iterator.t<'yield, 'return, 'next> =
  "%identity"

/**
`asIterable(value)` views an iterable iterator as an `Iterable.t`.
*/
external asIterable: t<'yield, 'return, 'next> => Stdlib_Iterable.t<'yield> = "%identity"

/**
`next(iterator)` advances `iterator` without sending a value back in.
*/
@send
external next: t<'yield, 'return, 'next> => Stdlib_Iterator.result<'yield, 'return> = "next"

/**
`nextValue(iterator, value)` advances `iterator`, sending `value` back into it.
*/
@send
external nextValue: (t<'yield, 'return, 'next>, 'next) => Stdlib_Iterator.result<'yield, 'return> =
  "next"

/**
`toArrayWithMapper(iterator, mapper)` consumes `iterator`, applies `mapper` to each yielded
value, and returns the mapped values as an array.
*/
@val
external toArrayWithMapper: (t<'yield, 'return, 'next>, 'yield => 'mapped) => array<'mapped> =
  "Array.from"

/**
`ignore(iterator)` marks `iterator` as intentionally unused.
*/
external ignore: t<'yield, 'return, 'next> => unit = "%ignore"

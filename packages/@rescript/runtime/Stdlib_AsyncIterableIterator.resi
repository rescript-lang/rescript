/***
A JavaScript async iterable iterator.

This models values that satisfy both the async iterable and async iterator
protocols without assuming any additional helper methods are available.

See [`AsyncIterator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncIterator) on MDN.
*/

/**
Type representing an async iterable iterator.
*/
type t<'yield, 'return, 'next> = private Stdlib_AsyncIterable.t<'yield>

/**
`make(nextFn)` creates an async iterable iterator from a function returning the next result.

The returned value is both an async iterator and an async iterable.

## Examples

```rescript
let current = ref(0)

let asyncIterableIterator = AsyncIterableIterator.make(async () => {
  let value = current.contents
  current := value + 1

  if value >= 3 {
    AsyncIterator.done()
  } else {
    AsyncIterator.value(value)
  }
})

let main = async () =>
  await asyncIterableIterator->AsyncIterableIterator.forEach(value => Console.log(value))

main()->ignore
```
*/
let make: (unit => promise<Stdlib_Iterator.result<'yield, 'return>>) => t<'yield, 'return, unit>

/**
`asAsyncIterable(value)` views `value` as an `AsyncIterable.t`.
*/
external asAsyncIterable: t<'yield, 'return, 'next> => Stdlib_AsyncIterable.t<'yield> = "%identity"

/**
`asAsyncIterator(value)` views `value` as an `AsyncIterator.t`.
*/
external asAsyncIterator: t<'yield, 'return, 'next> => Stdlib_AsyncIterator.t<
  'yield,
  'return,
  'next,
> = "%identity"

/**
`next(iterator)` advances `iterator` without sending a value back in.
*/
@send
external next: t<'yield, 'return, 'next> => promise<Stdlib_Iterator.result<'yield, 'return>> =
  "next"

/**
`nextValue(iterator, value)` advances `iterator`, sending `value` back into it.
*/
@send
external nextValue: (
  t<'yield, 'return, 'next>,
  'next,
) => promise<Stdlib_Iterator.result<'yield, 'return>> = "next"

/**
`forEach(iterator, f)` consumes all yielded values in `iterator` and runs `f`
for each yielded value.

The final return value is not passed to `f`.
*/
let forEach: (t<'yield, 'return, 'next>, 'yield => unit) => promise<unit>

/**
`ignore(iterator)` marks `iterator` as intentionally unused.
*/
external ignore: t<'yield, 'return, 'next> => unit = "%ignore"

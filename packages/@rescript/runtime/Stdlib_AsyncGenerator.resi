/***
A JavaScript async generator object.

Async generators are built-in async iterable iterators with additional
`return()` and `throw()` capabilities.

See [`AsyncGenerator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncGenerator) on MDN.
*/

/**
Type representing an async generator object.
*/
type t<'yield, 'return, 'next> = private Stdlib_AsyncIterable.t<'yield>

/**
`asAsyncIterable(generator)` views `generator` as an `AsyncIterable.t`.
*/
external asAsyncIterable: t<'yield, 'return, 'next> => Stdlib_AsyncIterable.t<'yield> = "%identity"

/**
`asAsyncIterableIterator(generator)` views `generator` as an `AsyncIterableIterator.t`.
*/
external asAsyncIterableIterator: t<'yield, 'return, 'next> => Stdlib_AsyncIterableIterator.t<
  'yield,
  'return,
  'next,
> = "%identity"

/**
`asAsyncIterator(generator)` views `generator` as an `AsyncIterator.t`.
*/
external asAsyncIterator: t<'yield, 'return, 'next> => Stdlib_AsyncIterator.t<
  'yield,
  'return,
  'next,
> = "%identity"

/**
`next(generator)` advances `generator` without sending a value back in.
*/
@send
external next: t<'yield, 'return, unit> => promise<Stdlib_Iterator.result<'yield, 'return>> = "next"

/**
`nextValue(generator, value)` advances `generator`, sending `value` back into it.
*/
@send
external nextValue: (
  t<'yield, 'return, 'next>,
  'next,
) => promise<Stdlib_Iterator.result<'yield, 'return>> = "next"

/**
`returnValue(generator, value)` completes `generator` with the final return `value`.
*/
@send
external returnValue: (
  t<'yield, 'return, 'next>,
  'return,
) => promise<Stdlib_Iterator.result<'yield, 'return>> = "return"

/**
`throwError(generator, error)` throws `error` into `generator`.
*/
@send
external throwError: (
  t<'yield, 'return, 'next>,
  exn,
) => promise<Stdlib_Iterator.result<'yield, 'return>> = "throw"

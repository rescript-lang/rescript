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
let next: t<'yield, 'return, 'next> => promise<Stdlib_AsyncIterator.result<'yield, 'return>>

/**
`nextValue(generator, value)` advances `generator`, sending `value` back into it.
*/
let nextValue: (
  t<'yield, 'return, 'next>,
  'next,
) => promise<Stdlib_AsyncIterator.result<'yield, 'return>>

/**
`returnValue(generator, value)` completes `generator` with the final return `value`.
*/
let returnValue: (
  t<'yield, 'return, 'next>,
  'return,
) => promise<Stdlib_AsyncIterator.result<'yield, 'return>>

/**
`throwError(generator, error)` throws `error` into `generator`.
*/
let throwError: (
  t<'yield, 'return, 'next>,
  exn,
) => promise<Stdlib_AsyncIterator.result<'yield, 'return>>

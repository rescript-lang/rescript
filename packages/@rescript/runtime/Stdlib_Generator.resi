/***
A JavaScript generator object.

Generators are built-in iterable iterators with additional `return()` and
`throw()` capabilities.

See [`Generator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Generator) on MDN.
*/

/**
Type representing a generator object.
*/
type t<'yield, 'return, 'next> = private Stdlib_Iterable.t<'yield>

/**
`asIterator(generator)` views `generator` as an `Iterator.t`.
*/
external asIterator: t<'yield, 'return, 'next> => Stdlib_Iterator.t<'yield, 'return, 'next> =
  "%identity"

/**
`asIterable(generator)` views `generator` as an `Iterable.t`.
*/
external asIterable: t<'yield, 'return, 'next> => Stdlib_Iterable.t<'yield> = "%identity"

/**
`asIteratorObject(generator)` views `generator` as an `IteratorObject.t`.
*/
external asIteratorObject: t<'yield, 'return, 'next> => Stdlib_IteratorObject.t<
  'yield,
  'return,
  'next,
> = "%identity"

/**
`next(generator)` advances `generator` without sending a value back in.
*/
let next: t<'yield, 'return, 'next> => Stdlib_Iterator.result<'yield, 'return>

/**
`nextValue(generator, value)` advances `generator`, sending `value` back into it.
*/
let nextValue: (t<'yield, 'return, 'next>, 'next) => Stdlib_Iterator.result<'yield, 'return>

/**
`returnValue(generator, value)` completes `generator` with the final return `value`.
*/
let returnValue: (t<'yield, 'return, 'next>, 'return) => Stdlib_Iterator.result<'yield, 'return>

/**
`throwError(generator, error)` throws `error` into `generator`.
*/
let throwError: (t<'yield, 'return, 'next>, exn) => Stdlib_Iterator.result<'yield, 'return>

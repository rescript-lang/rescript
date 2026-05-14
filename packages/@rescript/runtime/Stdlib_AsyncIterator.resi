/***
A JavaScript async iterator protocol object.

This models the async iterator protocol: an object with a `next()` method that
returns a promise resolving to an iterator result.

See [`AsyncIterator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/AsyncIterator) on MDN.
*/

/**
Type representing an async iterator protocol object.

- `'yield` is the type produced while iterating
- `'return` is the final completion value once iteration is done
- `'next` is the type that can be sent back into `next(value)`
*/
@notUndefined
type t<'yield, 'return, 'next>

/**
Result returned by `AsyncIterator.next`.
*/
type result<'yield, 'return> = Stdlib_Iterator.result<'yield, 'return>

type rawResult<'yield, 'return>

/**
Low-level raw JavaScript async iterator result shape.

This is mainly intended for internal use and protocol-level interop. Prefer
`AsyncIterator.result`, `AsyncIterator.next`, and `AsyncIterator.nextValue` in
normal ReScript code.
*/
let normalizeResult: rawResult<'yield, 'return> => result<'yield, 'return>

/**
`value(value)` creates a yielded iterator result.

## Examples

```rescript
let current = ref(0)

let asyncIterableIterator = AsyncIterator.make(async () => {
  let value = current.contents
  current := value + 1

  if value >= 3 {
    AsyncIterator.done()
  } else {
    AsyncIterator.value(value)
  }
})
```
*/
let value: 'yield => result<'yield, 'return>

/**
`done()` creates a completed iterator result with `unit` as the final value.
*/
let done: unit => result<'yield, unit>

/**
`doneWithValue(value)` creates a completed iterator result with the provided final value.
*/
let doneWithValue: 'return => result<'yield, 'return>

/**
`nextRaw(asyncIterator)` advances `asyncIterator` and returns a promise for the raw
JavaScript iterator result object.

This is mainly intended for internal use and protocol-level interop.
*/
let nextRaw: t<'yield, 'return, 'next> => promise<rawResult<'yield, 'return>>

/**
`next(asyncIterator)` advances `asyncIterator` without sending a value back in.

## Examples

```rescript
let asyncIterator: AsyncIterator.t<int, unit, unit> = %raw(`({
  current: 0,
  next() {
    if (this.current < 2) {
      return Promise.resolve({done: false, value: this.current++})
    }
    return Promise.resolve({done: true, value: undefined})
  }
})`)

let process = async () => {
  switch await asyncIterator->AsyncIterator.next {
  | Yield({value: 0}) => ()
  | _ => ()
  }
}

process()->ignore
```
*/
let next: t<'yield, 'return, 'next> => promise<result<'yield, 'return>>

/**
`nextValueRaw(asyncIterator, value)` advances `asyncIterator`, sends `value` back
into it, and returns a promise for the raw JavaScript iterator result object.

This is mainly intended for internal use and protocol-level interop.
*/
let nextValueRaw: (t<'yield, 'return, 'next>, 'next) => promise<rawResult<'yield, 'return>>

/**
`nextValue(asyncIterator, value)` advances `asyncIterator`, sending `value` back into it.
*/
let nextValue: (t<'yield, 'return, 'next>, 'next) => promise<result<'yield, 'return>>

/**
`forEach(asyncIterator, f)` consumes all yielded values in `asyncIterator` and runs `f`
for each yielded value.

The final return value is not passed to `f`.
*/
let forEach: (t<'yield, 'return, 'next>, 'yield => unit) => promise<unit>

/**
`make(nextFn)` creates an async iterator protocol object from a function returning
the next result.

The returned value is not automatically async iterable.

## Examples

```rescript
let current = ref(0)

let asyncIterator = AsyncIterator.make(async () => {
  let value = current.contents
  current := value + 1

  if value >= 3 {
    AsyncIterator.done()
  } else {
    AsyncIterator.value(value)
  }
})

let main = async () =>
  await asyncIterator->AsyncIterator.forEach(value => Console.log(value))

main()->ignore
```
*/
let make: (unit => promise<result<'yield, 'return>>) => t<'yield, 'return, unit>

/**
`ignore(asyncIterator)` marks `asyncIterator` as intentionally unused.
*/
external ignore: t<'yield, 'return, 'next> => unit = "%ignore"

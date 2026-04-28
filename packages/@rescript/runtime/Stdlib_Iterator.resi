/***
A JavaScript iterator protocol object.

This is the minimal protocol shape: an object with a `next()` method that
produces yielded values until it eventually completes with a final return value.

See [`Iterator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator) on MDN.
*/

/**
Type representing an iterator protocol object.

- `'yield` is the type produced while iterating
- `'return` is the final completion value once iteration is done
- `'next` is the type that can be sent back into `next(value)`
*/
@notUndefined
type t<'yield, 'return, 'next>

/**
Result returned by `Iterator.next`.
*/
@tag("done")
type result<'yield, 'return> =
  | @as(false) Yield({value: 'yield})
  | @as(true) Return({value: 'return})

type rawResult<'yield, 'return>

/**
Low-level raw JavaScript iterator result shape.

This is mainly intended for internal use and protocol-level interop. Prefer
`Iterator.result`, `Iterator.next`, and `Iterator.nextValue` in normal ReScript code.
*/
let normalizeResult: rawResult<'yield, 'return> => result<'yield, 'return>

/**
`value(value)` creates a yielded iterator result.
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
`nextRaw(iterator)` advances `iterator` and returns the raw JavaScript iterator result object.

This is mainly intended for internal use and protocol-level interop.
*/
let nextRaw: t<'yield, 'return, 'next> => rawResult<'yield, 'return>

/**
`next(iterator)` advances `iterator` without sending a value back in.

## Examples

```rescript
let iterator: Iterator.t<int, string, unit> = %raw(`({
  current: 0,
  next() {
    if (this.current < 2) {
      return {done: false, value: this.current++}
    }
    return {done: true, value: "done"}
  }
})`)

iterator->Iterator.next == Yield({value: 0})
```
*/
let next: t<'yield, 'return, 'next> => result<'yield, 'return>

/**
`nextValueRaw(iterator, value)` advances `iterator`, sending `value` back into it,
and returns the raw JavaScript iterator result object.

This is mainly intended for internal use and protocol-level interop.
*/
let nextValueRaw: (t<'yield, 'return, 'next>, 'next) => rawResult<'yield, 'return>

/**
`nextValue(iterator, value)` advances `iterator`, sending `value` back into it.
*/
let nextValue: (t<'yield, 'return, 'next>, 'next) => result<'yield, 'return>

/**
`make(nextFn)` creates a bare iterator protocol object from a function returning
the next result.

The returned value is not automatically iterable.

## Examples

```rescript
let current = ref(0)

let iterator = Iterator.make(() => {
  let value = current.contents
  current := value + 1

  if value >= 2 {
    Iterator.doneWithValue("done")
  } else {
    Iterator.value(value)
  }
})

iterator->Iterator.next == Yield({value: 0})
```
*/
let make: (unit => result<'yield, 'return>) => t<'yield, 'return, unit>

/**
`ignore(iterator)` marks `iterator` as intentionally unused.
*/
external ignore: t<'yield, 'return, 'next> => unit = "%ignore"

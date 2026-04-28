@notUndefined
type t<'yield, 'return, 'next>

type rawResult<'yield, 'return>

@tag("done")
type result<'yield, 'return> =
  | @as(false) Yield({value: 'yield})
  | @as(true) Return({value: 'return})

let normalizeResult: rawResult<'yield, 'return> => result<
  'yield,
  'return,
> = %raw(`result => result.done ? {done: true, value: result.value} : {done: false, value: result.value}`)

let value: 'yield => result<'yield, 'return> = %raw(`value => ({done: false, value})`)
let done: unit => result<'yield, unit> = %raw(`() => ({done: true, value: undefined})`)
let doneWithValue: 'return => result<'yield, 'return> = %raw(`value => ({done: true, value})`)

@send
external nextRaw: t<'yield, 'return, 'next> => rawResult<'yield, 'return> = "next"

let next = iterator => iterator->nextRaw->normalizeResult

@send
external nextValueRaw: (t<'yield, 'return, 'next>, 'next) => rawResult<'yield, 'return> = "next"

let nextValue = (iterator, value) => iterator->nextValueRaw(value)->normalizeResult

let make: (unit => result<'yield, 'return>) => t<
  'yield,
  'return,
  unit,
> = %raw(`function makeIterator(next) {
  return {
    next
  }
}`)

external ignore: t<'yield, 'return, 'next> => unit = "%ignore"

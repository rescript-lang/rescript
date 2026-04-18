@notUndefined
type t<'yield, 'return, 'next>

@tag("done")
type result<'yield, 'return> =
  | @as(false) Yield({value: 'yield})
  | @as(true) Return({value: 'return})

let value: 'yield => result<'yield, 'return> = %raw(`value => ({done: false, value})`)
let done: unit => result<'yield, unit> = %raw(`() => ({done: true, value: undefined})`)
let doneWithValue: 'return => result<'yield, 'return> = %raw(`value => ({done: true, value})`)

@send
external next: t<'yield, 'return, unit> => result<'yield, 'return> = "next"

@send
external nextValue: (t<'yield, 'return, 'next>, 'next) => result<'yield, 'return> = "next"

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

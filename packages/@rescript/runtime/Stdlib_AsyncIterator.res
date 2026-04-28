@notUndefined
type t<'yield, 'return, 'next>

type rawResult<'yield, 'return>

type result<'yield, 'return> = Stdlib_Iterator.result<'yield, 'return>

let value: 'yield => result<'yield, 'return> = %raw(`value => ({done: false, value})`)
let done: unit => result<'yield, unit> = %raw(`() => ({done: true, value: undefined})`)
let doneWithValue: 'return => result<'yield, 'return> = %raw(`value => ({done: true, value})`)

let normalizeResult: rawResult<'yield, 'return> => result<
  'yield,
  'return,
> = %raw(`result => result.done ? {done: true, value: result.value} : {done: false, value: result.value}`)

@send
external nextRaw: t<'yield, 'return, 'next> => promise<rawResult<'yield, 'return>> = "next"

let next = async iterator => normalizeResult(await iterator->nextRaw)

@send
external nextValueRaw: (t<'yield, 'return, 'next>, 'next) => promise<rawResult<'yield, 'return>> =
  "next"

let nextValue = async (iterator, value) => normalizeResult(await iterator->nextValueRaw(value))

let forEach = async (iterator, f) => {
  let iteratorDone = ref(false)

  while !iteratorDone.contents {
    switch await iterator->next {
    | Yield({value}) => f(value)
    | Return(_) => iteratorDone := true
    }
  }
}

let make: (unit => promise<result<'yield, 'return>>) => t<
  'yield,
  'return,
  unit,
> = %raw(`function makeAsyncIterator(next) {
  return {
    next
  }
}`)

external ignore: t<'yield, 'return, 'next> => unit = "%ignore"

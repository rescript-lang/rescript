@notUndefined
type t<'yield, 'return, 'next>

type result<'yield, 'return> = Stdlib_Iterator.result<'yield, 'return>

let value: 'yield => result<'yield, 'return> = %raw(`value => ({done: false, value})`)
let done: unit => result<'yield, unit> = %raw(`() => ({done: true, value: undefined})`)
let doneWithValue: 'return => result<'yield, 'return> = %raw(`value => ({done: true, value})`)

@send
external next: t<'yield, 'return, 'next> => promise<result<'yield, 'return>> = "next"

@send
external nextValue: (t<'yield, 'return, 'next>, 'next) => promise<result<'yield, 'return>> = "next"

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

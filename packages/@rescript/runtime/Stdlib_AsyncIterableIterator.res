type t<'yield, 'return, 'next> = Stdlib_AsyncIterable.t<'yield>

let make: (unit => promise<Stdlib_Iterator.result<'yield, 'return>>) => t<
  'yield,
  'return,
  unit,
> = %raw(`function makeAsyncIterableIterator(next) {
  return {
    next,
    [Symbol.asyncIterator]() {
      return this;
    }
  }
}`)

external asAsyncIterable: t<'yield, 'return, 'next> => Stdlib_AsyncIterable.t<'yield> = "%identity"
external asAsyncIterator: t<'yield, 'return, 'next> => Stdlib_AsyncIterator.t<
  'yield,
  'return,
  'next,
> = "%identity"

@send
external next: t<'yield, 'return, 'next> => promise<Stdlib_Iterator.result<'yield, 'return>> =
  "next"

@send
external nextValue: (
  t<'yield, 'return, 'next>,
  'next,
) => promise<Stdlib_Iterator.result<'yield, 'return>> = "next"

let forEach = async (iterator, f) => {
  let iteratorDone = ref(false)

  while !iteratorDone.contents {
    switch await iterator->next {
    | Yield({value}) => f(value)
    | Return(_) => iteratorDone := true
    }
  }
}

external ignore: t<'yield, 'return, 'next> => unit = "%ignore"

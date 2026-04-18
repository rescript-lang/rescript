type t<'yield, 'return, 'next> = Stdlib_AsyncIterable.t<'yield>

external asAsyncIterable: t<'yield, 'return, 'next> => Stdlib_AsyncIterable.t<'yield> = "%identity"
external asAsyncIterableIterator: t<'yield, 'return, 'next> => Stdlib_AsyncIterableIterator.t<
  'yield,
  'return,
  'next,
> = "%identity"
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

@send
external returnValue: (
  t<'yield, 'return, 'next>,
  'return,
) => promise<Stdlib_Iterator.result<'yield, 'return>> = "return"

@send
external throwError: (
  t<'yield, 'return, 'next>,
  exn,
) => promise<Stdlib_Iterator.result<'yield, 'return>> = "throw"

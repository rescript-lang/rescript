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

let next = generator => generator->asAsyncIterator->Stdlib_AsyncIterator.next

let nextValue = (generator, value) =>
  generator->asAsyncIterator->Stdlib_AsyncIterator.nextValue(value)

@send
external returnValueRaw: (
  t<'yield, 'return, 'next>,
  'return,
) => promise<Stdlib_AsyncIterator.rawResult<'yield, 'return>> = "return"

let returnValue = async (generator, value) =>
  Stdlib_AsyncIterator.normalizeResult(await generator->returnValueRaw(value))

@send
external throwErrorRaw: (
  t<'yield, 'return, 'next>,
  exn,
) => promise<Stdlib_AsyncIterator.rawResult<'yield, 'return>> = "throw"

let throwError = async (generator, error) =>
  Stdlib_AsyncIterator.normalizeResult(await generator->throwErrorRaw(error))

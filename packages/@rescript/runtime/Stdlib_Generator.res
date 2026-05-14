type t<'yield, 'return, 'next> = Stdlib_Iterable.t<'yield>

external asIterator: t<'yield, 'return, 'next> => Stdlib_Iterator.t<'yield, 'return, 'next> =
  "%identity"
external asIterable: t<'yield, 'return, 'next> => Stdlib_Iterable.t<'yield> = "%identity"
external asIteratorObject: t<'yield, 'return, 'next> => Stdlib_IteratorObject.t<
  'yield,
  'return,
  'next,
> = "%identity"

let next = generator => generator->asIterator->Stdlib_Iterator.next

let nextValue = (generator, value) => generator->asIterator->Stdlib_Iterator.nextValue(value)

@send
external returnValueRaw: (
  t<'yield, 'return, 'next>,
  'return,
) => Stdlib_Iterator.rawResult<'yield, 'return> = "return"

let returnValue = (generator, value) =>
  generator->returnValueRaw(value)->Stdlib_Iterator.normalizeResult

@send
external throwErrorRaw: (
  t<'yield, 'return, 'next>,
  exn,
) => Stdlib_Iterator.rawResult<'yield, 'return> = "throw"

let throwError = (generator, error) =>
  generator->throwErrorRaw(error)->Stdlib_Iterator.normalizeResult

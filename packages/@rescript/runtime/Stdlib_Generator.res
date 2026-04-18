type t<'yield, 'return, 'next> = Stdlib_Iterable.t<'yield>

external asIterator: t<'yield, 'return, 'next> => Stdlib_Iterator.t<'yield, 'return, 'next> =
  "%identity"
external asIterable: t<'yield, 'return, 'next> => Stdlib_Iterable.t<'yield> = "%identity"
external asIteratorObject: t<'yield, 'return, 'next> => Stdlib_IteratorObject.t<
  'yield,
  'return,
  'next,
> = "%identity"

@send
external next: t<'yield, 'return, unit> => Stdlib_Iterator.result<'yield, 'return> = "next"

@send
external nextValue: (t<'yield, 'return, 'next>, 'next) => Stdlib_Iterator.result<'yield, 'return> =
  "next"

@send
external returnValue: (
  t<'yield, 'return, 'next>,
  'return,
) => Stdlib_Iterator.result<'yield, 'return> = "return"

@send
external throwError: (t<'yield, 'return, 'next>, exn) => Stdlib_Iterator.result<'yield, 'return> =
  "throw"

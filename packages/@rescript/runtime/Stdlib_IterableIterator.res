type t<'yield, 'return, 'next> = Stdlib_Iterable.t<'yield>

let make: (unit => Stdlib_Iterator.result<'yield, 'return>) => t<
  'yield,
  'return,
  unit,
> = %raw(`function makeIterableIterator(next) {
  return {
    next,
    [Symbol.iterator]() {
      return this;
    }
  }
}`)

external asIterator: t<'yield, 'return, 'next> => Stdlib_Iterator.t<'yield, 'return, 'next> =
  "%identity"
external asIterable: t<'yield, 'return, 'next> => Stdlib_Iterable.t<'yield> = "%identity"

@send
external next: t<'yield, 'return, unit> => Stdlib_Iterator.result<'yield, 'return> = "next"

@send
external nextValue: (t<'yield, 'return, 'next>, 'next) => Stdlib_Iterator.result<'yield, 'return> =
  "next"

@val
external toArrayWithMapper: (t<'yield, 'return, 'next>, 'yield => 'mapped) => array<'mapped> =
  "Array.from"

external ignore: t<'yield, 'return, 'next> => unit = "%ignore"

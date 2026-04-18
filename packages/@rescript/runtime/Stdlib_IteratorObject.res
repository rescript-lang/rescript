type t<'yield, 'return, 'next> = Stdlib_Iterable.t<'yield>

type result<'yield, 'return> = Stdlib_Iterator.result<'yield, 'return>

external asIterator: t<'yield, 'return, 'next> => Stdlib_Iterator.t<'yield, 'return, 'next> =
  "%identity"
external asIterable: t<'yield, 'return, 'next> => Stdlib_Iterable.t<'yield> = "%identity"

@send
external next: t<'yield, 'return, unit> => Stdlib_Iterator.result<'yield, 'return> = "next"

@send
external nextValue: (t<'yield, 'return, 'next>, 'next) => Stdlib_Iterator.result<'yield, 'return> =
  "next"

@send
external toArray: t<'yield, 'return, 'next> => array<'yield> = "toArray"

@val
external toArrayWithMapper: (t<'yield, 'return, 'next>, 'yield => 'mapped) => array<'mapped> =
  "Array.from"

@send
external forEach: (t<'yield, 'return, 'next>, 'yield => unit) => unit = "forEach"

external ignore: t<'yield, 'return, 'next> => unit = "%ignore"

@send
external drop: (t<'yield, 'return, 'next>, int) => t<'yield, unit, unknown> = "drop"

@send
external every: (t<'yield, 'return, 'next>, 'yield => bool) => bool = "every"

@send
external filter: (t<'yield, 'return, 'next>, 'yield => bool) => t<'yield, unit, unknown> = "filter"

@send
external find: (t<'yield, 'return, 'next>, 'yield => bool) => option<'yield> = "find"

@send
external flatMap: (
  t<'yield, 'return, 'next>,
  'yield => Stdlib_Iterable.t<'mapped>,
) => t<'mapped, unit, unknown> = "flatMap"

@send
external map: (t<'yield, 'return, 'next>, 'yield => 'mapped) => t<'mapped, unit, unknown> = "map"

@send
external reduce: (
  t<'yield, 'return, 'next>,
  ('acc, 'yield) => 'acc,
  ~initialValue: 'acc=?,
) => 'acc = "reduce"

@send
external some: (t<'yield, 'return, 'next>, 'yield => bool) => bool = "some"

@send
external take: (t<'yield, 'return, 'next>, int) => t<'yield, unit, unknown> = "take"

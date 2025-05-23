@notUndefined
type t<'a>

type value<'a> = {
  done: bool,
  value: option<'a>,
}

@send external next: t<'a> => value<'a> = "next"
@send
external toArray: t<'a> => array<'a> = "toArray"
external toArrayWithMapper: (t<'a>, 'a => 'b) => array<'b> = "Array.from"

@send
external forEach: (t<'a>, 'a => unit) => unit = "forEach"

external ignore: t<'a> => unit = "%ignore"

@send
external drop: (t<'a>, int) => t<'a> = "drop"

@send
external every: (t<'a>, 'a => bool) => bool = "every"

@send
external filter: (t<'a>, 'a => bool) => t<'a> = "filter"

@send
external find: (t<'a>, 'a => bool) => option<'a> = "find"

@send
external flatMap: (t<'a>, 'a => t<'b>) => t<'b> = "flatMap"

@send
external map: (t<'a>, 'a => 'b) => t<'b> = "map"

@send
external reduce: (t<'a>, ('acc, 'a) => 'acc, ~initialValue: 'acc=?) => 'acc = "reduce"

@send
external some: (t<'a>, 'a => bool) => bool = "some"

@send
external take: (t<'a>, int) => t<'a> = "take"

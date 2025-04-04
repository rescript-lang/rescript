type t<'k, 'v>

@new external make: unit => t<'k, 'v> = "Map"
@new external fromArray: array<('k, 'v)> => t<'k, 'v> = "Map"
@new external fromIterator: Stdlib_Iterator.t<('k, 'v)> => t<'k, 'v> = "Map"

@get external size: t<'k, 'v> => int = "size"

@send external clear: t<'k, 'v> => unit = "clear"

@send external forEach: (t<'k, 'v>, 'v => unit) => unit = "forEach"
@send external forEachWithKey: (t<'k, 'v>, ('v, 'k) => unit) => unit = "forEach"

@send external get: (t<'k, 'v>, 'k) => option<'v> = "get"
@send external has: (t<'k, 'v>, 'k) => bool = "has"
@send external set: (t<'k, 'v>, 'k, 'v) => unit = "set"
@send external delete: (t<'k, 'v>, 'k) => bool = "delete"

@send external keys: t<'k, 'v> => Stdlib_Iterator.t<'k> = "keys"
@send external values: t<'k, 'v> => Stdlib_Iterator.t<'v> = "values"
@send external entries: t<'k, 'v> => Stdlib_Iterator.t<('k, 'v)> = "entries"

external ignore: t<'k, 'v> => unit = "%ignore"

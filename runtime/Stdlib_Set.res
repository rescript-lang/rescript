type t<'a>

@new external make: unit => t<'a> = "Set"
@new external fromArray: array<'a> => t<'a> = "Set"
@new external fromIterator: Stdlib_Iterator.t<'a> => t<'a> = "Set"

@get external size: t<'a> => int = "size"

@send external clear: t<'a> => unit = "clear"

@send external add: (t<'a>, 'a) => unit = "add"
@send external delete: (t<'a>, 'a) => bool = "delete"
@send external has: (t<'a>, 'a) => bool = "has"

@send external forEach: (t<'a>, 'a => unit) => unit = "forEach"

@send external values: t<'a> => Stdlib_Iterator.t<'a> = "values"

@send external difference: (t<'a>, t<'a>) => t<'a> = "difference"
@send external intersection: (t<'a>, t<'a>) => t<'a> = "intersection"
@send external union: (t<'a>, t<'a>) => t<'a> = "union"
@send external symmetricDifference: (t<'a>, t<'a>) => t<'a> = "symmetricDifference"
@send external isSubsetOf: (t<'a>, t<'a>) => bool = "isSubsetOf"
@send external isSupersetOf: (t<'a>, t<'a>) => bool = "isSupersetOf"
@send external isDisjointFrom: (t<'a>, t<'a>) => bool = "isDisjointFrom"

external toArray: t<'a> => array<'a> = "Array.from"

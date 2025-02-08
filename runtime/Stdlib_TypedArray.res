type t<'a>

@get_index external get: (t<'a>, int) => option<'a> = ""
@set_index external set: (t<'a>, int, 'a) => unit = ""

@get external buffer: t<'a> => Stdlib_ArrayBuffer.t = "buffer"
@get external byteLength: t<'a> => int = "byteLength"
@get external byteOffset: t<'a> => int = "byteOffset"

@send external setArray: (t<'a>, array<'a>) => unit = "set"
@send external setArrayFrom: (t<'a>, array<'a>, int) => unit = "set"

@get external length: t<'a> => int = "length"

@send external copyAllWithin: (t<'a>, ~target: int) => array<'a> = "copyWithin"
@send external copyWithinToEnd: (t<'a>, ~target: int, ~start: int) => array<'a> = "copyWithin"
@send
external copyWithin: (t<'a>, ~target: int, ~start: int, ~end: int) => array<'a> = "copyWithin"

@send external fillAll: (t<'a>, 'a) => t<'a> = "fill"
@send external fillToEnd: (t<'a>, 'a, ~start: int) => t<'a> = "fill"
@send external fill: (t<'a>, 'a, ~start: int, ~end: int) => t<'a> = "fill"

@send external reverse: t<'a> => unit = "reverse"
@send external toReversed: t<'a> => t<'a> = "toReversed"

@send external sort: (t<'a>, ('a, 'a) => Stdlib_Ordering.t) => unit = "sort"
@send external toSorted: (t<'a>, ('a, 'a) => Stdlib_Ordering.t) => t<'a> = "toSorted"

@send external with: (t<'a>, int, 'a) => t<'a> = "with"

@send external includes: (t<'a>, 'a) => bool = "includes"

@send external indexOf: (t<'a>, 'a) => int = "indexOf"
@send external indexOfFrom: (t<'a>, 'a, int) => int = "indexOf"

@send external joinWith: (t<'a>, string) => string = "join"

@send external lastIndexOf: (t<'a>, 'a) => int = "lastIndexOf"
@send external lastIndexOfFrom: (t<'a>, 'a, int) => int = "lastIndexOf"

@send external slice: (t<'a>, ~start: int, ~end: int) => t<'a> = "slice"
@send external sliceToEnd: (t<'a>, ~start: int) => t<'a> = "slice"
@send external copy: t<'a> => t<'a> = "slice"

@send external subarray: (t<'a>, ~start: int, ~end: int) => t<'a> = "subarray"
@send external subarrayToEnd: (t<'a>, ~start: int) => t<'a> = "subarray"

@send external toString: t<'a> => string = "toString"
@send external toLocaleString: t<'a> => string = "toLocaleString"

@send external every: (t<'a>, 'a => bool) => bool = "every"
@send external everyWithIndex: (t<'a>, ('a, int) => bool) => bool = "every"

@send external filter: (t<'a>, 'a => bool) => t<'a> = "filter"
@send external filterWithIndex: (t<'a>, ('a, int) => bool) => t<'a> = "filter"

@send external find: (t<'a>, 'a => bool) => option<'a> = "find"
@send external findWithIndex: (t<'a>, ('a, int) => bool) => option<'a> = "find"

@send external findIndex: (t<'a>, 'a => bool) => int = "findIndex"
@send external findIndexWithIndex: (t<'a>, ('a, int) => bool) => int = "findIndex"

@send external forEach: (t<'a>, 'a => unit) => unit = "forEach"
@send external forEachWithIndex: (t<'a>, ('a, int) => unit) => unit = "forEach"

@send external map: (t<'a>, 'a => 'b) => t<'b> = "map"
@send external mapWithIndex: (t<'a>, ('a, int) => 'b) => t<'b> = "map"

@send external reduce: (t<'a>, ('b, 'a) => 'b, 'b) => 'b = "reduce"
@send external reduceWithIndex: (t<'a>, ('b, 'a, int) => 'b, 'b) => 'b = "reduce"

@send external reduceRight: (t<'a>, ('b, 'a) => 'b, 'b) => 'b = "reduceRight"
@send external reduceRightWithIndex: (t<'a>, ('b, 'a, int) => 'b, 'b) => 'b = "reduceRight"

@send external some: (t<'a>, 'a => bool) => bool = "some"
@send external someWithIndex: (t<'a>, ('a, int) => bool) => bool = "some"

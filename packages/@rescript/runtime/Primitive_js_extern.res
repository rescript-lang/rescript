@unboxed
type null<+'a> = Value('a) | @as(null) Null

type undefined<+'a>

@unboxed type nullable<+'a> = Value('a) | @as(null) Null | @as(undefined) Undefined

type null_undefined<+'a> = nullable<'a>

external null: null<'a> = "%null"

external undefined: undefined<'a> = "%undefined"

external isNullable: nullable<'a> => bool = "%is_nullable"

external testAny: 'a => bool = "%is_nullable"

external typeof: 'a => string = "%typeof"

external eqNull: ('a, null<'a>) => bool = "%equal_null"

external eqUndefined: ('a, undefined<'a>) => bool = "%equal_undefined"

external eqNullable: ('a, nullable<'a>) => bool = "%equal_nullable"

external lt: ('a, 'a) => bool = "%unsafe_lt"

external le: ('a, 'a) => bool = "%unsafe_le"

external gt: ('a, 'a) => bool = "%unsafe_gt"

external ge: ('a, 'a) => bool = "%unsafe_ge"

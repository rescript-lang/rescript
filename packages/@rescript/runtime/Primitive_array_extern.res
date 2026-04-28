external length: array<'a> => int = "%array_length"
external getUnsafe: (array<'a>, int) => 'a = "%array_unsafe_get"
external setUnsafe: (array<'a>, int, 'a) => unit = "%array_unsafe_set"

@send external slice: (array<'a>, int, int) => array<'a> = "slice"
@send external concat: (array<'a>, array<'a>) => array<'a> = "concat"

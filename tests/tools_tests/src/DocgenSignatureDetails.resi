/** Labeled and optional parameters keep their parameter metadata. */
let labeledOptional: (~required: 'a, ~optional: array<'a>=?) => result<'a, string>

/** A callback remains one parameter instead of becoming outer parameters. */
let takesCallback: ('a => string) => bool

/** A tuple remains one return-type node. */
let returnsTuple: int => (string, int)

/** A returned function remains nested in the return type. */
let returnsFunction: int => string => bool

/** Less common type forms remain visible through an explicit fallback. */
let takesVariant: [#enabled | #count(int)] => unit

/** Non-functions do not receive function signature details. */
let constant: int

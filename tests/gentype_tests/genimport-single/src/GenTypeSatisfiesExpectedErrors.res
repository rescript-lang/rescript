/* This module intentionally contains cases that should cause TypeScript errors
 when the external imported Type mismatches the ReScript manifest type. */

@gentype.satisfies(("external-module", "Type"))
type numberT = int

@gentype
let useNumber = (x: numberT) => x

@gentype.satisfies(("external-module", "Type"))
type tupleT = (int, string)

@gentype
let useTuple = (x: tupleT) => x

@gentype.satisfies(("external-module", "Type"))
type arrayT = array<int>

@gentype
let useArray = (x: arrayT) => x

@gentype.satisfies(("external-module", "Type"))
type promiseT = Js.Promise.t<int>

@gentype
let usePromise = (x: promiseT) => x

@gentype.satisfies(("external-module", "Type"))
type nestedArrayT = array<array<int>>

@gentype
let useNestedArray = (x: nestedArrayT) => x

/* Positive case: string matches external Type=string */
@gentype.satisfies(("external-module", "Type"))
type stringT = string

@gentype
let useString = (x: stringT) => x

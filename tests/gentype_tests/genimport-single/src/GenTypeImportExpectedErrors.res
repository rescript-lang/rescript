/* This module intentionally contains cases that should cause TypeScript errors
 when the external imported Type mismatches the ReScript manifest type. */

@genType.import(("external-module", "Type"))
type numberT = int

@genType
let useNumber = (x: numberT) => x

@genType.import(("external-module", "Type"))
type tupleT = (int, string)

@genType
let useTuple = (x: tupleT) => x

@genType.import(("external-module", "Type"))
type arrayT = array<int>

@genType
let useArray = (x: arrayT) => x

@genType.import(("external-module", "Type"))
type promiseT = Js.Promise.t<int>

@genType
let usePromise = (x: promiseT) => x

@genType.import(("external-module", "Type"))
type nestedArrayT = array<array<int>>

@genType
let useNestedArray = (x: nestedArrayT) => x

/* Positive case: string matches external Type=string */
@genType.import(("external-module", "Type"))
type stringT = string

@genType
let useString = (x: stringT) => x

/***
A built-in object that serves as a namespace for globally-unique identifiers.

Compiles to a regular JavaScript Symbol.
*/

/**
Type representing a Symbol.
*/
type t

/**
`make(key)`

Makes a new unique Symbol value.

## Examples

```rescript
let _ Symbol.make("sym1")
->Symbol.description
->assertEqual(Some("sym1"))
```
*/
@val
external make: string => t = "Symbol"

@val @scope("Symbol")
external getFor: string => option<t> = "for"

@val @scope("Symbol")
external keyFor: t => option<string> = "keyFor"

@get
external description: t => option<string> = "description"

@send
external toString: t => string = "toString"

@val @scope("Symbol")
external asyncIterator: t = "asyncIterator"
@val @scope("Symbol")
external hasInstance: t = "hasInstance"
@val @scope("Symbol") external isConcatSpreadable: t = "isConcatSpreadable"
@val @scope("Symbol") external iterator: t = "iterator"
@val @scope("Symbol") external match: t = "match"
@val @scope("Symbol") external matchAll: t = "matchAll"
@val @scope("Symbol") external replace: t = "replace"
@val @scope("Symbol") external search: t = "search"
@val @scope("Symbol") external species: t = "species"
@val @scope("Symbol") external split: t = "split"
@val @scope("Symbol") external toPrimitive: t = "toPrimitive"
@val @scope("Symbol") external toStringTag: t = "toStringTag"
@val @scope("Symbol") external unscopables: t = "unscopables"

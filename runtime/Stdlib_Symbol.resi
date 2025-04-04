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
Symbol.make("sym1")
->Symbol.description
->assertEqual(Some("sym1"))
```
*/
@val
external make: string => t = "Symbol"

/**
`getFor(key)`

Searches for existing registered Symbols in the global Symbol registry with the given key and returns it if found. 
Otherwise a new Symbol gets created and registered with key.

## Examples

```rescript
Symbol.getFor("sym1")->assertEqual(Symbol.getFor("sym1"))
```
*/
@val
@scope("Symbol")
external getFor: string => option<t> = "for"

/**
`keyFor(key)`

Retrieves a shared Symbol key from the global Symbol registry for the given Symbol.

## Examples

```rescript
let globalSym = Symbol.getFor("sym1") // Global symbol

globalSym->Option.flatMap(Symbol.description)->assertEqual(Some("sym1"))
```
*/
@val
@scope("Symbol")
external keyFor: t => option<string> = "keyFor"

/**
`description`

Returns `Some(string)` containing the description of this symbol, or `None` if the symbol has no description.
## Examples

```rescript
let sym = Symbol.make("sym1")
Symbol.description(sym)->assertEqual(Some("sym1"))
```
*/
@get
external description: t => option<string> = "description"

/**
`toString`

// Returns a string representing this symbol value.

## Examples

```rescript
let sym = Symbol.make("sym1")

Symbol.toString(sym)->assertEqual("Symbol(sym1)")
```
*/
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

/**
  `ignore(symbol)` ignores the provided symbol and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

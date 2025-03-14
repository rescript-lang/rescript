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
Symbol.make("sym1")->assetEqual("sym1")
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
Symbol.getFor("foo")->assetEqual("foo")
```
*/
@val
external getFor: string => t = "Symbol.for"

/**
`keyFor(key)`

Retrieves a shared Symbol key from the global Symbol registry for the given Symbol.

## Examples

```rescript
let globalSym = Symbol.getFor("sym1") // Global symbol

Console.log(Symbol.keyFor(globalSym))
// Expected output: "sym1"

let localSym = Symbol.make("sym2") // Local symbol

Console.log(Symbol.keyFor(localSym))
// Expected output: undefined
```
*/
@val
external keyFor: t => option<string> = "Symbol.keyFor"

/**
`description`

Returns `Some(string)` containing the description of this symbol, or `None` if the symbol has no description.
## Examples

```rescript
let sym = Symbol.make("foo")
Console.log(sym->Symbol.description)
```
*/
@get
external description: t => option<string> = "description"

/**
`toString`

// Returns a string representing this symbol value.

## Examples

```rescript
let sym = Symbol.make("foo")
Console.log(sym->Symbol.toString) 
// Expected output: "Symbol(foo)"
```
*/
@send
external toString: t => string = "toString"

@val
external asyncIterator: t = "Symbol.asyncIterator"
@val
external hasInstance: t = "Symbol.hasInstance"
@val external isConcatSpreadable: t = "Symbol.isConcatSpreadable"
@val external iterator: t = "Symbol.iterator"
@val external match: t = "Symbol.match"
@val external matchAll: t = "Symbol.matchAll"
@val external replace: t = "Symbol.replace"
@val external search: t = "Symbol.search"
@val external species: t = "Symbol.species"
@val external split: t = "Symbol.split"
@val external toPrimitive: t = "Symbol.toPrimitive"
@val external toStringTag: t = "Symbol.toStringTag"
@val external unscopables: t = "Symbol.unscopables"

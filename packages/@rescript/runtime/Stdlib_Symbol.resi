/***
A built-in object that serves as a namespace for globally-unique identifiers.

Compiles to a regular JavaScript Symbol.
*/

/**
Type representing a Symbol.
*/
@notUndefined
type t

/**
`make(key)`

Makes a new unique Symbol value.

## Examples

```rescript
Symbol.make("sym1")->Symbol.description == Some("sym1")
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
Symbol.getFor("sym1") == Symbol.getFor("sym1")
```
*/
@val @scope("Symbol")
external getFor: string => option<t> = "for"

/**
`keyFor(key)`

Retrieves a shared Symbol key from the global Symbol registry for the given Symbol.

## Examples

```rescript
let globalSym = Symbol.getFor("sym1") // Global symbol

globalSym->Option.flatMap(Symbol.description) == Some("sym1")
```
*/
@val @scope("Symbol")
external keyFor: t => option<string> = "keyFor"

/**
`description`

Returns `Some(string)` containing the description of this symbol, or `None` if the symbol has no description.
## Examples

```rescript
let sym = Symbol.make("sym1")
Symbol.description(sym) == Some("sym1")
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

Symbol.toString(sym) == "Symbol(sym1)"
```
*/
@send
external toString: t => string = "toString"

/**
`asyncIterator` is the well-known symbol used by asynchronous iterables.

See [`Symbol.asyncIterator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/asyncIterator) on MDN.
*/
@val @scope("Symbol")
external asyncIterator: t = "asyncIterator"

/**
`hasInstance` is used by custom `instanceof` checks.

See [`Symbol.hasInstance`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/hasInstance) on MDN.
*/
@val @scope("Symbol")
external hasInstance: t = "hasInstance"

/**
`isConcatSpreadable` controls whether objects are flattened when passed to `Array.concat`.

See [`Symbol.isConcatSpreadable`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/isConcatSpreadable) on MDN.
*/
@val @scope("Symbol") external isConcatSpreadable: t = "isConcatSpreadable"

/**
`iterator` is returned by objects that can be iterated with `for..of`.

See [`Symbol.iterator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/iterator) on MDN.
*/
@val @scope("Symbol") external iterator: t = "iterator"

/**
`match` customises how values respond to `String.prototype.match`.

See [`Symbol.match`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/match) on MDN.
*/
@val @scope("Symbol") external match: t = "match"

/**
`matchAll` customises how values respond to `String.prototype.matchAll`.

See [`Symbol.matchAll`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/matchAll) on MDN.
*/
@val @scope("Symbol") external matchAll: t = "matchAll"

/**
`replace` customises how values respond to `String.prototype.replace`.

See [`Symbol.replace`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/replace) on MDN.
*/
@val @scope("Symbol") external replace: t = "replace"

/**
`search` customises how values respond to `String.prototype.search`.

See [`Symbol.search`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/search) on MDN.
*/
@val @scope("Symbol") external search: t = "search"

/**
`species` lets constructors return a different derived constructor when methods create new instances.

See [`Symbol.species`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/species) on MDN.
*/
@val @scope("Symbol") external species: t = "species"

/**
`split` customises how values respond to `String.prototype.split`.

See [`Symbol.split`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/split) on MDN.
*/
@val @scope("Symbol") external split: t = "split"

/**
`toPrimitive` customises how objects convert to primitive values.

See [`Symbol.toPrimitive`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/toPrimitive) on MDN.
*/
@val @scope("Symbol") external toPrimitive: t = "toPrimitive"

/**
`toStringTag` customises the default tag shown by `Object.prototype.toString`.

See [`Symbol.toStringTag`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/toStringTag) on MDN.
*/
@val @scope("Symbol") external toStringTag: t = "toStringTag"

/**
`unscopables` marks properties that should be skipped by `with`.

See [`Symbol.unscopables`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol/unscopables) on MDN.
*/
@val @scope("Symbol") external unscopables: t = "unscopables"

/**
  `ignore(symbol)` ignores the provided symbol and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

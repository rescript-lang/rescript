type t

@val external make: string => t = "Symbol"
@val external getFor: string => t = "Symbol.for"
@val external keyFor: t => option<string> = "Symbol.keyFor"

@val external asyncIterator: t = "Symbol.asyncIterator"
@val external hasInstance: t = "Symbol.hasInstance"
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

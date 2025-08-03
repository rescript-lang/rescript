type t<'a> = dict<'a>

@get_index external getUnsafe: (dict<'a>, string) => 'a = ""
@get_index external get: (dict<'a>, string) => option<'a> = ""
@set_index external set: (dict<'a>, string, 'a) => unit = ""
@val external delete: 'a => unit = "delete"

let delete = (dict, string) => {
  delete(get(dict, string))
}

@obj external make: unit => dict<'a> = ""

@val external fromArray: array<(string, 'a)> => dict<'a> = "Object.fromEntries"
@val external fromIterator: Stdlib_Iterator.t<(string, 'a)> => dict<'a> = "Object.fromEntries"

@val external toArray: dict<'a> => array<(string, 'a)> = "Object.entries"

@val external keysToArray: dict<'a> => array<string> = "Object.keys"

let size = dict => dict->keysToArray->Stdlib_Array.length

@val external valuesToArray: dict<'a> => array<'a> = "Object.values"

@val external assign: (dict<'a>, dict<'a>) => dict<'a> = "Object.assign"

@val external copy: (@as(json`{}`) _, dict<'a>) => dict<'a> = "Object.assign"

let forEach = (dict, f) => {
  dict->valuesToArray->Stdlib_Array.forEach(value => f(value))
}

@inline
let forEachWithKey = (dict, f) => {
  dict->keysToArray->Stdlib_Array.forEach(key => f(dict->getUnsafe(key), key))
}

let mapValues = (dict, f) => {
  let target = make()
  dict->forEachWithKey((value, key) => {
    target->set(key, f(value))
  })
  target
}

external has: (dict<'a>, string) => bool = "%dict_has"

external ignore: dict<'a> => unit = "%ignore"

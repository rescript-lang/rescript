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

@val external valuesToArray: dict<'a> => array<'a> = "Object.values"

@val external assign: (dict<'a>, dict<'a>) => dict<'a> = "Object.assign"

@val external copy: (@as(json`{}`) _, dict<'a>) => dict<'a> = "Object.assign"

// Use %raw to support for..in which is a ~10% faster than .forEach
let forEach: (dict<'a>, 'a => unit) => unit = %raw(`(dict, f) => {
  for (var i in dict) {
    f(dict[i]);
  }
}`)

// Use %raw to support for..in which is a ~10% faster than .forEach
let forEachWithKey: (dict<'a>, ('a, string) => unit) => unit = %raw(`(dict, f) => {
  for (var i in dict) {
    f(dict[i], i);
  }
}`)

// Use %raw to support for..in which is a ~10% faster than .forEach
let mapValues: (dict<'a>, 'a => 'b) => dict<'b> = %raw(`(dict, f) => {
  var target = {}, i;
  for (i in dict) {
    target[i] = f(dict[i]);
  }
  return target;
}`)

// Use %raw to support for..in which is a ~10% faster than Object.keys
let size: dict<'a> => int = %raw(`(dict) => {
  var size = 0, i;
  for (i in dict) {
    size++;
  }
  return size;
}`)

// Use %raw to support for..in which is a 2x faster than Object.keys
let isEmpty: dict<'a> => bool = %raw(`(dict) => {
  for (var _ in dict) {
    return false
  }
  return true
}`)

external has: (dict<'a>, string) => bool = "%dict_has"

external ignore: dict<'a> => unit = "%ignore"

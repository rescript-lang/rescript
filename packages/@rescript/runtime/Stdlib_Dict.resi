/***
A mutable dictionary with string keys.

Compiles to a regular JavaScript object.*/

/**
Type representing a dictionary of value `'a`.
*/
type t<'a> = dict<'a>

/**
`getUnsafe(dict, key)` Returns the `value` at the provided `key`.

This is _unsafe_, meaning it will return `undefined` value if `key` does not exist in `dict`.

Use `Dict.getUnsafe` only when you are sure the key exists (i.e. when iterating `Dict.keys` result).

## Examples
```rescript
let dict = dict{"key1": "value1", "key2": "value2"}
let value = dict->Dict.getUnsafe("key1")
Console.log(value) // value1
```
*/
@get_index
external getUnsafe: (dict<'a>, string) => 'a = ""

/**
Returns the value at the provided key, if it exists. Returns an option.

## Examples
```rescript
let dict = dict{"someKey": "someValue"}

switch dict->Dict.get("someKey") {
| None => Console.log("Nope, didn't have the key.")
| Some(value) => Console.log(value)
}
```

## Alternative: Pattern Matching with `dict{}`

For nested dictionary access, consider using the more concise `dict{}` pattern matching syntax:

```rescript
// More concise approach using pattern matching
let decodeImageUrl = (json: JSON.t) => {
  switch json {
  | JSON.Object(dict{"data": JSON.Object(dict{"image_url": JSON.String(url)})}) => Some(url)
  | _ => None
  }
}

// Alternative using Dict.get
let decodeImageUrl = (json: JSON.t) => {
  switch json {
  | JSON.Object(obj) =>
    switch Dict.get(obj, "data") {
    | Some(JSON.Object(data)) =>
      switch Dict.get(data, "image_url") {
      | Some(JSON.String(url)) => Some(url)
      | _ => None
      }
    | _ => None
    }
  | _ => None
  }
}
```
*/
@get_index
external get: (dict<'a>, string) => option<'a> = ""

/**
`set(dictionary, key, value)` sets the value at the provided key to the provided value.

## Examples
```rescript
let dict = Dict.make()

dict->Dict.set("someKey", "someValue")
```
*/
@set_index
external set: (dict<'a>, string, 'a) => unit = ""

/**
`delete(dictionary, key)` deletes the value at `key`, if it exists.

## Examples
```rescript
let dict = dict{"someKey": "someValue"}

dict->Dict.delete("someKey")
```
*/
let delete: (dict<'a>, string) => unit

/**
`make()` creates a new, empty dictionary.

## Examples
```rescript
let dict1: dict<int> = Dict.make() // You can annotate the type of the values of your dict yourself if you want

let dict2 = Dict.make() // Or you can let ReScript infer it via usage.
dict2->Dict.set("someKey", 12)
```
*/
@obj
external make: unit => dict<'a> = ""

/**
`fromArray(entries)` creates a new dictionary from the provided array of key/value pairs.

## Examples
```rescript
let dict = Dict.fromArray([("key1", "value1"), ("key2", "value2")])
```
*/
@val
external fromArray: array<(string, 'a)> => dict<'a> = "Object.fromEntries"

/**
`fromIterable(entries)` creates a new dictionary from the provided iterable of key/value pairs.

## Examples

```rescript
let iterable: Iterable.t<(string, int)> = [("first", 1), ("second", 2)]->Array.asIterable
iterable
->Dict.fromIterable
->Dict.valuesToArray == [1, 2]
```
*/
@val
external fromIterable: Stdlib_Iterable.t<(string, 'a)> => dict<'a> = "Object.fromEntries"

/**
`toArray(dictionary)` returns an array of all the key/value pairs of the dictionary.

## Examples
```rescript
let dict = Dict.make()
dict->Dict.set("someKey", 1)
dict->Dict.set("someKey2", 2)
let asArray = dict->Dict.toArray
Console.log(asArray) // Logs `[["someKey", 1], ["someKey2", 2]]` to the console
```
*/
@val
external toArray: dict<'a> => array<(string, 'a)> = "Object.entries"

/**
`keysToArray(dictionary)` returns an array of all the keys of the dictionary.

## Examples
```rescript
let dict = Dict.make()
dict->Dict.set("someKey", 1)
dict->Dict.set("someKey2", 2)
let keys = dict->Dict.keysToArray
Console.log(keys) // Logs `["someKey", "someKey2"]` to the console
```
*/
@val
external keysToArray: dict<'a> => array<string> = "Object.keys"

/**
`size(dictionary)` returns the number of key/value pairs in the dictionary.

## Examples
```rescript
let dict = Dict.make()
dict->Dict.size->assertEqual(0)

dict->Dict.set("someKey", 1)
dict->Dict.set("someKey2", 2)
dict->Dict.size->assertEqual(2)

// After deleting a key
dict->Dict.delete("someKey")
dict->Dict.size->assertEqual(1)
```
*/
let size: dict<'a> => int

/**
`isEmpty(dictionary)` returns `true` if the dictionary is empty (has no key/value pairs), `false` otherwise.

## Examples
```rescript
let emptyDict = Dict.make()
emptyDict->Dict.isEmpty->assertEqual(true)

let dict = Dict.make()
dict->Dict.set("someKey", 1)
dict->Dict.isEmpty->assertEqual(false)

// After clearing all keys
dict->Dict.delete("someKey")
dict->Dict.isEmpty->assertEqual(true)
```
*/
let isEmpty: dict<'a> => bool

/**
`valuesToArray(dictionary)` returns an array of all the values of the dictionary.

## Examples
```rescript
let dict = Dict.make()
dict->Dict.set("someKey", 1)
dict->Dict.set("someKey2", 2)
let values = dict->Dict.valuesToArray
Console.log(values) // Logs `[1, 2]` to the console
```
*/
@val
external valuesToArray: dict<'a> => array<'a> = "Object.values"

/**
`assign(dictionary1, dictionary2)` [shallowly](https://developer.mozilla.org/en-US/docs/Glossary/Shallow_copy) merges dictionary2 into dictionary1, and returns dictionary1.

Beware this will *mutate* dictionary1. If you're looking for a fresh merged dictionary instead, check out `Dict.concat`.

## Examples
```rescript
let dict1 = Dict.make()
dict1->Dict.set("firstKey", 1)
Console.log(dict1->Dict.keysToArray) // Logs `["firstKey"]`

let dict2 = Dict.make()
dict2->Dict.set("someKey", 2)
dict2->Dict.set("someKey2", 3)

let dict1 = dict1->Dict.assign(dict2)

Console.log(dict1->Dict.keysToArray) // Logs `["firstKey", "someKey", "someKey2"]`
```
*/
@val
external assign: (dict<'a>, dict<'a>) => dict<'a> = "Object.assign"

/**
`assignMany(target, sources)` [shallowly](https://developer.mozilla.org/en-US/docs/Glossary/Shallow_copy) merges each dictionary in `sources` into `target`, and returns `target`.

Beware this will *mutate* `target`. Later dictionaries overwrite earlier ones. If you're looking for a fresh merged dictionary instead, check out `Dict.concatMany`.

## Examples
```rescript
let target = dict{"firstKey": 1}
let result = target->Dict.assignMany([
  dict{"someKey": 2},
  dict{"someKey": 3, "someKey2": 4},
])

assertEqual(result, dict{"firstKey": 1, "someKey": 3, "someKey2": 4})
assertEqual(result === target, true)
```
*/
@variadic @val
external assignMany: (dict<'a>, array<dict<'a>>) => dict<'a> = "Object.assign"

/**
`concat(dictionary1, dictionary2)` [shallowly](https://developer.mozilla.org/en-US/docs/Glossary/Shallow_copy) merges `dictionary1` and `dictionary2` into a fresh dictionary, and returns the new dictionary.

Neither input dictionary is mutated. If both dictionaries contain the same key, the value from `dictionary2` overwrites the one from `dictionary1`.

## Examples
```rescript
let dict1 = dict{"firstKey": 1}
let dict2 = dict{"firstKey": 2, "someKey": 3}

let merged = dict1->Dict.concat(dict2)

assertEqual(dict1, dict{"firstKey": 1})
assertEqual(merged, dict{"firstKey": 2, "someKey": 3})
assertEqual(merged === dict1, false)
```
*/
@val
external concat: (@as(json`{}`) _, dict<'a>, dict<'a>) => dict<'a> = "Object.assign"

/**
`concatMany(target, sources)` [shallowly](https://developer.mozilla.org/en-US/docs/Glossary/Shallow_copy) merges `target` and each dictionary in `sources` into a fresh dictionary, and returns the new dictionary.

Neither `target` nor any dictionary in `sources` is mutated. Later dictionaries overwrite earlier ones when they contain the same key.

## Examples
```rescript
let target = dict{"firstKey": 1}
let merged = target->Dict.concatMany([
  dict{"someKey": 2},
  dict{"someKey": 3, "someKey2": 4},
])

assertEqual(target, dict{"firstKey": 1})
assertEqual(merged, dict{"firstKey": 1, "someKey": 3, "someKey2": 4})
assertEqual(merged === target, false)
```
*/
@variadic @val
external concatMany: (@as(json`{}`) _, dict<'a>, array<dict<'a>>) => dict<'a> = "Object.assign"

/**
`concatAll(dictionaries)` [shallowly](https://developer.mozilla.org/en-US/docs/Glossary/Shallow_copy) merges all dictionaries in `dictionaries` into a fresh dictionary, and returns the new dictionary.

None of the input dictionaries are mutated. Later dictionaries overwrite earlier ones when they contain the same key. If `dictionaries` is empty, an empty dictionary is returned.

## Examples
```rescript
let dict1 = dict{"firstKey": 1}
let dict2 = dict{"someKey": 2}
let dict3 = dict{"someKey": 3, "someKey2": 4}

let merged = Dict.concatAll([dict1, dict2, dict3])

assertEqual(dict1, dict{"firstKey": 1})
assertEqual(merged, dict{"firstKey": 1, "someKey": 3, "someKey2": 4})
```
*/
@variadic @val
external concatAll: (@as(json`{}`) _, array<dict<'a>>) => dict<'a> = "Object.assign"

/**
`copy(dictionary)` [shallowly copies](https://developer.mozilla.org/en-US/docs/Glossary/Shallow_copy) the provided dictionary to a new dictionary.

## Examples
```rescript
let dict = dict{"key1": "value1", "key2": "value2"}
let dict2 = dict->Dict.copy

// Both log `["key1", "key2"]` here.
Console.log2(dict->Dict.keysToArray, dict2->Dict.keysToArray)
```
*/
@val
external copy: (@as(json`{}`) _, dict<'a>) => dict<'a> = "Object.assign"

/**
`forEach(dictionary, f)` iterates through all values of the dict.

> Please note that this is *without the keys*, just the values. If you need the key as well, use `Dict.forEachWithKey`.

## Examples
```rescript
let dict = dict{"key1": "value1", "key2": "value2"}

dict->Dict.forEach(value => {
  Console.log(value)
})
```
*/
let forEach: (dict<'a>, 'a => unit) => unit

/**
`forEachWithKey(dictionary, f)` iterates through all values of the dict, including the key for each value.

## Examples
```rescript
let dict = dict{"key1": "value1", "key2": "value2"}

dict->Dict.forEachWithKey((value, key) => {
  Console.log2(value, key)
})
```
*/
let forEachWithKey: (dict<'a>, ('a, string) => unit) => unit

/**
`mapValues(dictionary, f)` returns a new dictionary with the same keys, and `f` applied to each value in the original dictionary.

## Examples

```rescript
let dict = dict{"key1": 1, "key2": 2}

dict->Dict.mapValues(v => v + 10)->Dict.toArray // [("key1", 11), ("key2", 12)]
dict->Dict.mapValues(v => Int.toString(v))->Dict.toArray // [("key1", "1"), ("key2", "2")]
```
*/
let mapValues: (dict<'a>, 'a => 'b) => dict<'b>

/**
`has(dictionary, "key")` returns true if the "key" is present in the dictionary.

Be aware that it uses the JavaScript `in` operator under the hood.

## Examples

```rescript
let dict = dict{"key1": Some(1), "key2": None}

dict->Dict.has("key1") == true
dict->Dict.has("key2") == true
dict->Dict.has("key3") == false
dict->Dict.has("toString") == true
```
*/
external has: (dict<'a>, string) => bool = "%dict_has"

/**
  `ignore(dict)` ignores the provided dict and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: dict<'a> => unit = "%ignore"

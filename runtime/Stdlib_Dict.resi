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
`fromIterator(entries)` creates a new dictionary from the provided iterator of key/value pairs.

## Examples

```rescript
let iterator: Iterator.t<(string, int)> = %raw(`
  (() => {
    var map1 = new Map();
    map1.set('first', 1);
    map1.set('second', 2);
    var iterator1 = map1[Symbol.iterator]();
    return iterator1;
  })()
`)
iterator
->Dict.fromIterator
->Dict.valuesToArray
->assertEqual([1, 2])
```
*/
@val
external fromIterator: Stdlib_Iterator.t<(string, 'a)> => dict<'a> = "Object.fromEntries"

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

Beware this will *mutate* dictionary1. If you're looking for a way to copy a dictionary, check out `Dict.copy`.

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

## Examples

```rescript
let dict = dict{"key1": Some(1), "key2": None}

dict->Dict.has("key1") // true
dict->Dict.has("key2") // true
dict->Dict.has("key3") // false
```
*/
let has: (dict<'a>, string) => bool

/**
  `ignore(dict)` ignores the provided dict and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: dict<'a> => unit = "%ignore"

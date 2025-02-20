/***
Bindings to the mutable JavaScript `Map`.

See [`Map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map) on MDN.
*/

/**
Type representing an instance of `Map`.
*/
type t<'k, 'v>

/**
Creates a new, mutable JavaScript `Map`. A `Map` can have any values as both keys and values.

See [`Map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map) on MDN.



## Examples
```rescript
`make()`
// You can annotate the type of your map if you want to
let myMap: Map.t<string, int> = Map.make()

// Or you can let ReScript infer what's in your map
let map = Map.make()
map->Map.set("lang", "ReScript") // Inferred as Map.t<string, string>
```

## Alternatives
A JavaScript `Map` is mutable. If you're looking for an immutable alternative, check out`Belt.Map`.
*/
@new
external make: unit => t<'k, 'v> = "Map"

/**
Turns an array of key/value pairs into a Map.

## Examples
```rescript
type languages = ReScript | JavaScript | TypeScript
let languageRank = [(ReScript, 1), (JavaScript, 2), (TypeScript, 3)]

let map = Map.fromArray(languageRank) // Map.t<languages, int>

switch map->Map.get(ReScript) {
| Some(1) => Console.log("Yay, ReScript is #1!")
| _ => Console.log("Uh-oh, something is _terribly_ wrong with this program... abort.")
}
```
*/
@new
external fromArray: array<('k, 'v)> => t<'k, 'v> = "Map"

/**
Turns an iterator in the shape of `('key, 'value)` into a `Map`.

## Examples

```rescript
// Let's pretend we have an interator in the correct shape
let iterator: Iterator.t<(string, string)> = %raw(`
  (() => {
    var map1 = new Map();

    map1.set('first', '1');
    map1.set('second', '2');

    var iterator1 = map1[Symbol.iterator]();
    return iterator1;
  })()
`)

iterator
->Map.fromIterator
->Map.size
->assertEqual(2)
```
*/
@new
external fromIterator: Stdlib_Iterator.t<('k, 'v)> => t<'k, 'v> = "Map"

/**
Returns the size, the number of key/value pairs, of the map.

## Examples
```rescript
let map = Map.make()

map->Map.set("someKey", "someValue")

let size = map->Map.size // 1
```
*/
@get
external size: t<'k, 'v> => int = "size"

/**
Clears all entries in the map.

## Examples
```rescript
let map = Map.make()

map->Map.set("someKey", "someValue")
map->Map.size // 1

map->Map.clear
map->Map.size // 0
```
*/
@send
external clear: t<'k, 'v> => unit = "clear"

/**
Iterates through all values of the map.

> Please note that this is *without the keys*, just the values. If you need the key as well, use `Map.forEachWithKey`.

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")
map->Map.set("someKey2", "someValue2")

map->Map.forEach(value => {
  Console.log(value)
})
```
*/
@send
external forEach: (t<'k, 'v>, 'v => unit) => unit = "forEach"

/**
Iterates through all values of the map, including the key for each value.

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")
map->Map.set("someKey2", "someValue2")

map->Map.forEachWithKey((value, key) => {
  Console.log2(value, key)
})
```
*/
@send
external forEachWithKey: (t<'k, 'v>, ('v, 'k) => unit) => unit = "forEach"

/**
Returns the value for a key, if a value exists at that key.

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")

switch map->Map.get("someKey") {
| None => Console.log("Nope, didn't have it.")
| Some(value) => Console.log2("Yay, had the value, and it's:", value)
}
```
*/
@send
external get: (t<'k, 'v>, 'k) => option<'v> = "get"

/**
Checks whether the map has a specific key.

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")

switch map->Map.has("someKey") {
| false => Console.log("Nope, didn't have it.")
| true => Console.log("Yay, we have the value!")
}
```
*/
@send
external has: (t<'k, 'v>, 'k) => bool = "has"

/**
Sets the provided `value` to the provided `key`.

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")
```
*/
@send
external set: (t<'k, 'v>, 'k, 'v) => unit = "set"

/**
Deletes the provided `key` and its value from the map. Returns a `bool` for whether the key existed, and was deleted.

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")
let didDeleteKey = map->Map.delete("someKey")
Console.log(didDeleteKey) // Logs `true` to the console, becuase the map had the key, so it was successfully deleted

let didDeleteKey = map->Map.delete("someNonExistantKey")
Console.log(didDeleteKey) // Logs `false` to the console, becuase the key did not exist
```
*/
@send
external delete: (t<'k, 'v>, 'k) => bool = "delete"

/**
Returns an iterator that holds all keys of the map.

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")
map->Map.set("anotherKey", "anotherValue")

let keys = map->Map.keys

// Logs the first key
Console.log(Iterator.next(keys).value)

// You can also turn the iterator into an array.
// Remember that an iterator consumes values. We'll need a fresh keys iterator to get an array of all keys, since we consumed a value via `next` above already.
Console.log(map->Map.keys->Iterator.toArray)
```
*/
@send
external keys: t<'k, 'v> => Stdlib_Iterator.t<'k> = "keys"

/**
Returns an iterator that holds all values of the map.

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")
map->Map.set("anotherKey", "anotherValue")

let values = map->Map.values

// Logs the first value
Console.log(Iterator.next(values).value)

// You can also turn the iterator into an array.
// Remember that an iterator consumes values. We'll need a fresh values iterator to get an array of all values, since we consumed a value via `next` above already.
Console.log(map->Map.values->Iterator.toArray)
```
*/
@send
external values: t<'k, 'v> => Stdlib_Iterator.t<'v> = "values"

/**
Returns an iterator that holds all entries of the map.
An entry is represented as a tuple of `('key, 'value)`,

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")
map->Map.set("anotherKey", "anotherValue")

let entries = map->Map.entries

// Logs the first value
Console.log(Iterator.next(entries).value)

// You can also turn the iterator into an array.
// Remember that an iterator consumes entries. We'll need a fresh entries iterator to get an array of all entries, since we consumed a value via `next` above already.
Console.log(map->Map.entries->Iterator.toArray)
```
*/
@send
external entries: t<'k, 'v> => Stdlib_Iterator.t<('k, 'v)> = "entries"

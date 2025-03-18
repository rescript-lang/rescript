/***
Bindings to JavaScript iterators.

See [`iterator protocols`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols) on MDN.
*/

/**
The type representing an iterator.
*/
type t<'a>

/**
The current value of an iterator.
*/
type value<'a> = {
  /**
  Whether there are more values to iterate on before the iterator is done.
  */
  done: bool,
  /**
  The value of this iteration, if any.
  */
  value: option<'a>,
}

/**
Returns the next value of the iterator, if any.

See [iterator protocols](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols) on MDN.

## Examples

```rescript
let iterator: Iterator.t<string> = %raw(`
  (() => {
    var array1 = ['a'];
    var iterator1 = array1[Symbol.iterator]();
    return iterator1
  })()
`)
(iterator->Iterator.next).done->assertEqual(false)
(iterator->Iterator.next).done->assertEqual(true)
```
*/
@send
external next: t<'a> => value<'a> = "next"

/**
Turns an iterator into an array of the remaining values.
Remember that each invocation of `next` of an iterator consumes a value. `Iterator.toArray` will consume all remaining values of the iterator and return them in an array to you.

See [iterator protocols](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols) on MDN.

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")
map->Map.set("someKey2", "someValue2")

// `Map.keys` returns all keys of the map as an iterator.
let mapKeysAsArray = map->Map.keys->Iterator.toArray

Console.log(mapKeysAsArray) // Logs ["someKey", "someKey2"] to the console.
```
*/
external toArray: t<'a> => array<'a> = "Array.from"

/**
`toArray(iterator)` turns `iterator` into an array of its remaining values, applying the provided mapper function on each item.
Remember that each invocation of `next` of an iterator consumes a value. `Iterator.toArrayWithMapper` will consume all remaining values of the iterator and return them in an array to you.

See [iterator protocols](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols) on MDN.

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")
map->Map.set("someKey2", "someValue2")

// `Map.keys` returns all keys of the map as an iterator.
let mapKeysAsArray = map
  ->Map.keys
  ->Iterator.toArrayWithMapper(key => key->String.length)

Console.log(mapKeysAsArray) // Logs [7, 8] to the console.
```
*/
external toArrayWithMapper: (t<'a>, 'a => 'b) => array<'b> = "Array.from"

/**
`forEach(iterator, fn)` consumes all values in the iterator and runs the callback `fn` for each value.

See [iterator protocols](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols) on MDN.

## Examples
```rescript
let iterator: Iterator.t<string> = %raw(`
  (() => {
    var array1 = ['a', 'b', 'c'];
    var iterator1 = array1[Symbol.iterator]();
    return iterator1
  })()
`)
iterator->Iterator.forEach(v => {
  switch v {
  | Some("a" | "b" | "c") => assert(true)
  | other =>
    other
    ->Option.isNone
    ->assertEqual(true)
  }
})
```
*/
let forEach: (t<'a>, option<'a> => unit) => unit

/**
  `ignore(iterator)` ignores the provided iterator and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t<'a> => unit = "%ignore"

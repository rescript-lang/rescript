/***
Bindings to JavaScript iterators.

See [`Iterator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator) on MDN.
*/

/**
The type representing an iterator.
*/
@notUndefined
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
(iterator->Iterator.next).done == false
(iterator->Iterator.next).done == true
```
*/
@send
external next: t<'a> => value<'a> = "next"

/**
Turns an iterator into an array of the remaining values.
Remember that each invocation of `next` of an iterator consumes a value. `Iterator.toArray` will consume all remaining values of the iterator and return them in an array to you.

See [Iterator.prototype.toArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/toArray) on MDN.

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")
map->Map.set("someKey2", "someValue2")

// `Map.keys` returns all keys of the map as an iterator.
let mapKeysAsArray = map->Map.keys->Iterator.toArray

mapKeysAsArray == ["someKey", "someKey2"]
```

## Remark

Since March 2025, this feature works across the latest devices and browser versions.
This feature might not work in older devices or browsers.
*/
@send
external toArray: t<'a> => array<'a> = "toArray"

/**
`toArray(iterator)` turns `iterator` into an array of its remaining values, applying the provided mapper function on each item.
Remember that each invocation of `next` of an iterator consumes a value. `Iterator.toArrayWithMapper` will consume all remaining values of the iterator and return them in an array to you.

See [Iterator.prototype.toArray](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/toArray) on MDN.

## Examples
```rescript
let map = Map.make()
map->Map.set("someKey", "someValue")
map->Map.set("someKey2", "someValue2")

// `Map.keys` returns all keys of the map as an iterator.
let mapKeysAsArray =
  map
  ->Map.keys
  ->Iterator.toArrayWithMapper(key => key->String.length)

mapKeysAsArray == [7, 8]
```
*/
external toArrayWithMapper: (t<'a>, 'a => 'b) => array<'b> = "Array.from"

/**
`forEach(iterator, fn)` consumes all values in the iterator and runs the callback `fn` for each value.

See [iterator protocols](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Iteration_protocols) on MDN.

## Examples
```rescript
let iterator: Iterator.t<string> = ["a", "b", "c"]->Array.values
let acc = ref("")
iterator->Iterator.forEach(v => {
  acc := acc.contents ++ v
})

acc.contents == "abc"
```

## Remark

Since March 2025, this feature works across the latest devices and browser versions.
This feature might not work in older devices or browsers.
*/
@send
external forEach: (t<'a>, 'a => unit) => unit = "forEach"

/**
  `ignore(iterator)` ignores the provided iterator and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t<'a> => unit = "%ignore"

/**
`drop(iterator, n)` returns a new iterator helper object that skips the given number of elements at the start of this iterator.

See [Iterator.prototype.drop](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/drop) on MDN.

## Examples
```rescript
let fibonacci: Iterator.t<int> = [1, 1, 2, 3, 5, 8, 13, 21]->Array.values

let seq = fibonacci->Iterator.drop(2)
seq->Iterator.next == {done: false, value: Some(2)}
seq->Iterator.next == {done: false, value: Some(3)}
```

## Remark

Since March 2025, this feature works across the latest devices and browser versions.
This feature might not work in older devices or browsers.
*/
@send
external drop: (t<'a>, int) => t<'a> = "drop"

/**
`every(iterator, fn)` tests whether all elements in the iterator pass the test implemented by the provided function.

See [Iterator.prototype.every](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/every) on MDN.

## Examples
```rescript
let fibonacci: Iterator.t<int> = [1, 1, 2, 3, 5, 8, 13, 21]->Array.values

let areAllEven = fibonacci->Iterator.every(n => n % 2 == 0)
areAllEven == false
```

## Remark

Since March 2025, this feature works across the latest devices and browser versions.
This feature might not work in older devices or browsers.
*/
@send
external every: (t<'a>, 'a => bool) => bool = "every"

/**
`filter(iterator, fn)` returns a new iterator helper object that contains the elements of the original iterator that pass the test implemented by the provided function.

See [Iterator.prototype.filter](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/filter) on MDN.

## Examples
```rescript
let fibonacci: Iterator.t<int> = [1, 1, 2, 3, 5, 8, 13, 21]->Array.values

let seq = fibonacci->Iterator.filter(n => n % 2 == 0)
seq->Iterator.next == {done: false, value: Some(2)}
seq->Iterator.next == {done: false, value: Some(8)}
```

## Remark

Since March 2025, this feature works across the latest devices and browser versions.
This feature might not work in older devices or browsers.
*/
@send
external filter: (t<'a>, 'a => bool) => t<'a> = "filter"

/**
`find(iterator, fn)` returns the value of the first element in the iterator that satisfies the provided testing function.

See [Iterator.prototype.find](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/find) on MDN.

## Examples
```rescript
let fibonacci: Iterator.t<int> = [1, 1, 2, 3, 5, 8, 13, 21]->Array.values

let seq = fibonacci->Iterator.find(n => n % 2 == 0)
seq == Some(2)
```

## Remark

Since March 2025, this feature works across the latest devices and browser versions.
This feature might not work in older devices or browsers.
*/
@send
external find: (t<'a>, 'a => bool) => option<'a> = "find"

/**
`flatMap(iterator, fn)` returns a new iterator helper object that contains the elements of the original iterator that pass the test implemented by the provided function.

See [Iterator.prototype.flatMap](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/flatMap) on MDN.

## Examples
```rescript
let map1 = Map.fromArray([("a", 1), ("b", 2), ("c", 3)])
let map2 = Map.fromArray([("d", 4), ("e", 5), ("f", 6)])

let letters =
  [map1, map2]
  ->Array.values
  ->Iterator.flatMap(m => Map.keys(m))
  ->Array.fromIterator
letters == ["a", "b", "c", "d", "e", "f"]
```

## Remark

Since March 2025, this feature works across the latest devices and browser versions.
This feature might not work in older devices or browsers.
 */
@send
external flatMap: (t<'a>, 'a => t<'b>) => t<'b> = "flatMap"

/**
`map(iterator, fn)` returns a new iterator helper object that yields elements of the iterator, each transformed by a mapping function.

See [Iterator.prototype.map](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/map) on MDN.

## Examples
```rescript
let map = Map.fromArray([("a", 1), ("b", 2), ("c", 3)])
let letters = map->Map.keys->Iterator.map(v => v->String.toUpperCase)->Array.fromIterator
letters == ["A", "B", "C"]
```

## Remark

Since March 2025, this feature works across the latest devices and browser versions.
This feature might not work in older devices or browsers.
*/
@send
external map: (t<'a>, 'a => 'b) => t<'b> = "map"

/**
`reduce(iterator, fn, initialValue)` applies a function against an accumulator and each element in the iterator (from left to right) to reduce it to a single value.

See [Iterator.prototype.reduce](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/reduce) on MDN.

## Examples
```rescript
let numbers: Iterator.t<int> = [1, 2, 3]->Array.values

let sum = numbers->Iterator.reduce((acc, n) => acc + n, ~initialValue=0)
sum == 6
```

## Remark

Since March 2025, this feature works across the latest devices and browser versions.
This feature might not work in older devices or browsers.
*/
@send
external reduce: (t<'a>, ('acc, 'a) => 'acc, ~initialValue: 'acc=?) => 'acc = "reduce"

/**
`some(iterator, fn)` The some() method of Iterator instances is similar to Array.some:
it tests whether at least one element produced by the iterator passes the test implemented by the provided function.
It returns a boolean value.

See [Iterator.prototype.some](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/some) on MDN.

## Examples
```rescript
let numbers: Iterator.t<int> = [1, 2, 3]->Array.values

let hasEven = numbers->Iterator.some(n => n % 2 == 0)
hasEven == true
```

## Remark

Since March 2025, this feature works across the latest devices and browser versions.
This feature might not work in older devices or browsers.
 */
@send
external some: (t<'a>, 'a => bool) => bool = "some"

/**
`take((iterator, n))` returns a new iterator helper object that contains the first `n` elements of this iterator.

See [Iterator.prototype.take](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Iterator/take) on MDN.

## Examples
```rescript
let fibonacci: Iterator.t<int> = [1, 1, 2, 3, 5, 8, 13, 21]->Array.values

let seq = fibonacci->Iterator.take(2)
seq->Iterator.next == {done: false, value: Some(1)}
seq->Iterator.next == {done: false, value: Some(1)}
seq->Iterator.next == {done: true, value: None}
```

## Remark

Since March 2025, this feature works across the latest devices and browser versions.
This feature might not work in older devices or browsers.
*/
@send
external take: (t<'a>, int) => t<'a> = "take"

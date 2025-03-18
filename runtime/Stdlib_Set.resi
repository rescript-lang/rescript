/***
Bindings to the mutable JavaScript `Set`.

See [`Set`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set) on MDN.
*/

/**
Type representing an instance of `Set`.
*/
type t<'a>

/**
Creates a new, mutable JavaScript `Set`. A `Set` is a collection of unique values.

See [`Set`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set) on MDN.



## Examples
```rescript
// You can annotate the type of your set if you want to
let mySet: Set.t<string> = Set.make()

// Or you can let ReScript infer what's in your Set
let set = Set.make()
set->Set.add("Fine name") // Inferred as Set.t<string>
```

## Alternatives
A JavaScript `Set` is mutable. If you're looking for an immutable alternative, check out `Belt.Set`.
*/
@new
external make: unit => t<'a> = "Set"

/**
Turns an array of values into a Set. Meaning only unique values are preserved.

## Examples
```rescript
type languages = ReScript | JavaScript | TypeScript
let languageRank = [ReScript, JavaScript, TypeScript]

let set = Set.fromArray(languageRank) // Set.t<languages>

switch set->Set.has(ReScript) {
| true => Console.log("Yay, ReScript is in there!")
| false => Console.log("Uh-oh, something is _terribly_ wrong with this program... abort.")
}
```
*/
@new
external fromArray: array<'a> => t<'a> = "Set"

/**
Turns an iterator into a `Set`.

## Examples

```rescript
// Let's pretend we have an interator
let iterator: Iterator.t<string> = %raw(`
  (() => {
    var array1 = ['a', 'b', 'c'];
    var iterator1 = array1[Symbol.iterator]();
    return iterator1
  })()
`)

iterator
->Set.fromIterator
->Set.size
->assertEqual(3)
```
*/
@new
external fromIterator: Stdlib_Iterator.t<'a> => t<'a> = "Set"

/**
Returns the size, the number of unique values, of the set.

See [`Set.prototype.size`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/size) on MDN.

## Examples
```rescript
let set = Set.make()

set->Set.add("someValue")
set->Set.add("someValue")
set->Set.add("someValue2")

let size = set->Set.size // 2
```
*/
@get
external size: t<'a> => int = "size"

/**
Clears all entries in the set.

See [`Set.clear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/clear) on MDN.

## Examples
```rescript
let set = Set.make()

set->Set.add("someKey")
set->Set.size // 1

set->Set.clear
set->Set.size // 0
```
*/
@send
external clear: t<'a> => unit = "clear"

/**
Adds a new value to the set.

See [`Set.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/add) on MDN.

## Examples
```rescript
let set = Set.make()
set->Set.add("someValue")
```
*/
@send
external add: (t<'a>, 'a) => unit = "add"

/**
Deletes the provided `value` from the set. Returns a `bool` for whether the value existed, and was deleted.

See [`Set.delete`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/delete) on MDN.

## Examples
```rescript
let set = Set.make()
set->Set.add("someValue")
let didDeleteValue = set->Set.delete("someValue")
Console.log(didDeleteValue) // Logs `true` to the console, becuase the set had the value, so it was successfully deleted

let didDeleteValue = set->Set.delete("someNonExistantKey")
Console.log(didDeleteValue) // Logs `false` to the console, becuase the value did not exist in the set
```
*/
@send
external delete: (t<'a>, 'a) => bool = "delete"

/**
Checks whether the set has a specific value.

See [`Set.has`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/has) on MDN.

## Examples
```rescript
let set = Set.make()
set->Set.add("someValue")

switch set->Set.has("someValue") {
| false => Console.log("Nope, didn't have it.")
| true => Console.log("Yay, we have the value!")
}
```
*/
@send
external has: (t<'a>, 'a) => bool = "has"

/**
Iterates through all values of the set.

See [`Set.forEach`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/forEach) on MDN.

## Examples
```rescript
let set = Set.make()
set->Set.add("someValue")
set->Set.add("someValue2")

set->Set.forEach(value => {
  Console.log(value)
})
```
*/
@send
external forEach: (t<'a>, 'a => unit) => unit = "forEach"

/**
Returns an iterator that holds all values of the set.

See [`Set.values`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/values) on MDN.

## Examples
```rescript
let set = Set.make()
set->Set.add("someValue")
set->Set.add("anotherValue")

let values = set->Set.values

// Logs the first value
Console.log(Iterator.next(values).value)

// You can also turn the iterator into an array.
// Remember that an iterator consumes values. We'll need a fresh values iterator to get an array of all values, since we consumed a value via `next` above already.
Console.log(set->Set.values->Iterator.toArray)
```
*/
@send
external values: t<'a> => Stdlib_Iterator.t<'a> = "values"

/** 
Returns a new set with the values of the set that are not in the other set.

See [`Set.difference`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/difference) on MDN.

## Examples
```rescript
let set1 = Set.fromArray(["apple", "orange", "banana"])
let set2 = Set.fromArray(["apple", "banana", "pear"])
set1->Set.difference(set2) // Set.fromArray(["orange"])
```
*/
@send
external difference: (t<'a>, t<'a>) => t<'a> = "difference"

/**
Returns a new set with the values containing the values which are in either the set, but not in both.

See [`Set.symmetricDifference`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/symmetricDifference) on MDN.

## Examples
```rescript
let set1 = Set.fromArray(["apple", "orange", "banana"])
let set2 = Set.fromArray(["apple", "banana", "pear"])
set1->Set.symmetricDifference(set2) // Set.fromArray(["orange", "pear"])
```

*/
@send
external symmetricDifference: (t<'a>, t<'a>) => t<'a> = "symmetricDifference"

/**
Returns a new set with the values containing the values which are in both the set and the other set.

See [`Set.intersection`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/intersection) on MDN.

## Examples
```rescript
let set1 = Set.fromArray(["apple", "orange", "banana"])
let set2 = Set.fromArray(["apple", "banana", "pear"])
set1->Set.intersection(set2) // Set.fromArray(["apple", "banana"])
```
*/
@send
external intersection: (t<'a>, t<'a>) => t<'a> = "intersection"

/**
Returns a bool indicating if this set has no elements in common with the given set.

See [`Set.isDisjointFrom`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/isDisjointFrom) on MDN.

## Examples
```rescript
let set1 = Set.fromArray(["apple", "orange", "banana"])
let set2 = Set.fromArray(["kiwi", "melon", "pear"])
set1->Set.isDisjointFrom(set2) // true
```
*/
@send
external isDisjointFrom: (t<'a>, t<'a>) => bool = "isDisjointFrom"

/**
Returns a bool indicating if the all values in the set are in the given set.

See [`Set.isSubsetOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/isSubsetOf) on MDN.

## Examples
```rescript
let set1 = Set.fromArray(["apple", "banana"])
let set2 = Set.fromArray(["apple", "banana", "pear"])
set1->Set.isSubsetOf(set2) // true
```
 */
@send
external isSubsetOf: (t<'a>, t<'a>) => bool = "isSubsetOf"

/**
Returns a bool indicating if the all values in the given set are in the set.

See [`Set.isSupersetOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/isSupersetOf) on MDN.

## Examples
```rescript
let set1 = Set.fromArray(["apple", "banana", "pear"])
let set2 = Set.fromArray(["apple", "banana"])
set1->Set.isSupersetOf(set2) // true
 ```
*/
@send
external isSupersetOf: (t<'a>, t<'a>) => bool = "isSupersetOf"

/**
 Returns a new set with the values of the set that are in both the set and the other set.

See [`Set.union`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/union) on MDN.

## Examples
```rescript
let set1 = Set.fromArray(["apple", "orange", "banana"])
let set2 = Set.fromArray(["apple", "banana", "pear"])
set1->Set.union(set2) // Set.fromArray(["apple", "orange", "banana", "pear"])
```
*/
@send
external union: (t<'a>, t<'a>) => t<'a> = "union"

/**
`toArray(set)` returns an array of all values of the set.

See [`Array.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/from) on MDN.

## Examples
```rescript
let set = Set.fromArray(["apple", "orange", "apple", "banana"])
set->Set.toArray // ["apple", "orange", "banana"]
```
*/
external toArray: t<'a> => array<'a> = "Array.from"

/**
  `ignore(set)` ignores the provided set and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t<'a> => unit = "%ignore"

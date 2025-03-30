/**
`make` create a new object that inherits the properties and methods from the standard built-in Object, such as `toString`. See [Object on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object)

## Examples

```rescript
let x = Object.make()
x->Object.keysToArray->Array.length // 0
x->Object.get("toString")->Option.isSome // true
```
*/
@obj
external make: unit => {..} = ""

/**
`is` determines if two objects are identical in all contexts. Objects, arrays, records, and other non-primitives are only identical if they reference the **exact** same object in memory. Primitives like ints, floats, and strings are identical if they have the same value. `+0` and `-0` are distinct. NaN is equal to itself. See [Object.is on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/is)

In most scenarios use `==` or `===` or the custom `equals` function (if provided) for the type.

## Examples

```rescript
Object.is(25, 13) // false
Object.is("abc", "abc") // true
Object.is(undefined, undefined) // true
Object.is(undefined, null) // false
Object.is(-0.0, 0.0) // false
Object.is(list{1, 2}, list{1, 2}) // false

Object.is([1, 2, 3], [1, 2, 3]) // false
[1, 2, 3] == [1, 2, 3] // true
[1, 2, 3] === [1, 2, 3] // false

let fruit = {"name": "Apple" }
Object.is(fruit, fruit) // true
Object.is(fruit, {"name": "Apple" }) // false
fruit == {"name": "Apple" } // true
fruit === {"name": "Apple" } // false
```
*/
@val
external is: ('a, 'a) => bool = "Object.is"

/**
`create` creates a new object, using an existing object as the prototype of the new object. See [Object.create on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/create)

**Note:** ReScript provides [first-class support for immutable objects](https://rescript-lang.org/docs/manual/latest/object) and [records](https://rescript-lang.org/docs/manual/latest/record). This is often safer and more convenient than using `create` and other functions in this module.

## Examples

```rescript
let x = {"fruit": "banana"}
let y = Object.create(x)
y->Object.get("fruit") // Some("banana")
```
*/
@val
external create: {..} => {..} = "Object.create"

@val external createWithProperties: ({..}, {..}) => {..} = "Object.create"

@val external createWithNull: (@as(json`null`) _, unit) => {..} = "Object.create"

@val external createWithNullAndProperties: (@as(json`null`) _, {..}) => {..} = "Object.create"

/**
`assign(target, source)` copies enumerable own properties from the source to the target, overwriting properties with the same name. It returns the modified target object. A deep clone is not created; properties are copied by reference.

**Warning:** ReScript provides compile-time support for type-safe access to JavaScript objects. This eliminates common errors such as accessing properties that do not exist, or using a property of type x as if it were a y. Using `assign` can bypass these safety checks and lead to run-time errors (if you are not careful). ReScript provides [first-class support for immutable objects](https://rescript-lang.org/docs/manual/latest/object) and [records](https://rescript-lang.org/docs/manual/latest/record). This is often safer and more convenient than using `assign` and other functions in this module.

See [Object.assign on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign) or [ECMAScript Language Specification](https://tc39.es/ecma262/multipage/fundamental-objects.html#sec-object.assign).

## Examples

```rescript
Object.assign({"a": 1}, {"a": 2}) // {"a": 2}
Object.assign({"a": 1, "b": 2}, {"a": 0}) // {"a": 0, "b": 2}
Object.assign({"a": 1}, {"a": null}) // {"a": null}
```
*/
@val
external assign: ({..}, {..}) => {..} = "Object.assign"

/**
`assignMany(target, sources)` copies enumerable own properties from each source to the target, overwriting properties with the same name. Later sources' properties overwrite earlier ones. It returns the modified target object. A deep clone is not created; properties are copied by reference.

**Note:** ReScript provides [first-class support for immutable objects](https://rescript-lang.org/docs/manual/latest/object), including spreading one object into another. This is often more convenient than using `assign` or `assignMany`.  

See [Object.assign on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign) or [ECMAScript Language Specification](https://tc39.es/ecma262/multipage/fundamental-objects.html#sec-object.assign).
*/
@variadic
@val
external assignMany: ({..}, array<{..}>) => {..} = "Object.assign"

@val external copy: (@as(json`{}`) _, {..} as 'a) => 'a = "Object.assign"

/**
`get` gets the value of a property by name. Returns `None` if the property does not exist or has the value `undefined`. Otherwise returns `Some`, including if the value is `null`.

## Examples

```rescript
{"a": 1}->Object.get("a") // Some(1)
{"a": 1}->Object.get("b") // None
{"a": undefined}->Object.get("a") // None
{"a": null}->Object.get("a") // Some(null)
{"a": 1}->Object.get("toString")->Option.isSome // true
```
*/
@get_index
external get: ({..}, string) => option<'a> = ""

/**
`getSymbol` gets the value of a property by symbol. Returns `None` if the property does not exist or has the value `undefined`. Otherwise returns `Some`, including if the value is `null`.

## Examples

```rescript
let fruit = Symbol.make("fruit")
let x = Object.make()
x->Object.setSymbol(fruit, "banana")
x->Object.getSymbol(fruit) // Some("banana")
```
*/
@get_index
external getSymbol: ({..}, Stdlib_Symbol.t) => option<'a> = ""

@get_index external getSymbolUnsafe: ({..}, Stdlib_Symbol.t) => 'a = ""

/**
`set(name, value)` assigns a value to the named object property, overwriting the previous value if any. See [Working with Objects on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Working_with_Objects#objects_and_properties)

## Examples

```rescript
{"a": 1}->Object.set("a", 2) // {"a": 2}
{"a": 1}->Object.set("a", None) // {"a": None}
{"a": 1}->Object.set("b", 2) // {"a": 1, "b": 2}
```
*/
@set_index
external set: ({..}, string, 'a) => unit = ""

@set_index external setSymbol: ({..}, Stdlib_Symbol.t, 'a) => unit = ""

/**
`keysToArray` returns an array of an object's own enumerable string-keyed property names. See [ECMAScript Language Specification](https://tc39.es/ecma262/multipage/fundamental-objects.html#sec-object.keys) 
or [Object.keys on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/keys).

## Examples

```rescript
{"a": 1, "b": 2}->Object.keysToArray // ["a", "b"]
{"a": None}->Object.keysToArray // ["a"]
Object.make()->Object.keysToArray // []
```
*/
@val
external keysToArray: {..} => array<string> = "Object.keys"

/**
`hasOwnProperty` determines whether the object has the specified property as its **own** property, as opposed to inheriting it. See [hasOwnProperty on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/hasOwnProperty)

## Examples

```rescript
let point = {"x": 1, "y": 2}
{"a": 1}->Object.hasOwnProperty("a") // true
{"a": 1}->Object.hasOwnProperty("b") // false
{"a": 1}->Object.hasOwnProperty("toString") // false
```
*/
@val
external hasOwnProperty: ({..}, string) => bool = "Object.prototype.hasOwnProperty.call"

/**
`seal` seals an object. Sealing an object prevents extensions and makes existing properties non-configurable. A sealed object has a fixed set of properties. Unlike `freeze`, values of existing properties can still be changed as long as they are writable. 

**Note:** `seal` returns the same object that was passed in; it does not create a copy. Any attempt to delete or add properties to a sealed object will fail, either silently or by throwing an error. 

See [ECMAScript Language Specification](https://tc39.es/ecma262/multipage/fundamental-objects.html#sec-object.seal) and [Object.seal on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/seal)

## Examples

```rescript
let point = {"x": 1, "y": 2}
point->Object.set("x", -7) // succeeds
point->Object.seal->ignore

try {
  point->Object.set("z", 9) // fails
} catch {
| Exn.Error(_) => assert(true)
| _ => assert(false)
}

point->Object.set("x", 13) // succeeds
```
*/
@val
external seal: ({..} as 'a) => 'a = "Object.seal"

/**
`preventExtensions` prevents new properties from being added to the object. It modifies the object (rather than creating a copy) and returns it.

See [ECMAScript Language Specification](https://tc39.es/ecma262/multipage/fundamental-objects.html#sec-object.preventextensions) and [Object.preventExtensions on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/preventExtensions)

## Examples

```rescript
let obj = {"a": 1}
obj->Object.set("b", 2) // succeeds
obj->Object.preventExtensions->ignore
try {
  obj->Object.set("c", 3) // fails
} catch {
| Exn.Error(_) => assert(true)
| _ => assert(false)
}
```
*/
@val
external preventExtensions: ({..} as 'a) => 'a = "Object.preventExtensions"

/**
`freeze` freezes an object. Freezing an object makes existing properties non-writable and prevents extensions. Once an object is frozen, new properties cannot be be added, existing properties cannot be removed, and their values cannot be changed.

**Note:** `freeze` returns the same object that was passed in; it does not create a frozen copy. Any attempt to change a frozen object will fail, either silently or by throwing an exception.

See [ECMAScript Language Specification](https://tc39.es/ecma262/multipage/fundamental-objects.html#sec-object.isfrozen) and [Object.isFrozen on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/isFrozen).

## Examples

```rescript
let obj = {"a": 1}
obj->Object.set("a", 2) // succeeds
obj->Object.freeze->ignore

try {
  obj->Object.set("a", 3) // fails
} catch {
| Exn.Error(_) => assert(true)
| _ => assert(false)
}
```
*/
@val
external freeze: ({..} as 'a) => 'a = "Object.freeze"

/**
`isSealed` determines if an object is sealed. A sealed object has a fixed set of properties.

See [ECMAScript Language Specification](https://tc39.es/ecma262/multipage/fundamental-objects.html#sec-object.issealed) and [Object.isSealed on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/isSealed)

## Examples

```rescript
let point = {"x": 1, "y": 3}->Object.seal
let pointIsSealed = point->Object.isSealed // true
let fruit = {"name": "Apple" }
let fruitIsSealed = fruit->Object.isSealed // false
 ```
*/
@val
external isSealed: 'a => bool = "Object.isSealed"

/**
`isFrozen` determines if an object is frozen. An object is frozen if an only if it is not extensible, all its properties are non-configurable, and all its data properties are non-writable.

See [ECMAScript Language Specification](https://tc39.es/ecma262/multipage/fundamental-objects.html#sec-object.isfrozen) and [Object.isFrozen on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/isFrozen).

## Examples

```rescript
let point = {"x": 1, "y": 3}->Object.freeze
let pointIsFrozen = point->Object.isFrozen // true
let fruit = {"name": "Apple" }
let fruitIsFrozen = fruit->Object.isFrozen // false
 ```
*/
@val
external isFrozen: 'a => bool = "Object.isFrozen"

/**
`isExtensible` determines if an object is extensible (whether it can have new properties added to it).

See [ECMAScript Language Specification](https://tc39.es/ecma262/multipage/fundamental-objects.html#sec-object.isextensible) and [Object.isExtensible on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/isExtensible)

## Examples

```rescript
let obj = {"a": 1}
obj->Object.isExtensible // true
obj->Object.preventExtensions->ignore
obj->Object.isExtensible // false
```
*/
@val
external isExtensible: 'a => bool = "Object.isExtensible"

/**
  `ignore(object)` ignores the provided object and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: {..} => unit = "%ignore"

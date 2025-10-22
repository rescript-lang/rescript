include Stdlib_Global

module Array = Stdlib_Array
module BigInt = Stdlib_BigInt
module Bool = Stdlib_Bool
module Console = Stdlib_Console
module DataView = Stdlib_DataView
module Date = Stdlib_Date
module Dict = Stdlib_Dict
module Exn = Stdlib_Exn
module Error = Stdlib_Error
module Float = Stdlib_Float
module Int = Stdlib_Int
module Intl = Stdlib_Intl
module JsError = Stdlib_JsError
module JsExn = Stdlib_JsExn
module JSON = Stdlib_JSON
module Lazy = Stdlib_Lazy
module List = Stdlib_List
module Math = Stdlib_Math
module Null = Stdlib_Null
module Nullable = Stdlib_Nullable
module Object = Stdlib_Object
module Option = Stdlib_Option
module Ordering = Stdlib_Ordering
module Pair = Stdlib_Pair
module Promise = Stdlib_Promise
module RegExp = Stdlib_RegExp
module Result = Stdlib_Result
module String = Stdlib_String
module Symbol = Stdlib_Symbol
module Type = Stdlib_Type

module Iterator = Stdlib_Iterator
module AsyncIterator = Stdlib_AsyncIterator
module Map = Stdlib_Map
module WeakMap = Stdlib_WeakMap
module Set = Stdlib_Set
module WeakSet = Stdlib_WeakSet

module ArrayBuffer = Stdlib_ArrayBuffer
module TypedArray = Stdlib_TypedArray
module Float32Array = Stdlib_Float32Array
module Float64Array = Stdlib_Float64Array
module Int8Array = Stdlib_Int8Array
module Int16Array = Stdlib_Int16Array
module Int32Array = Stdlib_Int32Array
module Uint8Array = Stdlib_Uint8Array
module Uint16Array = Stdlib_Uint16Array
module Uint32Array = Stdlib_Uint32Array
module Uint8ClampedArray = Stdlib_Uint8ClampedArray
module BigInt64Array = Stdlib_BigInt64Array
module BigUint64Array = Stdlib_BigUint64Array

// Type aliases for convenience
type date = Date.t
type null<+'a> = Primitive_js_extern.null<'a>
type undefined<+'a> = Primitive_js_extern.undefined<'a>
type nullable<+'a> = Primitive_js_extern.nullable<'a>
@deprecated("Use Lazy.t instead")
type lazy_t<+'a> = Lazy.t<'a>

@deprecated("Use rescript-webapi instead") @val external window: Dom.window = "window"
@deprecated("Use rescript-webapi instead") @val external document: Dom.document = "document"
/**
`globalThis` gives you the host global object (`window` in browsers, `global` in Node, etc.).
You can reach shared globals from any runtime without special checks.

## Examples

**Note:** These are demonstrative examples. In real code, prefer writing your own type-safe bindings.
See [`Bind to Global JS Values`](https://rescript-lang.org/docs/manual/v12.0.0/bind-to-global-js-values).


```rescript
typeof(globalThis["setTimeout"]) == #function

globalThis["myAppName"] = "SuperApp";
globalThis["myAppName"] == "SuperApp"
```
*/
@val external globalThis: {..} = "globalThis"

/**
`import(value)` dynamically import a value or function from a ReScript
module. The import call will return a `promise`, resolving to the dynamically loaded
value.

## Examples

`Array.res` file:

```rescript
@send external indexOf: (array<'a>, 'a) => int = "indexOf"

let indexOfOpt = (arr, item) =>
  switch arr->indexOf(item) {
  | -1 => None
  | index => Some(index)
  }
```
In other file you can import the `indexOfOpt` value defined in `Array.res`

```rescript
let main = async () => {
  let indexOfOpt = await import(Array.indexOfOpt)
  let index = indexOfOpt([1, 2], 2)
  Console.log(index)
}
```

Compiles to:

```javascript
async function main() {
  var add = await import("./Array.mjs").then(function(m) {
    return m.indexOfOpt;
  });
  var index = indexOfOpt([1, 2], 2);
  console.log(index);
}
```
*/
external import: 'a => promise<'a> = "%import"

/**
`panic(message)` throws a JavaScript `Error` prefixed with `Panic!`.
Call it when something went wrong and the program should stop right away.

## Examples

```rescript
let caught = try panic("Invariant violated") catch {
| JsExn(err) => JsExn.message(err)->Option.getOrThrow
}

caught == "Panic! Invariant violated"
```
*/
let panic = JsError.panic

/**
`assertEqual(a, b)` check if `a` is equal `b`. If not throw a panic exception

## Examples

```rescript
list{1, 2}->List.tailExn == list{2}
```
*/
let assertEqual = (a, b) => {
  if a != b {
    assert(false)
  }
}

/**
`null` returns the JavaScript `null` value as a `nullable<'a>`.
Use the `Nullable` helpers to convert it into an `option` or to read the value.

## Examples

```rescript
null->Nullable.toOption == None
```
*/
external null: nullable<'a> = "#null"
/**
`undefined` returns the JavaScript `undefined` value as a `nullable<'a>`.
Use the `Nullable` helpers to convert it into an `option` or to read the value.

## Examples

```rescript
undefined->Nullable.toOption == None
```
*/
external undefined: nullable<'a> = "#undefined"
/**
`typeof(value)` exposes JavaScript's `typeof` operator and returns a `Type.t` enum.
It helps you inspect values that come from JavaScript APIs.

## Examples

```rescript
typeof(1) == #number
typeof("a") == #string
```
*/
external typeof: 'a => Type.t = "#typeof"

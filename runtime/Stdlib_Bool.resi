/***
Functions for interacting with JavaScript booleans.
See: [`Boolean`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Boolean)
*/

/**
Type representing a boolean.
*/
type t = bool

/**
Converts a boolean to a string.

## Examples
```rescript
Bool.toString(true)->assertEqual("true")
Bool.toString(false)->assertEqual("false")
```
*/
let toString: bool => string

/**
Converts a string to a boolean.

## Examples
```rescript
Bool.fromString("true")->assertEqual(Some(true))
Bool.fromString("false")->assertEqual(Some(false))
Bool.fromString("notAValidBoolean")->assertEqual(None)
```
*/
let fromString: string => option<bool>

/**
Converts a string to a boolean.
Throws an `Invalid_argument` exception if the string is not a valid boolean.

## Examples
```rescript
Bool.fromStringOrThrow("true")->assertEqual(true)
Bool.fromStringOrThrow("false")->assertEqual(false)
switch Bool.fromStringOrThrow("notAValidBoolean") {
| exception Invalid_argument(_) => assert(true)
| _ => assert(false)
}
```
*/
let fromStringOrThrow: string => bool

/**
Converts a string to a boolean.
Beware, this function will throw an `Invalid_argument` exception 
if the string is not a valid boolean.

## Examples
```rescript
Bool.fromStringExn("true")->assertEqual(true)
Bool.fromStringExn("false")->assertEqual(false)
switch Bool.fromStringExn("notAValidBoolean") {
| exception Invalid_argument(_) => assert(true)
| _ => assert(false)
}
```
*/
@deprecated("Use `fromStringOrThrow` instead")
let fromStringExn: string => bool

external compare: (bool, bool) => Stdlib_Ordering.t = "%compare"

external equal: (bool, bool) => bool = "%equal"

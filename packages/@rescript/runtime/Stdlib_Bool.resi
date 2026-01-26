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
Bool.toString(true) == "true"
Bool.toString(false) == "false"
```
*/
let toString: bool => string

/**
Converts a string to a boolean.

## Examples
```rescript
Bool.fromString("true") == Some(true)
Bool.fromString("false") == Some(false)
Bool.fromString("notAValidBoolean") == None
```
*/
let fromString: string => option<bool>

/**
Converts a string to a boolean.
Throws an `Invalid_argument` exception if the string is not a valid boolean.

## Examples
```rescript
Bool.fromStringOrThrow("true") == true
Bool.fromStringOrThrow("false") == false
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
Bool.fromStringExn("true") == true
Bool.fromStringExn("false") == false
switch Bool.fromStringExn("notAValidBoolean") {
| exception Invalid_argument(_) => assert(true)
| _ => assert(false)
}
```
*/
@deprecated({
  reason: "Use `fromStringOrThrow` instead",
  migrate: Bool.fromStringOrThrow(),
})
let fromStringExn: string => bool

/**
Compares two booleans, returns an `Ordering.t` value.

## Examples
```rescript
Bool.compare(true, true) == Ordering.equal
Bool.compare(false, false) == Ordering.equal
Bool.compare(true, false) == Ordering.greater
Bool.compare(false, true) == Ordering.less
```
*/
external compare: (bool, bool) => Stdlib_Ordering.t = "%compare"

/**
Checks if two booleans are equal and have the same value.

## Examples
```rescript
Bool.equal(true, true) == true
Bool.equal(false, false) == true
Bool.equal(true, false) == false
Bool.equal(false, true) == false
```
*/
external equal: (bool, bool) => bool = "%equal"

/**
  `ignore(bool)` ignores the provided boolean and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: bool => unit = "%ignore"

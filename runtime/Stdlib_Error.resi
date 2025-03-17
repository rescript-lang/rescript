/***
Functions for working with JavaScript exceptions.

See [`Error`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error) on MDN.
*/

/** Represents a JavaScript exception. */
type t = Stdlib_Exn.t

let fromException: exn => option<t>

/**
Turns an `Error.t` into an `exn`.

## Examples
```rescript
let error = Error.make("Something went wrong.")

let asExn = error->Error.toException // `asExn` is now type `exn`
```
*/
external toException: t => exn = "%identity"

/**
`stack(error)` retrieves the `stack` property of the error, if it exists. The stack is a list of what functions were called, and what files they are defined in, prior to the error happening.

See [`Error.prototype.stack`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/Stack) on MDN.

## Example
```rescript
let error = Error.make("error")
Console.log(error->Error.stack) // Logs `stack` if it exists on `someError`
```
*/
@get
external stack: t => option<string> = "stack"

/**
`message(error)` retrieves the `message` property of the error, if it exists.

See [`Error.prototype.message`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/message) on MDN.

## Example
```rescript
let error = Error.SyntaxError.make("Some message here")
Console.log(error->Error.message) // Logs "Some message here" to the console
```
*/
@get
external message: t => option<string> = "message"

/**
`name(error)` retrieves the `name` property of the error, if it exists.

See [`Error.prototype.name`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/name) on MDN.

## Example
```rescript
let error = Error.SyntaxError.make("Some message here")
Console.log(error->Error.name) // Logs "SyntaxError" to the console
```
*/
@get
external name: t => option<string> = "name"

/**
`fileName(error)` retrieves the `fileName` property of the error, if it exists.

See [`Error.prototype.fileName`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/fileName) on MDN.
*/
@get
external fileName: t => option<string> = "fileName"

/**
`make(message)` creates a new error, setting its `message` to the provided value.

See [`Error`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/Error) on MDN.

## Example
```rescript
let error = Error.make("Some message here")
Console.log(error->Error.message) // Logs "Some message here" to the console
Console.log(error->Error.name) // Logs "Error" to the console, because this is a regular error
```
*/
@new
external make: string => t = "Error"

module EvalError: {
  /**
  Creates a new `EvalError` with the provided `message`.

  See [`EvalError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/EvalError) on MDN.
  */
  @new
  external make: string => t = "EvalError"
}
module RangeError: {
  /**
  Creates a new `RangeError` with the provided `message`.

  See [`RangeError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RangeError) on MDN.
  */
  @new
  external make: string => t = "RangeError"
}
module ReferenceError: {
  /**
  Creates a new `ReferenceError` with the provided `message`.

  See [`ReferenceError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ReferenceError) on MDN.
  */
  @new
  external make: string => t = "ReferenceError"
}
module SyntaxError: {
  /**
  Creates a new `SyntaxError` with the provided `message`.

  See [`SyntaxError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SyntaxError) on MDN.
  */
  @new
  external make: string => t = "SyntaxError"
}
module TypeError: {
  /**
  Creates a new `TypeError` with the provided `message`.

  See [`TypeError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypeError) on MDN.
  */
  @new
  external make: string => t = "TypeError"
}
module URIError: {
  /**
  Creates a new `URIError` with the provided `message`.

  See [`URIError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/URIError) on MDN.
  */
  @new
  external make: string => t = "URIError"
}

/**
Raises (throws in JavaScript language) the provided `Error.t`, which will stop execution.

## Examples
```rescript
let error = Error.make("Everything is upside down.")

if 5 > 10 {
  error->Error.raise
} else {
  Console.log("Phew, sanity still rules.")
}
```
*/
external raise: t => 'a = "%raise"

/**
Raises a panic exception with the given message.

A panic exception is a native JavaScript exception that is not intended to be caught and
handled. Compared to a ReScript exception this will give a better stack trace and
debugging experience.

## Examples

```rescript
try {
  Error.panic("Uh oh. This was unexpected!")
} catch {
| Exn.Error(obj) =>
  switch Exn.message(obj) {
  | Some(m) => assert(m == "Panic! Uh oh. This was unexpected!")
  | None => assert(false)
  }
| _ => assert(false)
}
```
*/
let panic: string => 'a

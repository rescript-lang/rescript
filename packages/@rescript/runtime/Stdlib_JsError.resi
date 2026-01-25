/***
Functions for working with JavaScript errors.

See [`Error`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error) on MDN.
*/

/** Represents a JavaScript error. */
@notUndefined
type t

/**
`stack(error)` retrieves the `stack` property of the error, if it exists. The stack is a list of what functions were called, and what files they are defined in, prior to the error happening.

See [`Error.prototype.stack`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/Stack) on MDN.

## Example
```rescript
let error = JsError.make("error")
Console.log(error->JsError.stack) // Logs `stack` if it exists on `someError`
```
*/
@get
external stack: t => option<string> = "stack"

/**
`message(error)` retrieves the `message` property of the error.

See [`Error.prototype.message`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/message) on MDN.

## Example
```rescript
let error = JsError.SyntaxError.make("Some message here")
error->JsError.message == "Some message here"
```
*/
@get
external message: t => string = "message"

/**
`name(error)` retrieves the `name` property of the error.

See [`Error.prototype.name`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/name) on MDN.

## Example
```rescript
let error = JsError.SyntaxError.make("Some message here")
error->JsError.name == "SyntaxError"
```
*/
@get
external name: t => string = "name"

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
let error = JsError.make("Some message here")
error->JsError.message == "Some message here"
error->JsError.name == "Error"
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

  /**
  Creates a new `EvalError` with the provided `message` and throws it.

  `JsError.EvalError.throwWithMessage("message")` is equivalent to `JsError.EvalError.make("message")->JsError.throw`.

  See [`EvalError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/EvalError) on MDN.
  */
  let throwWithMessage: string => 'a
}
module RangeError: {
  /**
  Creates a new `RangeError` with the provided `message`.

  See [`RangeError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RangeError) on MDN.
  */
  @new
  external make: string => t = "RangeError"

  /**
  Creates a new `RangeError` with the provided `message` and throws it.
  
  `JsError.RangeError.throwWithMessage("message")` is equivalent to `JsError.RangeError.make("message")->JsError.throw`.

  See [`RangeError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RangeError) on MDN.
  */
  let throwWithMessage: string => 'a
}
module ReferenceError: {
  /**
  Creates a new `ReferenceError` with the provided `message`.

  See [`ReferenceError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ReferenceError) on MDN.
  */
  @new
  external make: string => t = "ReferenceError"

  /**
  Creates a new `ReferenceError` with the provided `message` and throws it.

  `JsError.ReferenceError.throwWithMessage("message")` is equivalent to `JsError.ReferenceError.make("message")->JsError.throw`.

  See [`ReferenceError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ReferenceError) on MDN.
  */
  let throwWithMessage: string => 'a
}
module SyntaxError: {
  /**
  Creates a new `SyntaxError` with the provided `message`.

  See [`SyntaxError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SyntaxError) on MDN.
  */
  @new
  external make: string => t = "SyntaxError"

  /**
  Creates a new `SyntaxError` with the provided `message` and throws it.
  
  `JsError.SyntaxError.throwWithMessage("message")` is equivalent to `JsError.SyntaxError.make("message")->JsError.throw`.
  
  See [`SyntaxError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SyntaxError) on MDN.
  */
  let throwWithMessage: string => 'a
}
module TypeError: {
  /**
  Creates a new `TypeError` with the provided `message`.

  See [`TypeError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypeError) on MDN.
  */
  @new
  external make: string => t = "TypeError"

  /**
  Creates a new `TypeError` with the provided `message` and throws it.

  `JsError.TypeError.throwWithMessage("message")` is equivalent to `JsError.TypeError.make("message")->JsError.throw`.

  See [`TypeError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypeError) on MDN.
  */
  let throwWithMessage: string => 'a
}
module URIError: {
  /**
  Creates a new `URIError` with the provided `message`.

  See [`URIError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/URIError) on MDN.
  */
  @new
  external make: string => t = "URIError"

  /**
  Creates a new `URIError` with the provided `message` and throws it.

  `JsError.URIError.throwWithMessage("message")` is equivalent to `JsError.URIError.make("message")->JsError.throw`.

  See [`URIError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/URIError) on MDN.
  */
  let throwWithMessage: string => 'a
}

/**
Throws the given error, terminating execution unless caught by a surrounding try/catch block.

## Examples

```rescript
let error = JsError.make("Everything is upside down.")

if 5 > 10 {
  JsError.throw(error)
} else {
  Console.log("Phew, sanity still rules.")
}
```
*/
external throw: t => 'a = "%raise"

/**
Creates a new `Error` with the provided `message` and throws it.

`JsError.throwWithMessage("message")` is equivalent to `JsError.make("message")->JsError.throw`.
*/
let throwWithMessage: string => 'a

/**
Throws a panic exception with the given message.

A panic exception is a native JavaScript exception that is not intended to be caught and
handled. Compared to a ReScript exception this will give a better stack trace and
debugging experience.

## Examples

```rescript
try {
  JsError.panic("Uh oh. This was unexpected!")
} catch {
| JsExn(obj) =>
  switch JsExn.message(obj) {
  | Some(m) => assert(m == "Panic! Uh oh. This was unexpected!")
  | None => assert(false)
  }
| _ => assert(false)
}
```
*/
let panic: string => 'a

/**
Casts a `JsError.t` to a `JsExn.t`. 

This is useful when you want to compare a JS exception and a JS error.
*/
external toJsExn: t => Stdlib_JsExn.t = "%identity"

/**
  `ignore(error)` ignores the provided error and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

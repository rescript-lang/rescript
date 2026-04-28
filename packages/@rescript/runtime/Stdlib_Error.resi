/***
Functions for working with JavaScript exceptions.

See [`Error`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error) on MDN.
*/

/** Represents a JavaScript exception. */
@deprecated({
  reason: "Use `JsError.t` instead",
  migrate: %replace.type(: JsError.t),
})
type t = Stdlib_Exn.t

@deprecated({
  reason: "Use `JsExn.fromException` instead",
  migrate: JsExn.fromException(),
})
let fromException: exn => option<t>

/**
Turns an `Error.t` into an `exn`.

## Examples
```rescript
let error = Error.make("Something went wrong.")

let asExn = error->Error.toException // `asExn` is now type `exn`
```
*/
@deprecated("Use functions from `JsExn` instead")
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
@deprecated({
  reason: "Use `JsError.stack` instead",
  migrate: JsError.stack(),
})
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
@deprecated({
  reason: "Use `JsError.message` instead",
  migrate: JsError.message(),
})
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
@deprecated({
  reason: "Use `JsError.name` instead",
  migrate: JsError.name(),
})
@get
external name: t => option<string> = "name"

/**
`fileName(error)` retrieves the `fileName` property of the error, if it exists.

See [`Error.prototype.fileName`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/fileName) on MDN.
*/
@deprecated({
  reason: "Use `JsError.fileName` instead",
  migrate: JsError.fileName(),
})
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
@deprecated({
  reason: "Use `JsError.make` instead",
  migrate: JsError.make(),
})
@new
external make: string => t = "Error"

module EvalError: {
  /**
  Creates a new `EvalError` with the provided `message`.

  See [`EvalError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/EvalError) on MDN.
  */
  @deprecated({
    reason: "Use `JsError.EvalError.make` instead",
    migrate: JsError.EvalError.make(),
  })
  @new
  external make: string => t = "EvalError"
}
module RangeError: {
  /**
  Creates a new `RangeError` with the provided `message`.

  See [`RangeError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RangeError) on MDN.
  */
  @deprecated({
    reason: "Use `JsError.RangeError.make` instead",
    migrate: JsError.RangeError.make(),
  })
  @new
  external make: string => t = "RangeError"
}
module ReferenceError: {
  /**
  Creates a new `ReferenceError` with the provided `message`.

  See [`ReferenceError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ReferenceError) on MDN.
  */
  @deprecated({
    reason: "Use `JsError.ReferenceError.make` instead",
    migrate: JsError.ReferenceError.make(),
  })
  @new
  external make: string => t = "ReferenceError"
}
module SyntaxError: {
  /**
  Creates a new `SyntaxError` with the provided `message`.

  See [`SyntaxError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SyntaxError) on MDN.
  */
  @deprecated({
    reason: "Use `JsError.SyntaxError.make` instead",
    migrate: JsError.SyntaxError.make(),
  })
  @new
  external make: string => t = "SyntaxError"
}
module TypeError: {
  /**
  Creates a new `TypeError` with the provided `message`.

  See [`TypeError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypeError) on MDN.
  */
  @deprecated({
    reason: "Use `JsError.TypeError.make` instead",
    migrate: JsError.TypeError.make(),
  })
  @new
  external make: string => t = "TypeError"
}
module URIError: {
  /**
  Creates a new `URIError` with the provided `message`.

  See [`URIError`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/URIError) on MDN.
  */
  @deprecated({
    reason: "Use `JsError.URIError.make` instead",
    migrate: JsError.URIError.make(),
  })
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
@deprecated({
  reason: "`raise` has been renamed to `throw` to align with JavaScript vocabulary. Please use `JsError.throw` instead",
  migrate: JsError.throw(),
})
external raise: t => 'a = "%raise"

/**
Throws the given exception, terminating execution unless caught by a surrounding try/catch block.

## Examples

```rescript
let error = Error.make("Everything is upside down.")

if 5 > 10 {
  Error.throw(error)
} else {
  Console.log("Phew, sanity still rules.")
}
```
*/
@deprecated({
  reason: "Use `JsError.throw` instead",
  migrate: JsError.throw(),
})
external throw: t => 'a = "%raise"

/**
Throws a panic exception with the given message.

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
@deprecated({
  reason: "Use `JsError.panic` instead",
  migrate: JsError.panic(),
})
let panic: string => 'a

/**
  `ignore(error)` ignores the provided error and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
@deprecated({
  reason: "Use `JsError.ignore` instead",
  migrate: JsError.ignore(),
})
external ignore: t => unit = "%ignore"

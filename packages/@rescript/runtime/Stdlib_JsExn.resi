/***
Provide utilities for dealing with JS exceptions.

JS exceptions can be of any type, even though they *should* be of type `Error` of one of its subclasses.

See [`throw`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/throw) on MDN.

*/

/** Represents a JS exception */
type t = unknown

let fromException: exn => option<t>

/**
`anyToExnInternal(obj)` will take any value `obj` and wrap it
in a JsExn if given value is not an exn already. If
`obj` is an exn, it will return `obj` without any changes.

This function is mostly useful for cases where you want to unify a type of a value
that potentially is either exn, a JS error, or any other JS value really (e.g. for
a value passed to a Promise.catch callback)

**IMPORTANT**: This is an internal API and may be changed / removed any time in the future.
*/
external anyToExnInternal: 'a => exn = "%wrap_exn"

/**
`stack(jsExn)` retrieves the `stack` property of the exception, if it exists. The stack is a list of what functions were called, and what files they are defined in, prior to the error happening.

See [`Error.prototype.stack`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/Stack) on MDN.

## Example
```rescript
try {JsError.make("error")->JsError.throw} catch {
| JsExn(e) => Console.log(JsExn.stack(e)) // Logs `stack`
| _ => assert(false)
}
```
*/
let stack: t => option<string>

/**
`message(error)` retrieves the `message` property of the error, if it exists.

See [`Error.prototype.message`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/message) on MDN.

## Example
```rescript
try {JsError.SyntaxError.throwWithMessage("Some message here")} catch {
| JsExn(e) => e->JsExn.message->Option.getExn == "Some message here"
| _ => assert(false)
}
```
*/
let message: t => option<string>

/**
`name(error)` retrieves the `name` property of the error, if it exists.

See [`Error.prototype.name`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/name) on MDN.

## Example
```rescript
try {JsError.SyntaxError.throwWithMessage("Some message here")} catch {
| JsExn(e) => e->JsExn.name->Option.getExn == "SyntaxError"
| _ => assert(false)
}
```
*/
let name: t => option<string>

/**
`fileName(error)` retrieves the `fileName` property of the error, if it exists.

See [`Error.prototype.fileName`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/fileName) on MDN.
*/
let fileName: t => option<string>

/**
Throws the given value, terminating execution unless caught by a surrounding try/catch block.

This is meant to be used when a JS API is based on throwing values that are not of type `Error` or its subclasses.
*/
external throw: 'a => 'b = "%raise"

/**
  `ignore(jsExn)` ignores the provided JS exception and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

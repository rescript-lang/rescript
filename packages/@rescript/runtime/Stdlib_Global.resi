/***
Bindings to functions available in the global JavaScript scope.
*/

/**
An `id` representing a timeout started via `setTimeout`.

See [`setTimeout`](https://developer.mozilla.org/en-US/docs/Web/API/setTimeout) on MDN.
*/
type timeoutId

/**
`setTimeout(callback, durationInMilliseconds)` starts a timer that will execute `callback` after `durationInMilliseconds`.

See [`setTimeout`](https://developer.mozilla.org/en-US/docs/Web/API/setTimeout) on MDN.

## Examples

```rescript
// Log to the console after 200 milliseconds.
let timeoutId = setTimeout(() => {
  Console.log("This prints in 200 ms.")
}, 200)
```
*/
@val
external setTimeout: (unit => unit, int) => timeoutId = "setTimeout"

/**
`setTimeoutFloat(callback, durationInMilliseconds)` starts a timer that will execute `callback` after `durationInMilliseconds`.

The same as `setTimeout`, but allows you to pass a `float` instead of an `int` for the duration.

See [`setTimeout`](https://developer.mozilla.org/en-US/docs/Web/API/setTimeout) on MDN.

## Examples

```rescript
// Log to the console after 200 milliseconds.
let timeoutId = setTimeoutFloat(() => {
  Console.log("This prints in 200 ms.")
}, 200.)
```
*/
@val
external setTimeoutFloat: (unit => unit, float) => timeoutId = "setTimeout"

/**
`clearTimeout(timeoutId)` clears a scheduled timeout if it hasn't already executed.

See [`clearTimeout`](https://developer.mozilla.org/en-US/docs/Web/API/clearTimeout) on MDN.

## Examples

```rescript
let timeoutId = setTimeout(() => {
  Console.log("This prints in 2 seconds.")
}, 2000)

// Clearing the timeout right away, before 2 seconds has passed, means that the above callback logging to the console will never run.
clearTimeout(timeoutId)
```
*/
@val
external clearTimeout: timeoutId => unit = "clearTimeout"

/**
An `id` representing an interval started via `setInterval`.

See [`setInterval`](https://developer.mozilla.org/en-US/docs/Web/API/setInterval) on MDN.
*/
type intervalId

/**
`setInterval(callback, intervalInMilliseconds)` starts an interval that will execute `callback` every `durationInMilliseconds` milliseconds.

See [`setInterval`](https://developer.mozilla.org/en-US/docs/Web/API/setInterval) on MDN.

## Examples

```rescript
// Log to the console ever 200 ms (200 milliseconds).
let intervalId = setInterval(() => {
  Console.log("This prints every 200 ms.")
}, 200)

let timeoutId = setTimeout(() => {
  clearInterval(intervalId)
}, 500)
```
*/
@val
external setInterval: (unit => unit, int) => intervalId = "setInterval"

/**
`setIntervalFloat(callback, intervalInMilliseconds)` starts an interval that will execute `callback` every `durationInMilliseconds` milliseconds.

The same as `setInterval`, but allows you to pass a `float` instead of an `int` for the duration.

See [`setInterval`](https://developer.mozilla.org/en-US/docs/Web/API/setInterval) on MDN.

## Examples

```rescript
// Log to the console ever 2 seconds (200 milliseconds).
let intervalId = setIntervalFloat(() => {
  Console.log("This prints every 200 ms")
}, 200.)

// Stop the interval after 500 ms
let timeoutId = setTimeoutFloat(() => {
  clearInterval(intervalId)
}, 500.0)
```
*/
@val
external setIntervalFloat: (unit => unit, float) => intervalId = "setInterval"

/**
`clearInterval(intervalId)` clears a scheduled interval.

See [`clearInterval`](https://developer.mozilla.org/en-US/docs/Web/API/clearInterval) on MDN.

## Examples

```rescript
let intervalId = setInterval(() => {
  Console.log("This prints in 100 ms")
}, 100)

// Stop the interval after 500 ms
let timeoutId = setTimeout(() => {
  clearInterval(intervalId)
}, 500)
```
*/
@val
external clearInterval: intervalId => unit = "clearInterval"

/**
Encodes a URI by replacing characters in the provided string that aren't valid in a URL.

This is intended to operate on full URIs, so it encodes fewer characters than what `encodeURIComponent` does.
If you're looking to encode just parts of a URI, like a query parameter, prefer `encodeURIComponent`.

See [`encodeURI`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURI) on MDN.

## Examples
```rescript
Console.log(encodeURI("https://rescript-lang.org?array=[someValue]"))
// Logs "https://rescript-lang.org?array=%5BsomeValue%5D" to the console.
```

*/
@val
external encodeURI: string => string = "encodeURI"

/**
Decodes a previously encoded URI back to a regular string.

This is intended to operate on full URIs, so it decodes fewer characters than what `decodeURIComponent` does.
If you're looking to decode just parts of a URI, like a query parameter, prefer `decodeURIComponent`.

See [`decodeURI`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURI) on MDN.

## Examples
```rescript
Console.log(decodeURI("https://rescript-lang.org?array=%5BsomeValue%5D"))
// Logs "https://rescript-lang.org?array=[someValue]" to the console.
```
*/
@val
external decodeURI: string => string = "decodeURI"

/**
Encodes a string so it can be used as part of a URI.

See [`encodeURIComponent`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent) on MDN.

## Examples
```rescript
Console.log(encodeURIComponent("array=[someValue]"))
// Logs "array%3D%5BsomeValue%5D" to the console.
```
*/
@val
external encodeURIComponent: string => string = "encodeURIComponent"

/**
Decodes a previously URI encoded string back to its original form.

See [`decodeURIComponent`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/decodeURIComponent) on MDN.

## Examples
```rescript
Console.log(decodeURIComponent("array%3D%5BsomeValue%5D"))
// Logs "array=[someValue]" to the console.
```
*/
@val
external decodeURIComponent: string => string = "decodeURIComponent"

module TimeoutId: {
  /**
    `ignore(timeoutId)` ignores the provided timeoutId and returns unit.

    This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
    without having to store or process it further.
*/
  external ignore: timeoutId => unit = "%ignore"
}

module IntervalId: {
  /**
    `ignore(intervalId)` ignores the provided intervalId and returns unit.

    This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
    without having to store or process it further.
  */
  external ignore: intervalId => unit = "%ignore"
}

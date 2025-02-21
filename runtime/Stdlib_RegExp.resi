/***
Functions for handling RegExp's in ReScript.

See [`RegExp`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp) on MDN.
*/

/**
Type representing an instantiated `RegExp`.
*/
type t

module Result: {
  /**
  Type representing the result of a `RegExp` execution.
  */
  type t = array<option<string>>

  /**
  `fullMatch(regExpResult)` returns the full string that matched in this result.

  ## Examples
  ```rescript
  // Match the first two words separated by a space
  let regexp = RegExp.fromString("(\\w+) (\\w+)")

  switch regexp->RegExp.exec("ReScript is pretty cool, right?") {
  | None => Console.log("Nope, no match...")
  | Some(result) => Console.log(result->RegExp.Result.fullMatch) // Prints the full string that matched, "ReScript is"
  }
  ```
  */
  @get_index
  external fullMatch: (t, @as(0) _) => string = ""

  /**
  `matches(regExpResult)` returns all matches for `regExpResult`.

  ## Examples
  ```rescript
  // Match the first two words separated by a space
  let regexp = RegExp.fromString("(\\w+) (\\w+)")

  // This below will log "ReScript" and "is" to the console.
  switch regexp->RegExp.exec("ReScript is pretty cool, right?") {
  | None => Console.log("Nope, no match...")
  | Some(result) => switch result->RegExp.Result.matches {
    | [firstWord, secondWord] => Console.log2(firstWord, secondWord)
    | _ => Console.log("Didn't find exactly two words...")
    }
  }
  ```
  */
  @send
  external matches: (t, @as(1) _) => array<string> = "slice"
  @get external index: t => int = "index"

  /**
  `input(regExpResult)` returns the full input string that was passed to what produced the `RegExp.Result.t`.

  ## Examples
  ```rescript
  // Match the first two words separated by a space
  let regexp = RegExp.fromString("(\\w+) (\\w+)")

  // This below will log the full input string "ReScript is pretty cool, right?" to the console.
  switch regexp->RegExp.exec("ReScript is pretty cool, right?") {
  | None => Console.log("Nope, no match...")
  | Some(result) => Console.log(result->RegExp.Result.input)
  }
  ```
  */
  @get
  external input: t => string = "input"
}

/**
`fromString(string)` creates a `RegExp.t` from the provided string. This can then be used to match on strings using `RegExp.exec`.

See [`RegExp`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/RegExp) on MDN.

## Examples
```rescript
// Match the first word in a sentence
let regexp = RegExp.fromString("\\w+")

switch regexp->RegExp.exec("ReScript is pretty cool, right?") {
| None => Console.log("Nope, no match...")
| Some(result) => Console.log(result->RegExp.Result.fullMatch) // Prints "ReScript"
}
```
*/
@new
external fromString: string => t = "RegExp"

/**
`fromStringWithFlags(string)` creates a `RegExp.t` from the provided string, using the provided `flags`. This can then be used to match on strings using `RegExp.exec`.

See [`RegExp parameters`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/RegExp#parameters) on MDN.

## Examples
```rescript
// Match the first word in a sentence
let regexp = RegExp.fromStringWithFlags("\\w+", ~flags="g")

switch regexp->RegExp.exec("ReScript is pretty cool, right?") {
| None => Console.log("Nope, no match...")
| Some(result) => Console.log(result->RegExp.Result.fullMatch) // Prints "ReScript"
}
```
*/
@new
external fromStringWithFlags: (string, ~flags: string) => t = "RegExp"

/**
`test(regexp, string)` tests whether the provided `regexp` matches on the provided string.

See [`RegExp.test`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/test) on MDN.

## Examples
```rescript
// Match the first word in a sentence
let regexp = RegExp.fromString("\\w+")

if regexp->RegExp.test("ReScript is cool!") {
  Console.log("Yay, there's a word in there.")
}
```
*/
@send
external test: (t, string) => bool = "test"

/**
`exec(regexp, string)` executes the provided regexp on the provided string, optionally returning a `RegExp.Result.t` if the regexp matches on the string.

See [`RegExp.exec`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/exec) on MDN.

## Examples
```rescript
// Match the first word in a sentence
let regexp = RegExp.fromString("\\w+")

switch regexp->RegExp.exec("ReScript is pretty cool, right?") {
| None => Console.log("Nope, no match...")
| Some(result) => Console.log(result->RegExp.Result.fullMatch) // Prints "ReScript"
}
```
*/
@return(nullable)
@send
external exec: (t, string) => option<Result.t> = "exec"

/**
`lastIndex(regexp)` returns the index the next match will start from.

See [`RegExp.lastIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/lastIndex) on MDN.

## Examples
```rescript
// Match the first word in a sentence
let regexp = RegExp.fromString("\\w+")
let someStr = "Many words here."

Console.log(regexp->RegExp.lastIndex) // Logs `0` to the console

regexp->RegExp.exec(someStr)->ignore

Console.log(regexp->RegExp.lastIndex) // Logs `4` to the console
```
*/
@get
external lastIndex: t => int = "lastIndex"

/**
`setLastIndex(regexp, index)` set the index the next match will start from.

See [`RegExp.lastIndex`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/lastIndex) on MDN.

## Examples
```rescript
// Match the first word in a sentence
let regexp = RegExp.fromString("\\w+")
let someStr = "Many words here."

regexp->RegExp.setLastIndex(4)
regexp->RegExp.exec(someStr)->ignore

Console.log(regexp->RegExp.lastIndex) // Logs `10` to the console
```
*/
@set
external setLastIndex: (t, int) => unit = "lastIndex"

/**
`ignoreCase(regexp)` returns whether the ignore case (`i`) flag is set on this `RegExp`.

See [`RegExp.ignoreCase`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/ignoreCase) on MDN.

## Examples
```rescript
let regexp1 = RegExp.fromStringWithFlags("\\w+", ~flags="g")
Console.log(regexp1->RegExp.ignoreCase) // Logs `false`, since `i` is not set

let regexp2 = RegExp.fromStringWithFlags("\\w+", ~flags="i")
Console.log(regexp2->RegExp.ignoreCase) // Logs `true`, since `i` is set
```
*/
@get
external ignoreCase: t => bool = "ignoreCase"

/**
`global(regexp)` returns whether the global (`g`) flag is set on this `RegExp`.

See [`RegExp.global`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/global) on MDN.

## Examples
```rescript
let regexp1 = RegExp.fromStringWithFlags("\\w+", ~flags="g")
Console.log(regexp1->RegExp.global) // Logs `true`, since `g` is set

let regexp2 = RegExp.fromStringWithFlags("\\w+", ~flags="i")
Console.log(regexp2->RegExp.global) // Logs `false`, since `g` is not set
```
*/
@get
external global: t => bool = "global"

/**
`multiline(regexp)` returns whether the multiline (`m`) flag is set on this `RegExp`.

See [`RegExp.multiline`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/multiline) on MDN.

## Examples
```rescript
let regexp1 = RegExp.fromStringWithFlags("\\w+", ~flags="g")
Console.log(regexp1->RegExp.multiline) // Logs `false`, since `m` is not set

let regexp2 = RegExp.fromStringWithFlags("\\w+", ~flags="mi")
Console.log(regexp2->RegExp.multiline) // Logs `true`, since `m` is set
```
*/
@get
external multiline: t => bool = "multiline"

/**
`source(regexp)` returns the source text for this `RegExp`, without the two forward slashes (if present), and without any set flags.

See [`RegExp.source`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/source) on MDN.

## Examples
```rescript
let regexp = RegExp.fromStringWithFlags("\\w+", ~flags="g")
Console.log(regexp->RegExp.source) // Logs `\w+`, the source text of the `RegExp`
```
*/
@get
external source: t => string = "source"

/**
`sticky(regexp)` returns whether the sticky (`y`) flag is set on this `RegExp`.

See [`RegExp.sticky`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/sticky) on MDN.

## Examples
```rescript
let regexp1 = RegExp.fromStringWithFlags("\\w+", ~flags="g")
Console.log(regexp1->RegExp.unicode) // Logs `false`, since `y` is not set

let regexp2 = RegExp.fromStringWithFlags("\\w+", ~flags="my")
Console.log(regexp2->RegExp.unicode) // Logs `true`, since `y` is set
```
*/
@get
external sticky: t => bool = "sticky"

/**
`unicode(regexp)` returns whether the unicode (`y`) flag is set on this `RegExp`.

See [`RegExp.unicode`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp/unicode) on MDN.

## Examples
```rescript
let regexp1 = RegExp.fromStringWithFlags("\\w+", ~flags="g")
Console.log(regexp1->RegExp.unicode) // Logs `false`, since `u` is not set

let regexp2 = RegExp.fromStringWithFlags("\\w+", ~flags="mu")
Console.log(regexp2->RegExp.unicode) // Logs `true`, since `u` is set
```
*/
@get
external unicode: t => bool = "unicode"

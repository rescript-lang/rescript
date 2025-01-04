/**
   Since [others] depend on this file, its public mli files **should not
   export types** introduced here, otherwise it would cause 
   conflicts here.

   If the type exported here is also exported in modules from others,
   you will get a type not equivalent.
*/
@deprecated("Do not use. This will be removed in v13")
external /* Internal */

__unsafe_cast: 'a => 'b = "%identity"

/* Exceptions */

external raise: exn => 'a = "%raise"

@deprecated("Use custom exception instead")
let failwith = s => raise(Failure(s))

@deprecated("Use custom exception instead")
let invalid_arg = s => raise(Invalid_argument(s))

@deprecated("Use custom exception instead") exception Exit

/* Composition operators */

external \"|>": ('a, 'a => 'b) => 'b = "%revapply"
external \"@@": ('a => 'b, 'a) => 'b = "%apply"

/* Debugging */

external __LOC__: string = "%loc_LOC"
external __FILE__: string = "%loc_FILE"
external __LINE__: int = "%loc_LINE"
external __MODULE__: string = "%loc_MODULE"
external __POS__: (string, int, int, int) = "%loc_POS"

external __LOC_OF__: 'a => (string, 'a) = "%loc_LOC"
external __LINE_OF__: 'a => (int, 'a) = "%loc_LINE"
external __POS_OF__: 'a => ((string, int, int, int), 'a) = "%loc_POS"

/* Unified operations */

external \"~+": 'a => 'a = "%plus"
external \"~-": 'a => 'a = "%neg"

external \"+": ('a, 'a) => 'a = "%add"
external \"-": ('a, 'a) => 'a = "%sub"
external \"*": ('a, 'a) => 'a = "%mul"
external \"/": ('a, 'a) => 'a = "%div"
external \"%": ('a, 'a) => 'a = "%mod"
external mod: ('a, 'a) => 'a = "%mod"

/* Comparisons */
/* Note: Later comparisons will be converted to unified operations too */

external \"=": ('a, 'a) => bool = "%equal"
external \"<>": ('a, 'a) => bool = "%notequal"
external \"<": ('a, 'a) => bool = "%lessthan"
external \">": ('a, 'a) => bool = "%greaterthan"
external \"<=": ('a, 'a) => bool = "%lessequal"
external \">=": ('a, 'a) => bool = "%greaterequal"
external compare: ('a, 'a) => int = "%compare"
external min: ('a, 'a) => 'a = "%min"
external max: ('a, 'a) => 'a = "%max"
external \"==": ('a, 'a) => bool = "%eq"
external \"!=": ('a, 'a) => bool = "%noteq"

/* Boolean operations */

external not: bool => bool = "%boolnot"

external \"&&": (bool, bool) => bool = "%sequand"

external \"||": (bool, bool) => bool = "%sequor"

/* Integer operations */

external succ: int => int = "%succint"
external pred: int => int = "%predint"

@deprecated("Use Core instead. This will be removed in v13")
let abs = x =>
  if x >= 0 {
    x
  } else {
    -x
  }

external land: (int, int) => int = "%andint"
external lor: (int, int) => int = "%orint"
external lxor: (int, int) => int = "%xorint"

let lnot = x => lxor(x, -1)

external lsl: (int, int) => int = "%lslint"
external lsr: (int, int) => int = "%lsrint"
external asr: (int, int) => int = "%asrint"

@deprecated("Use Core instead. This will be removed in v13")
let max_int = lsr(-1, 1)

@deprecated("Use Core instead. This will be removed in v13")
let min_int =
  max_int + 1

/* Floating-point operations */

external \"~-.": float => float = "%negfloat"
external \"~+.": float => float = "%identity"
external \"+.": (float, float) => float = "%addfloat"
external \"-.": (float, float) => float = "%subfloat"
external \"*.": (float, float) => float = "%mulfloat"
external \"/.": (float, float) => float = "%divfloat"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external \"**": (float, float) => float = "pow"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external exp: float => float = "exp"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external acos: float => float = "acos"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external asin: float => float = "asin"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external atan: float => float = "atan"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external atan2: (float, float) => float = "atan2"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external cos: float => float = "cos"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external cosh: float => float = "cosh"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external log: float => float = "log"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external log10: float => float = "log10"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external log1p: float => float = "log1p"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external sin: float => float = "sin"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external sinh: float => float = "sinh"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external sqrt: float => float = "sqrt"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external tan: float => float = "tan"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external tanh: float => float = "tanh"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external ceil: float => float = "ceil"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external floor: float => float = "floor"

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Math")
external abs_float: float => float = "abs"

@deprecated("Use Core instead. This will be removed in v13")
external mod_float: (float, float) => float = "%modfloat"

@deprecated("Use Core instead. This will be removed in v13")
external float: int => float = "%floatofint"

@deprecated("Use Core instead. This will be removed in v13")
external float_of_int: int => float = "%floatofint"

@deprecated("Use Core instead. This will be removed in v13")
external truncate: float => int = "%intoffloat"

@deprecated("Use Core instead. This will be removed in v13")
external int_of_float: float => int = "%intoffloat"

@deprecated("Use Core instead. This will be removed in v13")
let infinity = 0x1p2047

@deprecated("Use Core instead. This will be removed in v13")
let neg_infinity = -0x1p2047

@deprecated("Use Core instead. This will be removed in v13") @val @scope("Number")
external nan: float = "NaN"

@deprecated("Use Core instead. This will be removed in v13")
let max_float = 1.79769313486231571e+308 /* 0x1.ffff_ffff_ffff_fp+1023 */

@deprecated("Use Core instead. This will be removed in v13")
let min_float = 2.22507385850720138e-308 /* 0x1p-1022 */

@deprecated("Use Core instead. This will be removed in v13")
let epsilon_float = 2.22044604925031308e-16 /* 0x1p-52 */

@deprecated("Do not use. This will be removed in v13")
type fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

@deprecated("Do not use. This will be removed in v13")
let classify_float = (x: float): fpclass =>
  if (%raw(`isFinite`): _ => _)(x) {
    if abs_float(x) >= /* 0x1p-1022 */ /* 2.22507385850720138e-308 */ min_float {
      FP_normal
    } else if x != 0. {
      FP_subnormal
    } else {
      FP_zero
    }
  } else if (%raw(`isNaN`): _ => _)(x) {
    FP_nan
  } else {
    FP_infinite
  }

/* String and byte sequence operations -- more in modules String and Bytes */

external \"^": (string, string) => string = "%string_concat"

/* Character operations -- more in module Char */

@deprecated("Use Core instead. This will be removed in v13")
external int_of_char: char => int = "%identity"

@deprecated("Use Core instead. This will be removed in v13")
external unsafe_char_of_int: int => char = "%identity"

@deprecated("Use Core instead. This will be removed in v13")
let char_of_int = n =>
  if n < 0 || n > 255 {
    invalid_arg("char_of_int")
  } else {
    unsafe_char_of_int(n)
  }

/* Unit operations */

external ignore: 'a => unit = "%ignore"

/* Pair operations */

external fst: (('a, 'b)) => 'a = "%field0"
external snd: (('a, 'b)) => 'b = "%field1"

/* References */

type ref<'a> = {mutable contents: 'a}
external ref: 'a => ref<'a> = "%makeref"
external \"!": ref<'a> => 'a = "%refget"
external \":=": (ref<'a>, 'a) => unit = "%refset"
external incr: ref<int> => unit = "%incr"
external decr: ref<int> => unit = "%decr"

/* String conversion functions */

@deprecated("Use Core instead. This will be removed in v13")
let string_of_bool = b =>
  if b {
    "true"
  } else {
    "false"
  }

@deprecated("Use Core instead. This will be removed in v13")
let bool_of_string = param =>
  switch param {
  | "true" => true
  | "false" => false
  | _ => invalid_arg("bool_of_string")
  }

@deprecated("Use Core instead. This will be removed in v13")
let bool_of_string_opt = param =>
  switch param {
  | "true" => Some(true)
  | "false" => Some(false)
  | _ => None
  }

@deprecated("Use Core instead. This will be removed in v13")
external string_of_int: int => string = "String"

@deprecated("Use Core instead. This will be removed in v13") @scope("Number")
external int_of_string: string => int = "parseInt"

let int_of_string_opt = s =>
  switch int_of_string(s) {
  | n if n == %raw("NaN") => None
  | n => Some(n)
  }

@deprecated("Use Core instead. This will be removed in v13")
external string_get: (string, int) => char = "%string_safe_get"

/* List operations -- more in module List */

@deprecated("Use Core instead. This will be removed in v13")
let rec \"@" = (l1, l2) =>
  switch l1 {
  | list{} => l2
  | list{hd, ...tl} => list{hd, ...\"@"(tl, l2)}
  }

/* Miscellaneous */

type int32 = int

/***
Bindings to functions available in the global JavaScript scope.
*/

/**
An `id` representing a timeout started via `setTimeout`.

See [`setTimeout`](https://developer.mozilla.org/en-US/docs/Web/API/setTimeout) on MDN.
*/
type timeoutId = Js_global.timeoutId

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
type intervalId = Js_global.intervalId

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

@val external window: Dom.window = "window"
@val external document: Dom.document = "document"
@val external globalThis: {..} = "globalThis"

external null: Js.Nullable.t<'a> = "#null"
external undefined: Js.Nullable.t<'a> = "#undefined"
external typeof: 'a => Type.t = "#typeof"

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

type null<+'a> = Js.null<'a>

type undefined<+'a> = Js.undefined<'a>

type nullable<+'a> = Js.nullable<'a>

let panic = Error.panic

/**
`assertEqual(a, b)` check if `a` is equal `b`. If not raise a panic exception

## Examples

```rescript
list{1, 2}
->List.tailExn
->assertEqual(list{2})
```
*/
let assertEqual = (a, b) => {
  if a != b {
    assert(false)
  }
}

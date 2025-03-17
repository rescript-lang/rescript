/***
Functions for handling values that could be `null`.

If you also need to cover `undefined`, check out `Nullable` instead.
*/

/**
A type representing a value that can be either `'a` or `null`.
*/
@unboxed
type t<+'a> = Primitive_js_extern.null<'a> =
  | Value('a)
  | @as(null) Null

/**
Converts a `Null.t` into a `Nullable.t`.

## Examples
```rescript
let nullValue = Null.make("Hello")
let asNullable = nullValue->Null.asNullable // Nullable.t<string>
```
*/
external asNullable: t<'a> => Stdlib_Nullable.t<'a> = "%identity"

/**
The value `null`.

See [`null`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/null) on MDN.

## Examples
```rescript
Console.log(null) // Logs `null` to the console.
```
*/
external null: t<'a> = "#null"

/**
Creates a new `Null.t` from the provided value.
This means the compiler will enforce null checks for the new value.

## Examples
```rescript
let myStr = "Hello"
let asNullValue = myStr->Null.make // The compiler now thinks this can be `string` or `null`.
```
*/
external make: 'a => t<'a> = "%identity"

let equal: (t<'a>, t<'b>, ('a, 'b) => bool) => bool

let compare: (t<'a>, t<'b>, ('a, 'b) => Stdlib_Ordering.t) => Stdlib_Ordering.t

/**
Converts a nullable value into an option, so it can be pattern matched on.
Will convert `null` to `None`, and a present value to `Some(value)`.

## Examples
```rescript
let nullStr = Null.make("Hello")

switch nullStr->Null.toOption {
| Some(str) => Console.log2("Got string:", str)
| None => Console.log("Didn't have a value.")
}
```
*/
external toOption: t<'a> => option<'a> = "#null_to_opt"

/**
Turns an `option` into a `Null.t`. `None` will be converted to `null`.

## Examples
```rescript
let optString: option<string> = None
let asNull = optString->Null.fromOption // Null.t<string>
Console.log(asNull == Null.null) // Logs `true` to the console.
```
*/
let fromOption: option<'a> => t<'a>

/**
`getOr(value, default)` returns `value` if not `null`, otherwise return
`default`.

## Examples

```rescript
Null.getOr(Null.null, "Banana") // Banana
Null.getOr(Null.make("Apple"), "Banana") // Apple

let greet = (firstName: option<string>) =>
  "Greetings " ++ firstName->Option.getOr("Anonymous")

Null.make("Jane")->Null.toOption->greet // "Greetings Jane"
Null.null->Null.toOption->greet // "Greetings Anonymous"
```
*/
let getOr: (t<'a>, 'a) => 'a

@deprecated("Use getOr instead")
let getWithDefault: (t<'a>, 'a) => 'a

/**
`getExn(value)` raises an exception if `null`, otherwise returns the value.

```rescript
Null.getExn(Null.make(3))->assertEqual(3)

switch Null.getExn(%raw("'ReScript'")) {
| exception Invalid_argument(_) => assert(false)
| value => assertEqual(value, "ReScript")
}

switch Null.getExn(%raw("null")) {
| exception Invalid_argument(_) => assert(true)
| _ => assert(false)
}
```

## Exceptions

- Raises `Invalid_argument` if `value` is `null`,
*/
let getExn: t<'a> => 'a

/**
`getUnsafe(value)` returns `value`.

## Examples

```rescript
Null.getUnsafe(Null.make(3)) == 3
Null.getUnsafe(Null.null) // Raises an error
```

## Important

- This is an unsafe operation, it assumes `value` is not `null`.
*/
external getUnsafe: t<'a> => 'a = "%identity"

/**
`forEach(value, f)` call `f` on `value`. if `value` is not `null`, then if calls
`f`, otherwise returns `unit`.

## Examples

```rescript
Null.forEach(Null.make("thing"), x => Console.log(x)) // logs "thing"
Null.forEach(Null.null, x => Console.log(x)) // logs nothing
```
*/
let forEach: (t<'a>, 'a => unit) => unit

/**
`map(value, f)` returns `f(value)` if `value` is not `null`, otherwise returns
`value` unchanged.

## Examples

```rescript
Null.map(Null.make(3), x => x * x) // Null.make(9)
Null.map(Null.null, x => x * x) // null
```
*/
let map: (t<'a>, 'a => 'b) => t<'b>

/**
`mapOr(value, default, f)` returns `f(value)` if `value` is not `null`,
otherwise returns `default`.

## Examples

```rescript
let someValue = Null.make(3)
someValue->Null.mapOr(0, x => x + 5) // 8

let noneValue = Null.null
noneValue->Null.mapOr(0, x => x + 5) // 0
```
*/
let mapOr: (t<'a>, 'b, 'a => 'b) => 'b

@deprecated("Use mapOr instead")
let mapWithDefault: (t<'a>, 'b, 'a => 'b) => 'b

/**
`flatMap(value, f)` returns `f(value)` if `value` is not `null`, otherwise
returns `value` unchanged.

## Examples

```rescript
let addIfAboveOne = value =>
  if (value > 1) {
    Null.make(value + 1)
  } else {
    Null.null
  }

Null.flatMap(Null.make(2), addIfAboveOne) // Null.make(3)
Null.flatMap(Null.make(-4), addIfAboveOne) // null
Null.flatMap(Null.null, addIfAboveOne) // null
```
*/
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>

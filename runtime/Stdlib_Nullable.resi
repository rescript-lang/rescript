/***
Functions for handling nullable values.

Primarily useful when interoping with JavaScript when you don't know whether you'll get a value, `null` or `undefined`.
*/

/**
Type representing a nullable value.
A nullable value can be the value `'a`, `null` or `undefined`.
*/
@unboxed
type t<'a> = Primitive_js_extern.nullable<'a> =
  | Value('a)
  | @as(null) Null
  | @as(undefined) Undefined

/**
The value `null`.

See [`null`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/null) on MDN.

## Examples
```rescript
Console.log(Nullable.null) // Logs `null` to the console.
```
*/
external null: t<'a> = "#null"

/**
The value `undefined`.

See [`undefined`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/undefined) on MDN.

## Examples
```rescript
Console.log(undefined) // Logs `undefined` to the console.
```
*/
external undefined: t<'a> = "#undefined"

/**
`isNullable(a)` returns `true` if `a` is null or undefined, `false` otherwise.

## Examples

```rescript
let myStr = "Hello"
let asNullable = myStr->Nullable.make

// Can't do the below because we're now forced to check for nullability
// myStr == asNullable

// Check if asNullable is not null or undefined
switch asNullable->Nullable.isNullable {
| true => assert(false)
| false => assert(true)
}
```
*/
external isNullable: t<'a> => bool = "#is_nullable"

/**
Creates a new nullable value from the provided value.
This means the compiler will enforce null checks for the new value.

## Examples
```rescript
let myStr = "Hello"
let asNullable = myStr->Nullable.make

// Can't do the below because we're now forced to check for nullability
// myStr == asNullable

// Need to do this
switch asNullable->Nullable.toOption {
| Some(value) if value == myStr => Console.log("Yay, values matched!")
| _ => Console.log("Values did not match.")
}
```
*/
external make: 'a => t<'a> = "%identity"

let equal: (t<'a>, t<'b>, ('a, 'b) => bool) => bool

let compare: (t<'a>, t<'b>, ('a, 'b) => Stdlib_Ordering.t) => Stdlib_Ordering.t

/**
Converts a nullable value into an option, so it can be pattern matched on.
Will convert both `null` and `undefined` to `None`, and a present value to `Some(value)`.

## Examples
```rescript
let nullableString = Nullable.make("Hello")

switch nullableString->Nullable.toOption {
| Some(str) => Console.log2("Got string:", str)
| None => Console.log("Didn't have a value.")
}
```
*/
external toOption: t<'a> => option<'a> = "#nullable_to_opt"

/**
Turns an `option` into a `Nullable.t`.

## Examples
```rescript
let optString = Some("Hello")
let asNullable = optString->Nullable.fromOption // Nullable.t<string>
```
*/
let fromOption: option<'a> => t<'a>

/**
`getOr(value, default)` returns `value` if not `null` or `undefined`,
otherwise return `default`.

## Examples

```rescript
Nullable.getOr(Nullable.null, "Banana") // Banana
Nullable.getOr(Nullable.make("Apple"), "Banana") // Apple

let greet = (firstName: option<string>) =>
  "Greetings " ++ firstName->Option.getOr("Anonymous")

Nullable.make("Jane")->Nullable.toOption->greet // "Greetings Jane"
Nullable.null->Nullable.toOption->greet // "Greetings Anonymous"
```
*/
let getOr: (t<'a>, 'a) => 'a

@deprecated("Use getOr instead")
let getWithDefault: (t<'a>, 'a) => 'a

/**
`getExn(value)` raises an exception if `null` or `undefined`, otherwise returns the value.

```rescript
switch Nullable.getExn(%raw("'Hello'")) {
| exception Invalid_argument(_) => assert(false)
| value => assertEqual(value, "Hello")
}

switch Nullable.getExn(%raw("null")) {
| exception Invalid_argument(_) => assert(true)
| _ => assert(false)
}

switch Nullable.getExn(%raw("undefined")) {
| exception Invalid_argument(_) => assert(true)
| _ => assert(false)
}
```

## Exceptions

- Raises `Invalid_argument` if `value` is `null` or `undefined`
*/
let getExn: t<'a> => 'a

/**
`getUnsafe(value)` returns `value`.

## Examples

```rescript
Nullable.getUnsafe(Nullable.make(3)) == 3
Nullable.getUnsafe(Nullable.null) // Raises an error
```

## Important

- This is an unsafe operation, it assumes `value` is not `null` or `undefined`.
*/
external getUnsafe: t<'a> => 'a = "%identity"

/**
`forEach(value, f)` call `f` on `value`. if `value` is not `null` or `undefined`, 
then if calls `f`, otherwise returns `unit`.

## Examples

```rescript
Nullable.forEach(Nullable.make("thing"), x => Console.log(x)) // logs "thing"
Nullable.forEach(Nullable.null, x => Console.log(x)) // returns ()
Nullable.forEach(undefined, x => Console.log(x)) // returns ()
```
*/
let forEach: (t<'a>, 'a => unit) => unit

/**
`map(value, f)` returns `f(value)` if `value` is not `null` or `undefined`,
otherwise returns `value` unchanged.

## Examples

```rescript
Nullable.map(Nullable.make(3), x => x * x) // Nullable.make(9)
Nullable.map(undefined, x => x * x) // undefined
```
*/
let map: (t<'a>, 'a => 'b) => t<'b>

/**
`mapOr(value, default, f)` returns `f(value)` if `value` is not `null`
or `undefined`, otherwise returns `default`.

## Examples

```rescript
let someValue = Nullable.make(3)
someValue->Nullable.mapOr(0, x => x + 5) // 8

let noneValue = Nullable.null
noneValue->Nullable.mapOr(0, x => x + 5) // 0
```
*/
let mapOr: (t<'a>, 'b, 'a => 'b) => 'b

@deprecated("Use mapOr instead")
let mapWithDefault: (t<'a>, 'b, 'a => 'b) => 'b

/**
`flatMap(value, f)` returns `f(value)` if `value` is not `null` or `undefined`,
otherwise returns `value` unchanged.

## Examples

```rescript
let addIfAboveOne = value =>
  if (value > 1) {
    Nullable.make(value + 1)
  } else {
    Nullable.null
  }

Nullable.flatMap(Nullable.make(2), addIfAboveOne) // Nullable.make(3)
Nullable.flatMap(Nullable.make(-4), addIfAboveOne) // undefined
Nullable.flatMap(Nullable.null, addIfAboveOne) // undefined
```
*/
let flatMap: (t<'a>, 'a => t<'b>) => t<'b>

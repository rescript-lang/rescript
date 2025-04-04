/***
Functions for interacting with JSON.
*/

/** 
A type representing a JSON object.
*/
@unboxed
type rec t =
  | Boolean(bool)
  | @as(null) Null
  | String(string)
  | Number(float)
  | Object(dict<t>)
  | Array(array<t>)

@unboxed
type replacer = Keys(array<string>) | Replacer((string, t) => t)

/** 
`parseExn(string, ~reviver=?)` 

Parses a JSON string or throws a JavaScript exception (SyntaxError), if the string isn't valid.
The reviver describes how the value should be transformed. It is a function which receives a key and a value.
It returns a JSON type.

## Examples
```rescript
try {
  let _ = JSON.parseExn(`{"foo":"bar","hello":"world"}`)
  // { foo: 'bar', hello: 'world' }

  let _ = JSON.parseExn("")
  // error
} catch {
| Exn.Error(_) => Console.log("error")
}

let reviver = (_, value: JSON.t) =>
  switch value {
  | String(string) => string->String.toUpperCase->JSON.Encode.string
  | Number(number) => (number *. 2.0)->JSON.Encode.float
  | _ => value
  }

let jsonString = `{"hello":"world","someNumber":21}`

try {
  JSON.parseExn(jsonString, ~reviver)->Console.log
  // { hello: 'WORLD', someNumber: 42 }

  JSON.parseExn("", ~reviver)->Console.log
  // error
} catch {
| Exn.Error(_) => Console.log("error")
}
```

## Exceptions 

- Raises a SyntaxError (Exn.t) if the string isn't valid JSON.
*/
@raises(Exn.t)
@val
external parseExn: (string, ~reviver: (string, t) => t=?) => t = "JSON.parse"

/** 
`parseExnWithReviver(string, reviver)` 

Parses a JSON string or throws a JavaScript exception (SyntaxError), if the string isn't valid.
The reviver describes how the value should be transformed. It is a function which receives a key and a value.
It returns a JSON type.

## Examples
```rescript
let reviver = (_, value: JSON.t) =>
  switch value {
  | String(string) => string->String.toUpperCase->JSON.Encode.string
  | Number(number) => (number *. 2.0)->JSON.Encode.float
  | _ => value
  }

let jsonString = `{"hello":"world","someNumber":21}`

try {
  JSON.parseExnWithReviver(jsonString, reviver)->Console.log
  // { hello: 'WORLD', someNumber: 42 }

  JSON.parseExnWithReviver("", reviver)->Console.log
  // error
} catch {
| Exn.Error(_) => Console.log("error")
}
```

## Exceptions 

- Raises a SyntaxError if the string isn't valid JSON.
*/
@deprecated("Use `parseExn` with optional parameter instead")
@raises(Exn.t)
@val
external parseExnWithReviver: (string, (string, t) => t) => t = "JSON.parse"

/** 
`stringify(json, ~replacer=?, ~space=?)` 

Converts a JSON object to a JSON string.
The replacer describes how the value should be transformed. It is a function which receives a key and a value,
or an array of keys which should be included in the output.
If you want to stringify any type, use `JSON.stringifyAny` instead.

## Examples
```rescript
let json =
  Dict.fromArray([
    ("foo", JSON.Encode.string("bar")),
    ("hello", JSON.Encode.string("world")),
    ("someNumber", JSON.Encode.int(42)),
  ])->JSON.Encode.object

JSON.stringify(json)
// {"foo":"bar","hello":"world","someNumber":42}

JSON.stringify(json, ~space=2)
// {
//   "foo": "bar",
//   "hello": "world",
//   "someNumber": 42
// }

JSON.stringify(json, ~replacer=Keys(["foo", "someNumber"]))
// {"foo":"bar","someNumber":42}

let replacer = JSON.Replacer((_, value) => {
  let decodedValue = value->JSON.Decode.string

  switch decodedValue {
  | Some(string) => string->String.toUpperCase->JSON.Encode.string
  | None => value
  }
})

JSON.stringify(json, ~replacer)
// {"foo":"BAR","hello":"WORLD","someNumber":42}
```
*/
@val
external stringify: (t, ~replacer: replacer=?, ~space: int=?) => string = "JSON.stringify"

/** 
`stringifyWithIndent(json, indentation)` 

Converts a JSON object to a JSON string. The output will be indented.
If you want to stringify any type, use `JSON.stringifyAnyWithIndent` instead.

## Examples
```rescript
let json =
  Dict.fromArray([
    ("foo", JSON.Encode.string("bar")),
    ("hello", JSON.Encode.string("world")),
    ("someNumber", JSON.Encode.int(42)),
  ])->JSON.Encode.object

JSON.stringifyWithIndent(json, 2)
// {
//   "foo": "bar",
//   "hello": "world",
//   "someNumber": 42
// }
```
*/
@deprecated("Use `stringify` with optional parameter instead")
@val
external stringifyWithIndent: (t, @as(json`null`) _, int) => string = "JSON.stringify"

/** 
`stringifyWithReplacer(json, replacer)` 

Converts a JSON object to a JSON string.
The replacer describes how the value should be transformed. It is a function which receives a key and a value.
If you want to stringify any type, use `JSON.stringifyAnyWithReplacer` instead.

## Examples
```rescript
let json =
  Dict.fromArray([
    ("foo", JSON.Encode.string("bar")),
    ("hello", JSON.Encode.string("world")),
    ("someNumber", JSON.Encode.int(42)),
  ])->JSON.Encode.object

let replacer = (_, value) => {
  let decodedValue = value->JSON.Decode.string

  switch decodedValue {
  | Some(string) => string->String.toUpperCase->JSON.Encode.string
  | None => value
  }
}

JSON.stringifyWithReplacer(json, replacer)
// {"foo":"BAR","hello":"WORLD","someNumber":42}
```
*/
@deprecated("Use `stringify` with optional parameter instead")
@val
external stringifyWithReplacer: (t, (string, t) => t) => string = "JSON.stringify"

/** 
`stringifyWithReplacerAndIndent(json, replacer, indentation)`

Converts a JSON object to a JSON string. The output will be indented.
The replacer describes how the value should be transformed. It is a function which receives a key and a value.
If you want to stringify any type, use `JSON.stringifyAnyWithReplacerAndIndent` instead.

## Examples
```rescript
let json =
  Dict.fromArray([
    ("foo", JSON.Encode.string("bar")),
    ("hello", JSON.Encode.string("world")),
    ("someNumber", JSON.Encode.int(42)),
  ])->JSON.Encode.object

let replacer = (_, value) => {
  let decodedValue = value->JSON.Decode.string

  switch decodedValue {
  | Some(string) => string->String.toUpperCase->JSON.Encode.string
  | None => value
  }
}

JSON.stringifyWithReplacerAndIndent(json, replacer, 2)
// {
//   "foo": "BAR",
//   "hello": "WORLD",
//   "someNumber": 42
// }
```
*/
@deprecated("Use `stringify` with optional parameters instead")
@val
external stringifyWithReplacerAndIndent: (t, (string, t) => t, int) => string = "JSON.stringify"

/** 
`stringifyWithFilter(json, filter)` 

Converts a JSON object to a JSON string.
The filter is an array of keys, which should be included in the output.
If you want to stringify any type, use `JSON.stringifyAnyWithFilter` instead.

## Examples
```rescript
let json =
  Dict.fromArray([
    ("foo", JSON.Encode.string("bar")),
    ("hello", JSON.Encode.string("world")),
    ("someNumber", JSON.Encode.int(42)),
  ])->JSON.Encode.object

JSON.stringifyWithFilter(json, ["foo", "someNumber"])
// {"foo":"bar","someNumber":42}
```
*/
@deprecated("Use `stringify` with optional parameter instead")
@val
external stringifyWithFilter: (t, array<string>) => string = "JSON.stringify"

/** 
`stringifyWithFilterAndIndent(json, filter, indentation)` 

Converts a JSON object to a JSON string. The output will be indented.
The filter is an array of keys, which should be included in the output.
If you want to stringify any type, use `JSON.stringifyAnyWithFilterAndIndent` instead.

## Examples
```rescript
let json =
  Dict.fromArray([
    ("foo", JSON.Encode.string("bar")),
    ("hello", JSON.Encode.string("world")),
    ("someNumber", JSON.Encode.int(42)),
  ])->JSON.Encode.object

JSON.stringifyWithFilterAndIndent(json, ["foo", "someNumber"], 2)
// {
//   "foo": "bar",
//   "someNumber": 42
// }
```
*/
@deprecated("Use `stringify` with optional parameters instead")
@val
external stringifyWithFilterAndIndent: (t, array<string>, int) => string = "JSON.stringify"

/** 
`stringifyAny(any, ~replacer=?, ~space=?)`

Converts any type to a JSON string.
The replacer describes how the value should be transformed. It is a function which receives a key and a value.
Stringifying a function or `undefined` will return `None`.
If the value contains circular references or `BigInt`s, the function will throw a JavaScript exception (TypeError).
If you want to stringify a JSON object, use `JSON.stringify` instead.

## Examples

```rescript
let dict = Dict.fromArray([
  ("foo", JSON.Encode.string("bar")),
  ("hello", JSON.Encode.string("world")),
  ("someNumber", JSON.Encode.int(42)),
])

dict
->JSON.stringifyAny
->Option.getUnsafe
->assertEqual(`{"foo":"bar","hello":"world","someNumber":42}`)

dict
->JSON.stringifyAny(~space=2)
->Option.getUnsafe
->assertEqual(`{
  "foo": "bar",
  "hello": "world",
  "someNumber": 42
}`)

dict
->JSON.stringifyAny(~replacer=Keys(["foo", "someNumber"]))
->Option.getUnsafe
->assertEqual(`{"foo":"bar","someNumber":42}`)

let replacer = JSON.Replacer((_, value) => {
  let decodedValue = value->JSON.Decode.string

  switch decodedValue {
  | Some(string) => string->String.toUpperCase->JSON.Encode.string
  | None => value
  }
})

dict
->JSON.stringifyAny(~replacer)
->Option.getUnsafe
->assertEqual(`{"foo":"BAR","hello":"WORLD","someNumber":42}`)

JSON.stringifyAny(() => "hello world")
->assertEqual(None)

// Raise a exception
switch BigInt.fromInt(0)->JSON.stringifyAny {
| exception _ => assert(true)
| _ => assert(false)
}
```

## Exceptions

- Raises a TypeError if the value contains circular references.
- Raises a TypeError if the value contains `BigInt`s.
*/
@raises(Exn.t)
@val
external stringifyAny: ('a, ~replacer: replacer=?, ~space: int=?) => option<string> =
  "JSON.stringify"

/** 
`stringifyAnyWithIndent(any, indentation)`

Converts any type to a JSON string. The output will be indented.
Stringifying a function or `undefined` will return `None`.
If the value contains circular references or `BigInt`s, the function will throw a JavaScript exception (TypeError).
If you want to stringify a JSON object, use `JSON.stringifyWithIndent` instead.

## Examples

```rescript
let dict = Dict.fromArray([
  ("foo", JSON.Encode.string("bar")),
  ("hello", JSON.Encode.string("world")),
  ("someNumber", JSON.Encode.int(42)),
])

dict
->JSON.stringifyAnyWithIndent(2)
->Option.getUnsafe
->assertEqual(`{
  "foo": "bar",
  "hello": "world",
  "someNumber": 42
}`)

JSON.stringifyAny(() => "hello world")->assertEqual(None)

switch BigInt.fromInt(0)->JSON.stringifyAny {
| exception _ => assert(true)
| _ => assert(false)
}
```

## Exceptions 

- Raises a TypeError if the value contains circular references.
- Raises a TypeError if the value contains `BigInt`s.
*/
@deprecated("Use `stringifyAny` with optional parameter instead")
@raises(Exn.t)
@val
external stringifyAnyWithIndent: ('a, @as(json`null`) _, int) => option<string> = "JSON.stringify"

/** 
`stringifyAnyWithReplacer(json, replacer)`

Converts any type to a JSON string.
The replacer describes how the value should be transformed. It is a function which receives a key and a value.
Stringifying a function or `undefined` will return `None`.
If the value contains circular references or `BigInt`s, the function will throw a JavaScript exception (TypeError).
If you want to stringify a JSON object, use `JSON.stringifyWithReplacer` instead.

## Examples
```rescript
let dict = Dict.fromArray([
  ("foo", JSON.Encode.string("bar")),
  ("hello", JSON.Encode.string("world")),
  ("someNumber", JSON.Encode.int(42)),
])

let replacer = (_, value) => {
  let decodedValue = value->JSON.Decode.string

  switch decodedValue {
  | Some(string) => string->String.toUpperCase->JSON.Encode.string
  | None => value
  }
}

dict
->JSON.stringifyAnyWithReplacer(replacer)
->Option.getUnsafe
->assertEqual(`{"foo":"BAR","hello":"WORLD","someNumber":42}`)

JSON.stringifyAny(() => "hello world")->assertEqual(None)

switch BigInt.fromInt(0)->JSON.stringifyAny {
| exception _ => assert(true)
| _ => assert(false)
}
```

## Exceptions

- Raises a TypeError if the value contains circular references.
- Raises a TypeError if the value contains `BigInt`s.
*/
@deprecated("Use `stringifyAny` with optional parameter instead")
@raises
@val
external stringifyAnyWithReplacer: ('a, (string, t) => t) => option<string> = "JSON.stringify"

/** 
`stringifyAnyWithReplacerAndIndent(json, replacer, indentation)`

Converts any type to a JSON string. The output will be indented.
The replacer describes how the value should be transformed. It is a function which receives a key and a value.
Stringifying a function or `undefined` will return `None`.
If the value contains circular references or `BigInt`s, the function will throw a JavaScript exception (TypeError).
If you want to stringify a JSON object, use `JSON.stringifyWithReplacerAndIndent` instead.

## Examples

```rescript
let dict = Dict.fromArray([
  ("foo", JSON.Encode.string("bar")),
  ("hello", JSON.Encode.string("world")),
  ("someNumber", JSON.Encode.int(42)),
])

let replacer = (_, value) => {
  let decodedValue = value->JSON.Decode.string

  switch decodedValue {
  | Some(string) => string->String.toUpperCase->JSON.Encode.string
  | None => value
  }
}

dict
->JSON.stringifyAnyWithReplacer(replacer)
->Option.getUnsafe
->assertEqual(`{"foo":"BAR","hello":"WORLD","someNumber":42}`)

JSON.stringifyAny(() => "hello world")->assertEqual(None)

switch BigInt.fromInt(0)->JSON.stringifyAny {
| exception _ => assert(true)
| _ => assert(false)
}
```

## Exceptions 

- Raises a TypeError if the value contains circular references.
- Raises a TypeError if the value contains `BigInt`s.
*/
@deprecated("Use `stringifyAny` with optional parameters instead")
@raises
@val
external stringifyAnyWithReplacerAndIndent: ('a, (string, t) => t, int) => option<string> =
  "JSON.stringify"

/** 
`stringifyAnyWithFilter(json, filter)`

Converts any type to a JSON string.
The filter is an array of keys, which should be included in the output.
Stringifying a function or `undefined` will return `None`.
If the value contains circular references or `BigInt`s, the function will throw a JavaScript exception (TypeError).
If you want to stringify a JSON object, use `JSON.stringifyWithFilter` instead.

## Examples

```rescript
let dict = Dict.fromArray([
  ("foo", JSON.Encode.string("bar")),
  ("hello", JSON.Encode.string("world")),
  ("someNumber", JSON.Encode.int(42)),
])

dict
->JSON.stringifyAnyWithFilter(["foo", "someNumber"])
->assertEqual(`{"foo":"bar","someNumber":42}`)

JSON.stringifyAny(() => "hello world")->assertEqual(None)

switch BigInt.fromInt(0)->JSON.stringifyAny {
| exception _ => assert(true)
| _ => assert(false)
}
```

## Exceptions

- Raises a TypeError if the value contains circular references.
- Raises a TypeError if the value contains `BigInt`s.
*/
@deprecated("Use `stringifyAny` with optional parameter instead")
@raises
@val
external stringifyAnyWithFilter: ('a, array<string>) => string = "JSON.stringify"

/** 
`stringifyAnyWithFilterAndIndent(json, filter, indentation)`

Converts any type to a JSON string. The output will be indented.
The filter is an array of keys, which should be included in the output.
Stringifying a function or `undefined` will return `None`.
If the value contains circular references or `BigInt`s, the function will throw a JavaScript exception (TypeError).
If you want to stringify a JSON object, use `JSON.stringifyWithFilterAndIndent` instead.

## Examples

```rescript
let dict = Dict.fromArray([
  ("foo", JSON.Encode.string("bar")),
  ("hello", JSON.Encode.string("world")),
  ("someNumber", JSON.Encode.int(42)),
])

dict
->JSON.stringifyAny
->Option.getUnsafe
->assertEqual(`{"foo":"bar","hello":"world","someNumber":42}`)

dict
->JSON.stringifyAny(~space=2)
->Option.getUnsafe
->assertEqual(`{
  "foo": "bar",
  "hello": "world",
  "someNumber": 42
}`)

dict
->JSON.stringifyAny(~replacer=Keys(["foo", "someNumber"]))
->Option.getUnsafe
->assertEqual(`{"foo":"bar","someNumber":42}`)

JSON.stringifyAny(() => "hello world")->assertEqual(None)

switch BigInt.fromInt(0)->JSON.stringifyAny {
| exception _ => assert(true)
| _ => assert(false)
}
```

## Exceptions 

- Raises a TypeError if the value contains circular references.
- Raises a TypeError if the value contains `BigInt`s.
*/
@deprecated("Use `stringifyAny` with optional parameters instead")
@raises
@val
external stringifyAnyWithFilterAndIndent: ('a, array<string>, int) => string = "JSON.stringify"

module Classify: {
  /**
  A type representing a JavaScript type.
  */
  type t =
    | Bool(bool)
    | Null
    | String(string)
    | Number(float)
    | Object(dict<t>)
    | Array(array<t>)

  /**
  Returns the JSON type of any value.

  ## Examples
  ```rescript
  JSON.Classify.classify("hello world")
  // String("hello world")

  JSON.Classify.classify(42)
  // Number(42)
  ```
  */
  @deprecated("Directly switch on the JSON object instead")
  let classify: 'a => t
}

module Encode: {
  /**
  Returns a boolean as a JSON object.

  ## Examples
  ```rescript
  JSON.Encode.bool(true)
  ```
  */
  external bool: bool => t = "%identity"

  /**
  Returns null as a JSON object.

  ## Examples
  ```rescript
  JSON.Encode.null
  ```
  */
  external null: t = "#null"

  /**
  Returns a string as a JSON object.

  ## Examples
  ```rescript
  JSON.Encode.string("hello world")
  ```
  */
  external string: string => t = "%identity"

  /**
  Returns an int as a JSON object.

  ## Examples
  ```rescript
  JSON.Encode.int(42)
  ```
  */
  external int: int => t = "%identity"

  /**
  Returns a float as a JSON object.

  ## Examples
  ```rescript
  JSON.Encode.float(42.0)
  ```
  */
  external float: float => t = "%identity"

  /**
  Returns a dict as a JSON object.

  ## Examples
  ```rescript
  let dict = Dict.fromArray([
    ("foo", JSON.Encode.string("bar")),
    ("hello", JSON.Encode.string("world")),
  ])

  JSON.Encode.object(dict)
  ```
  */
  external object: dict<t> => t = "%identity"

  /**
  Returns an array as a JSON object.

  ## Examples
  ```rescript
  let array = [JSON.Encode.string("hello world"), JSON.Encode.int(42)]

  JSON.Encode.array(array)
  ```
  */
  external array: array<t> => t = "%identity"
}

module Decode: {
  /**
  Decodes a single JSON value. If the value is a bool, it will return `Some(bool)` - otherwise it will return `None`.

  ## Examples
  ```rescript
  JSON.parseExn(`true`)->JSON.Decode.bool
  // Some(true)

  JSON.parseExn(`"hello world"`)->JSON.Decode.bool
  // None
  ```
  */
  let bool: t => option<bool>

  /**
  Decodes a single JSON value. If the value is null, it will return `Some(Null.t)` - otherwise it will return `None`.

  ## Examples
  ```rescript
  JSON.parseExn(`null`)->JSON.Decode.null
  // Some(null)

  JSON.parseExn(`"hello world"`)->JSON.Decode.null
  // None
  ```
  */
  let null: t => option<Stdlib_Null.t<'a>>

  /**
  Decodes a single JSON value. If the value is a string, it will return `Some(string)` - otherwise it will return `None`.

  ## Examples
  ```rescript
  JSON.parseExn(`"hello world"`)->JSON.Decode.string
  // Some("hello world")

  JSON.parseExn(`42`)->JSON.Decode.string
  // None 
  ```
  */
  let string: t => option<string>

  /**
  Decodes a single JSON value. If the value is a float, it will return `Some(float)` - otherwise it will return `None`.

  ## Examples
  ```rescript
  JSON.parseExn(`42.0`)->JSON.Decode.float
  // Some(42.0)

  JSON.parseExn(`"hello world"`)->JSON.Decode.float
  // None
  ```
  */
  let float: t => option<float>

  /**
  Decodes a single JSON value. If the value is an object, it will return `Some(dict)` - otherwise it will return `None`.

  ## Examples
  ```rescript
  JSON.parseExn(`{"foo":"bar"}`)->JSON.Decode.object
  // Some({ foo: 'bar' })

  JSON.parseExn(`"hello world"`)->JSON.Decode.object
  // None
  ```
  */
  let object: t => option<dict<t>>

  /**
  Decodes a single JSON value. If the value is an array, it will return `Some(array)` - otherwise it will return `None`.

  ## Examples
  ```rescript
  JSON.parseExn(`["foo", "bar"]`)->JSON.Decode.array
  // Some([ 'foo', 'bar' ])

  JSON.parseExn(`"hello world"`)->JSON.Decode.array
  // None
  ```
  */
  let array: t => option<array<t>>
}

/**
  `ignore(json)` ignores the provided json and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

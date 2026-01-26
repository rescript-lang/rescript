/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/***
Efficient JSON encoding using JavaScript API

**see** [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
*/

/* ## Types */

/** The JSON data structure */
@deprecated({
  reason: "Use `JSON.t` instead.",
  migrate: %replace.type(: JSON.t),
})
@unboxed
type rec t = Stdlib_JSON.t =
  | Boolean(bool)
  | @as(null) Null
  | String(string)
  | Number(float)
  | Object(dict<t>)
  | Array(array<t>)

@deprecated("This functionality has been deprecated and will be removed in v13.")
module Kind: {
  type json = t
  /** Underlying type of a JSON value */
  type rec t<_> =
    | String: t<Js_string.t>
    | Number: t<float>
    | Object: t<dict<json>>
    | Array: t<array<json>>
    | Boolean: t<bool>
    | Null: t<Js_types.null_val>
}

@deprecated("This functionality has been deprecated and will be removed in v13.")
type tagged_t =
  | JSONFalse
  | JSONTrue
  | JSONNull
  | JSONString(string)
  | JSONNumber(float)
  | JSONObject(dict<t>)
  | JSONArray(array<t>)

/* ## Accessors */

@deprecated("This functionality has been deprecated and will be removed in v13.")
let classify: t => tagged_t

/**
`test(v, kind)` returns `true` if `v` is of `kind`.
*/
@deprecated("This functionality has been deprecated and will be removed in v13.")
let test: ('a, Kind.t<'b>) => bool

/**
`decodeString(json)` returns `Some(s)` if `json` is a `string`, `None` otherwise.
*/
@deprecated({
  reason: "Use `JSON.Decode.string` instead.",
  migrate: JSON.Decode.string(),
})
let decodeString: t => option<Js_string.t>

/**
`decodeNumber(json)` returns `Some(n)` if `json` is a `number`, `None` otherwise.
*/
@deprecated({
  reason: "Use `JSON.Decode.float` instead.",
  migrate: JSON.Decode.float(),
})
let decodeNumber: t => option<float>

/**
`decodeObject(json)` returns `Some(o)` if `json` is an `object`, `None` otherwise.
*/
@deprecated({
  reason: "Use `JSON.Decode.object` instead.",
  migrate: JSON.Decode.object(),
})
let decodeObject: t => option<dict<t>>

/**
`decodeArray(json)` returns `Some(a)` if `json` is an `array`, `None` otherwise.
*/
@deprecated({
  reason: "Use `JSON.Decode.array` instead.",
  migrate: JSON.Decode.array(),
})
let decodeArray: t => option<array<t>>

/**
`decodeBoolean(json)` returns `Some(b)` if `json` is a `boolean`, `None` otherwise.
*/
@deprecated({
  reason: "Use `JSON.Decode.bool` instead.",
  migrate: JSON.Decode.bool(),
})
let decodeBoolean: t => option<bool>

/**
`decodeNull(json)` returns `Some(null)` if `json` is a `null`, `None` otherwise.
*/
@deprecated({
  reason: "Use JSON.Decode.null instead.",
  migrate: JSON.Decode.null(),
})
let decodeNull: t => option<Js_null.t<'a>>

/* ## Constructors */

/*
   Those functions allows the construction of an arbitrary complex
   JSON values.
*/

/** `null` is the singleton null JSON value. */
@deprecated({
  reason: "Use `JSON.Encode.null` instead.",
  migrate: JSON.Encode.null,
})
@val
external null: t = "null"

/** `string(s)` makes a JSON string of the `string` `s`. */
@deprecated({
  reason: "Use `JSON.Encode.string` instead.",
  migrate: JSON.Encode.string(),
})
external string: string => t = "%identity"

/** `number(n)` makes a JSON number of the `float` `n`. */
@deprecated({
  reason: "Use `JSON.Encode.float` instead.",
  migrate: JSON.Encode.float(),
})
external number: float => t = "%identity"

/** `boolean(b)` makes a JSON boolean of the `bool` `b`. */
@deprecated({
  reason: "Use `JSON.Encode.bool` instead.",
  migrate: JSON.Encode.bool(),
})
external boolean: bool => t = "%identity"

/** `object_(dict)` makes a JSON object of the `dict`. */
@deprecated({
  reason: "Use `JSON.Encode.object` instead.",
  migrate: JSON.Encode.object(),
})
external object_: dict<t> => t = "%identity"

/** `array_(a)` makes a JSON array of the `Js.Json.t` array `a`. */
@deprecated({
  reason: "Use `JSON.Encode.array` instead.",
  migrate: JSON.Encode.array(),
})
external array: array<t> => t = "%identity"

/*
  The functions below are specialized for specific array type which
  happened to be already JSON object in the ReScript runtime. Therefore
  they are more efficient (constant time rather than linear conversion).
*/

/** `stringArray(a)` makes a JSON array of the `string` array `a`. */
@deprecated({
  reason: "Use `JSON.Encode.stringArray` instead.",
  migrate: JSON.Encode.stringArray(),
})
external stringArray: array<string> => t = "%identity"

/** `numberArray(a)` makes a JSON array of the `float` array `a`. */
@deprecated({
  reason: "Use `JSON.Encode.floatArray` instead.",
  migrate: JSON.Encode.floatArray(),
})
external numberArray: array<float> => t = "%identity"

/** `booleanArray(a)` makes a JSON array of the `bool` array `a`. */
@deprecated({
  reason: "Use `JSON.Encode.boolArray` instead.",
  migrate: JSON.Encode.boolArray(),
})
external booleanArray: array<bool> => t = "%identity"

/** `objectArray(a) makes a JSON array of the `JsDict.t` array `a`. */
@deprecated({
  reason: "Use `JSON.Encode.objectArray` instead.",
  migrate: JSON.Encode.objectArray(),
})
external objectArray: array<dict<t>> => t = "%identity"

/* ## String conversion */

/**
`parseExn(s)` parses the `string` `s` into a JSON data structure.
Returns a JSON data structure.
Throws `SyntaxError` if the given string is not a valid JSON. Note: `SyntaxError` is a JavaScript exception.

See [`parse`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/parse) on MDN.

## Examples

```rescript
/* parse a simple JSON string */

let json = try Js.Json.parseExn(` "hello" `) catch {
| _ => failwith("Error parsing JSON string")
}

switch Js.Json.classify(json) {
| Js.Json.JSONString(value) => Js.log(value)
| _ => failwith("Expected a string")
}
```

```rescript
/* parse a complex JSON string */

let getIds = s => {
  let json = try Js.Json.parseExn(s) catch {
  | _ => failwith("Error parsing JSON string")
  }

  switch Js.Json.classify(json) {
  | Js.Json.JSONObject(value) =>
    /* In this branch, compiler infer value : Js.Json.t dict */
    switch Js.Dict.get(value, "ids") {
    | Some(ids) =>
      switch Js.Json.classify(ids) {
      | Js.Json.JSONArray(ids) => /* In this branch compiler infer ids : Js.Json.t array */
        ids
      | _ => failwith("Expected an array")
      }
    | None => failwith("Expected an `ids` property")
    }
  | _ => failwith("Expected an object")
  }
}

/* prints `1, 2, 3` */
Js.log(getIds(` { "ids" : [1, 2, 3 ] } `))
```
*/
@deprecated({
  reason: "Use `JSON.parseOrThrow` instead.",
  migrate: JSON.parseOrThrow(),
})
@val
@scope("JSON")
external parseExn: string => t = "parse"

/**
`stringify(json)` formats the JSON data structure as a `string`.
Returns the string representation of a given JSON data structure.

See [`stringify`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify) on MDN.

## Examples

```rescript
/* Creates and stringifies a simple JS object */

let dict = Js.Dict.empty()
Js.Dict.set(dict, "name", Js.Json.string("John Doe"))
Js.Dict.set(dict, "age", Js.Json.number(30.0))
Js.Dict.set(dict, "likes", Js.Json.stringArray(["ReScript", "ocaml", "js"]))

Js.log(Js.Json.stringify(Js.Json.object_(dict)))
```
*/
@deprecated({
  reason: "Use `JSON.stringify` instead.",
  migrate: JSON.stringify(),
})
@val
@scope("JSON")
external stringify: t => string = "stringify"

/**
`stringifyWithSpace(json)` formats the JSON data structure as a `string`.
Returns the string representation of a given JSON data structure with spacing.

See [`stringify`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify) on MDN.

## Examples

```rescript
/* Creates and stringifies a simple JS object with spacing */

let dict = Js.Dict.empty()
Js.Dict.set(dict, "name", Js.Json.string("John Doe"))
Js.Dict.set(dict, "age", Js.Json.number(30.0))
Js.Dict.set(dict, "likes", Js.Json.stringArray(["ReScript", "ocaml", "js"]))

Js.log(Js.Json.stringifyWithSpace(Js.Json.object_(dict), 2))
```
*/
@deprecated({
  reason: "Use `JSON.stringify` with optional `~space` instead.",
  migrate: JSON.stringify(~space=%insert.unlabelledArgument(2)),
})
@val
@scope("JSON")
external stringifyWithSpace: (t, @as(json`null`) _, int) => string = "stringify"

/**
`stringifyAny(value)` formats any value into a JSON string.

## Examples

```rescript
/* prints `["hello", "world"]` */
Js.log(Js.Json.stringifyAny(["hello", "world"]))
```
*/
@deprecated({
  reason: "Use `JSON.stringifyAny` instead.",
  migrate: JSON.stringifyAny(),
})
@val
@scope("JSON")
external stringifyAny: 'a => option<string> = "stringify"

/**
Best-effort serialization, it tries to seralize as
many objects as possible and deserialize it back

It is unsafe in two aspects
- It may throw during  parsing
- when you cast it to a specific type, it may have a type mismatch
*/
@deprecated("This functionality has been deprecated and will be removed in v13.")
let deserializeUnsafe: string => 'a

/**
It will throw in such situations:
- The object can not be serlialized to a JSON
- There are cycles
- Some JS engines can not stringify deeply nested json objects
*/
@deprecated("This functionality has been deprecated and will be removed in v13.")
let serializeExn: 'a => string

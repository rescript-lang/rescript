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

@raises @val external parseExn: (string, ~reviver: (string, t) => t=?) => t = "JSON.parse"
@deprecated("Use `parseExn` with optional parameter instead") @raises @val
external parseExnWithReviver: (string, (string, t) => t) => t = "JSON.parse"

@val external stringify: (t, ~replacer: replacer=?, ~space: int=?) => string = "JSON.stringify"
@deprecated("Use `stringify` with optional parameter instead") @val
external stringifyWithIndent: (t, @as(json`null`) _, int) => string = "JSON.stringify"
@deprecated("Use `stringify` with optional parameter instead") @val
external stringifyWithReplacer: (t, (string, t) => t) => string = "JSON.stringify"
@deprecated("Use `stringify` with optional parameters instead") @val
external stringifyWithReplacerAndIndent: (t, (string, t) => t, int) => string = "JSON.stringify"
@deprecated("Use `stringify` with optional parameter instead") @val
external stringifyWithFilter: (t, array<string>) => string = "JSON.stringify"
@deprecated("Use `stringify` with optional parameters instead") @val
external stringifyWithFilterAndIndent: (t, array<string>, int) => string = "JSON.stringify"

@raises @val
external stringifyAny: ('a, ~replacer: replacer=?, ~space: int=?) => option<string> =
  "JSON.stringify"
@deprecated("Use `stringifyAny` with optional parameter instead") @raises @val
external stringifyAnyWithIndent: ('a, @as(json`null`) _, int) => option<string> = "JSON.stringify"
@deprecated("Use `stringifyAny` with optional parameter instead") @raises @val
external stringifyAnyWithReplacer: ('a, (string, t) => t) => option<string> = "JSON.stringify"
@deprecated("Use `stringifyAny` with optional parameters instead") @raises @val
external stringifyAnyWithReplacerAndIndent: ('a, (string, t) => t, int) => option<string> =
  "JSON.stringify"
@deprecated("Use `stringifyAny` with optional parameter instead") @raises @val
external stringifyAnyWithFilter: ('a, array<string>) => string = "JSON.stringify"
@deprecated("Use `stringifyAny` with optional parameters instead") @raises @val
external stringifyAnyWithFilterAndIndent: ('a, array<string>, int) => string = "JSON.stringify"

module Classify = {
  @val external _internalClass: 'a => string = "Object.prototype.toString.call"
  external _asBool: 'a => bool = "%identity"
  external _asString: 'a => string = "%identity"
  external _asFloat: 'a => float = "%identity"
  external _asArray: 'a => array<t> = "%identity"
  external _asDict: 'a => dict<t> = "%identity"

  type t =
    | Bool(bool)
    | Null
    | String(string)
    | Number(float)
    | Object(dict<t>)
    | Array(array<t>)

  @deprecated("Directly switch on the JSON object instead")
  let classify = value => {
    switch _internalClass(value) {
    | "[object Boolean]" => Bool(_asBool(value))
    | "[object Null]" => Null
    | "[object String]" => String(_asString(value))
    | "[object Number]" => Number(_asFloat(value))
    | "[object Array]" => Array(_asArray(value))
    | _ => Object(_asDict(value))
    }
  }
}

module Encode = {
  external bool: bool => t = "%identity"
  external null: t = "#null"
  external string: string => t = "%identity"
  external int: int => t = "%identity"
  external float: float => t = "%identity"
  external object: dict<t> => t = "%identity"
  external array: array<t> => t = "%identity"
}

module Decode = {
  let bool = json =>
    switch json {
    | Boolean(b) => Some(b)
    | _ => None
    }

  let null = json =>
    switch json {
    | Null => Some(Stdlib_Null.null)
    | _ => None
    }

  let string = json =>
    switch json {
    | String(s) => Some(s)
    | _ => None
    }

  let float = json =>
    switch json {
    | Number(f) => Some(f)
    | _ => None
    }

  let object = json =>
    switch json {
    | Object(o) => Some(o)
    | _ => None
    }

  let array = (json: t) =>
    switch json {
    | Array(a) => Some(a)
    | _ => None
    }
}

external ignore: t => unit = "%ignore"

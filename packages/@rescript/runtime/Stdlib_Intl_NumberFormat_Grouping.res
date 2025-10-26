/***
Represents the `useGrouping` option accepted by `Intl.NumberFormat`.

See [`Intl.NumberFormat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/NumberFormat) on MDN for full option details.
*/
@notUndefined
type t

type parsed = [#bool(bool) | #always | #auto | #min2]

/**
Constructs a grouping setting from a boolean.

## Examples

```rescript
let formatter =
  Intl.NumberFormat.make(
    ~locales=["en-US"],
    ~options={useGrouping: Intl.NumberFormat.Grouping.fromBool(false)},
  )
formatter->Intl.NumberFormat.format(1234.) == "1234"
```
*/
external fromBool: bool => t = "%identity"
/**
Constructs a grouping setting from a string literal.

## Examples

```rescript
let formatter =
  Intl.NumberFormat.make(
    ~locales=["en-US"],
    ~options={useGrouping: Intl.NumberFormat.Grouping.fromString(#always)},
  )
formatter->Intl.NumberFormat.format(1234.) == "1,234"
```
*/
external fromString: [#always | #auto | #min2] => t = "%identity"

/**
`parseJsValue(value)` attempts to interpret a JavaScript `value` as a grouping setting.

## Examples

```rescript
Intl.NumberFormat.Grouping.parseJsValue(Js.Json.string("auto")) == Some(#auto)
```
*/
let parseJsValue = value =>
  switch Stdlib_Type.Classify.classify(value) {
  | String("always") => Some(#always)
  | String("auto") => Some(#auto)
  | String("min2") => Some(#min2)
  | Bool(value) => Some(#bool(value))
  | _ => None
  }

/**
  `ignore(grouping)` ignores the provided grouping and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

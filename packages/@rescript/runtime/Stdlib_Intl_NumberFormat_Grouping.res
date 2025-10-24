/***
Represents the `useGrouping` option accepted by `Intl.NumberFormat`.
*/
@notUndefined
type t

type parsed = [#bool(bool) | #always | #auto | #min2]

/**
Constructs a grouping setting from a boolean.

## Examples

```rescript
Stdlib_Intl_NumberFormat_Grouping.fromBool(true) == Stdlib_Intl_NumberFormat_Grouping.fromBool(true)
```
*/
external fromBool: bool => t = "%identity"
/**
Constructs a grouping setting from a string literal.

## Examples

```rescript
Stdlib_Intl_NumberFormat_Grouping.fromString(#auto) == Stdlib_Intl_NumberFormat_Grouping.fromString(#auto)
```
*/
external fromString: [#always | #auto | #min2] => t = "%identity"

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

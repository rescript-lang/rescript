@notUndefined
type t

type listType = [
  | #conjunction
  | #disjunction
  | #unit
]
type style = [
  | #long
  | #short
  | #narrow
]

type options = {
  localeMatcher?: Stdlib_Intl_Common.localeMatcher,
  \"type"?: listType,
  style?: style,
}

type listPartComponentType = [
  | #element
  | #literal
]

type listPart = {
  \"type": listPartComponentType,
  value: string,
}

type resolvedOptions = {
  locale: string,
  style: style,
  \"type": listType,
}

type supportedLocalesOptions = {localeMatcher: Stdlib_Intl_Common.localeMatcher}

/**
Creates a new `Intl.ListFormat` instance for formatting lists.

See [`Intl.ListFormat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/ListFormat) on MDN.

## Examples

```rescript
let formatter = Intl.ListFormat.make(~locales=["en"], ~options={\"type": #conjunction})
formatter->Intl.ListFormat.format(["apples", "bananas", "cherries"]) == "apples, bananas, and cherries"
```
*/
@new external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.ListFormat"

/**
`supportedLocalesOf(locales, ~options)` filters `locales` to those supported for list formatting.

See [`Intl.ListFormat.supportedLocalesOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/ListFormat/supportedLocalesOf) on MDN.

## Examples

```rescript
Intl.ListFormat.supportedLocalesOf(["en-US", "klingon"]) == ["en-US"]
```
*/
@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => array<string> =
  "Intl.ListFormat.supportedLocalesOf"

/**
`resolvedOptions(formatter)` returns the actual options being used.

See [`Intl.ListFormat.prototype.resolvedOptions`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/ListFormat/resolvedOptions) on MDN.

## Examples

```rescript
let formatter = Intl.ListFormat.make(~locales=["en"])
Intl.ListFormat.resolvedOptions(formatter).locale == "en"
```
*/
@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

/**
`format(formatter, items)` returns the formatted list string.

## Examples

```rescript
let formatter = Intl.ListFormat.make(~locales=["en"])
formatter->Intl.ListFormat.format(["a", "b"]) == "a and b"
```
*/
@send external format: (t, array<string>) => string = "format"

/**
`formatToParts(formatter, items)` returns the list as an array of parts describing how it would be rendered.

See [`Intl.ListFormat.prototype.formatToParts`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/ListFormat/formatToParts) on MDN.

## Examples

```rescript
let formatter = Intl.ListFormat.make(~locales=["en"])
formatter->Intl.ListFormat.formatToParts(["a", "b"])->Array.length > 0
```
*/
@send external formatToParts: (t, array<string>) => array<listPart> = "formatToParts"

/**
  `ignore(listFormat)` ignores the provided listFormat and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

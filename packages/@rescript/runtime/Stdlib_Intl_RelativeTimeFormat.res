/***
Bindings to JavaScript's `Intl.RelativeTimeFormat`.

See [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/RelativeTimeFormat) for API details.
*/
@notUndefined
type t

type numeric = [#always | #auto]
type style = [#long | #short | #narrow]
type timeUnit = [#year | #quarter | #month | #week | #day | #hour | #minute | #second]

type options = {
  localeMatcher?: Stdlib_Intl_Common.localeMatcher,
  numeric?: numeric,
  style?: style,
}

type supportedLocalesOptions = {localeMatcher: Stdlib_Intl_Common.localeMatcher}

type resolvedOptions = {
  locale: string,
  numeric: numeric,
  style: style,
  numberingSystem: string,
}

type relativeTimePartComponent = [#literal | #integer]
type relativeTimePart = {
  \"type": relativeTimePartComponent,
  value: string,
  unit?: timeUnit,
}

/**
Creates a new `Intl.RelativeTimeFormat` instance for formatting relative time strings.

See [`Intl.RelativeTimeFormat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/RelativeTimeFormat) on MDN.

## Examples

```rescript
let formatter = Intl.RelativeTimeFormat.make(~locales=["en-US"])
formatter->Intl.RelativeTimeFormat.format(1, #day)->String.length > 0
```
*/
@new
external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.RelativeTimeFormat"

/**
`supportedLocalesOf(locales, ~options)` filters `locales` to those supported for relative time formatting.

See [`Intl.RelativeTimeFormat.supportedLocalesOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/RelativeTimeFormat/supportedLocalesOf) on MDN.

## Examples

```rescript
Intl.RelativeTimeFormat.supportedLocalesOf(["en-US", "klingon"]) == ["en-US"]
```
*/
@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => array<string> =
  "Intl.RelativeTimeFormat.supportedLocalesOf"

/**
`resolvedOptions(formatter)` returns the locale and options currently in use.

See [`Intl.RelativeTimeFormat.prototype.resolvedOptions`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/RelativeTimeFormat/resolvedOptions) on MDN.

## Examples

```rescript
let formatter = Intl.RelativeTimeFormat.make(~locales=["en-US"])
Intl.RelativeTimeFormat.resolvedOptions(formatter).locale == "en-US"
```
*/
@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

/**
`format(formatter, value, unit)` returns the formatted string for `value` expressed in `unit`.

## Examples

```rescript
let formatter = Intl.RelativeTimeFormat.make(~locales=["en"])
formatter->Intl.RelativeTimeFormat.format(-1, #day)->String.length > 0
```
*/
@send external format: (t, int, timeUnit) => string = "format"

/**
`formatToParts(formatter, value, unit)` returns an array describing how the output string is assembled.

See [`Intl.RelativeTimeFormat.prototype.formatToParts`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/RelativeTimeFormat/formatToParts) on MDN.

## Examples

```rescript
let formatter = Intl.RelativeTimeFormat.make(~locales=["en"])
formatter->Intl.RelativeTimeFormat.formatToParts(-1, #day)->Array.length > 0
```
*/
@send external formatToParts: (t, int, timeUnit) => array<relativeTimePart> = "formatToParts"

/**
  `ignore(relativeTimeFormat)` ignores the provided relativeTimeFormat and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

module Common = Stdlib_Intl_Common
module Collator = Stdlib_Intl_Collator
module DateTimeFormat = Stdlib_Intl_DateTimeFormat
module ListFormat = Stdlib_Intl_ListFormat
module Locale = Stdlib_Intl_Locale
module NumberFormat = Stdlib_Intl_NumberFormat
module PluralRules = Stdlib_Intl_PluralRules
module RelativeTimeFormat = Stdlib_Intl_RelativeTimeFormat
module Segmenter = Stdlib_Intl_Segmenter
module Segments = Stdlib_Intl_Segments

/**
`getCanonicalLocalesExn(locale)` returns the canonical form of `locale`.

Throws `RangeError` when the locale string is malformed.

See [`Intl.getCanonicalLocales`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/getCanonicalLocales) on MDN.

## Examples

```rescript
Intl.getCanonicalLocalesExn("EN-US") == ["en-US"]
```
*/
external getCanonicalLocalesExn: string => array<string> = "Intl.getCanonicalLocales"

/**
`getCanonicalLocalesManyExn(locales)` canonicalises every locale in `locales`.

Throws `RangeError` when any locale string is malformed.

See [`Intl.getCanonicalLocales`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/getCanonicalLocales) on MDN.

## Examples

```rescript
Intl.getCanonicalLocalesManyExn(["EN-US", "fr"]) == ["en-US", "fr"]
```
*/
external getCanonicalLocalesManyExn: array<string> => array<string> = "Intl.getCanonicalLocales"

/**
`supportedValuesOfExn(key)` returns the list of values supported by the runtime for the feature identified by `key`.

Throws `RangeError` when `key` is unsupported.

See [`Intl.supportedValuesOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/supportedValuesOf) on MDN.

## Examples

```rescript
Intl.supportedValuesOfExn("calendar")->Array.includes("gregory") == true
```
*/
external supportedValuesOfExn: string => array<string> = "Intl.supportedValuesOf"

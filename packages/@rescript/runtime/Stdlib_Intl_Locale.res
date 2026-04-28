/***
Bindings to JavaScript's `Intl.Locale`.
*/

@notUndefined
type t

type options = {
  baseName?: string,
  calendar?: Stdlib_Intl_Common.calendar,
  collation?: Stdlib_Intl_Common.collation,
  hourCycle?: [#h11 | #h12 | #h23 | #h24],
  caseFirst?: [#upper | #lower | #"false"],
  numberingSystem?: Stdlib_Intl_Common.numberingSystem,
  numeric?: bool,
  language?: string,
  script?: string,
  region?: string,
}

/**
Creates a new `Intl.Locale` object from a locale identifier and optional modifiers.

See [`Intl.Locale`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("en-US")
locale->Intl.Locale.language == "en"
```
*/
@new external make: (string, ~options: options=?) => t = "Intl.Locale"

/**
`baseName(locale)` returns the canonical base name (without Unicode extensions).

See [`Intl.Locale.prototype.baseName`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/baseName) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("fr-CA")
locale->Intl.Locale.baseName == "fr-CA"
```
*/
@get external baseName: t => string = "baseName"

/**
`calendar(locale)` returns the specified calendar, if present.

See [`Intl.Locale.prototype.calendar`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/calendar) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("en", ~options={calendar: #gregory})
locale->Intl.Locale.calendar == Some("gregory")
```
*/
@get external calendar: t => option<string> = "calendar"

/**
`caseFirst(locale)` returns the case-first ordering setting, if present.

See [`Intl.Locale.prototype.caseFirst`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/caseFirst) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("en", ~options={caseFirst: #upper})
locale->Intl.Locale.caseFirst == Some("upper")
```
*/
@get external caseFirst: t => option<string> = "caseFirst"

/**
`collation(locale)` returns the collation type, if present.

See [`Intl.Locale.prototype.collation`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/collation) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("en", ~options={collation: #phonebk})
locale->Intl.Locale.collation == Some("phonebk")
```
*/
@get external collation: t => option<string> = "collation"

/**
`hourCycle(locale)` returns the preferred hour cycle, if present.

See [`Intl.Locale.prototype.hourCycle`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/hourCycle) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("en", ~options={hourCycle: #h12})
locale->Intl.Locale.hourCycle == Some("h12")
```
*/
@get external hourCycle: t => option<string> = "hourCycle"

/**
`language(locale)` returns the primary language subtag.

See [`Intl.Locale.prototype.language`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/language) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("pt-BR")
locale->Intl.Locale.language == "pt"
```
*/
@get external language: t => string = "language"

/**
`numberingSystem(locale)` returns the numbering system identifier, if present.

See [`Intl.Locale.prototype.numberingSystem`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/numberingSystem) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("en", ~options={numberingSystem: #latn})
locale->Intl.Locale.numberingSystem == Some("latn")
```
*/
@get external numberingSystem: t => option<string> = "numberingSystem"

/**
`numeric(locale)` indicates whether numeric ordering should be used for region subtags.

See [`Intl.Locale.prototype.numeric`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/numeric) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("en", ~options={numeric: true})
locale->Intl.Locale.numeric == true
```
*/
@get external numeric: t => bool = "numeric"

/**
`region(locale)` returns the region subtag, if present.

See [`Intl.Locale.prototype.region`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/region) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("en-US")
locale->Intl.Locale.region == Some("US")
```
*/
@get external region: t => option<string> = "region"

/**
`script(locale)` returns the script subtag, if present.

See [`Intl.Locale.prototype.script`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/script) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("sr-Cyrl")
locale->Intl.Locale.script == Some("Cyrl")
```
*/
@get external script: t => option<string> = "script"

/**
`maximize(locale)` adds likely subtags to produce the most specific locale.

See [`Intl.Locale.prototype.maximize`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/maximize) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("en")
locale->Intl.Locale.maximize->Intl.Locale.region == Some("US")
```
*/
@send external maximize: t => t = "maximize"

/**
`minimize(locale)` removes unnecessary subtags while preserving semantics.

See [`Intl.Locale.prototype.minimize`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/minimize) on MDN.

## Examples

```rescript
let locale = Intl.Locale.make("en-Latn-US")
locale->Intl.Locale.minimize->Intl.Locale.baseName == "en"
```
*/
@send external minimize: t => t = "minimize"

/**
  `ignore(locale)` ignores the provided locale and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

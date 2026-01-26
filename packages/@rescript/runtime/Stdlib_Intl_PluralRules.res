@notUndefined
type t

type localeType = [#cardinal | #ordinal]

type options = {
  localeMatcher?: Stdlib_Intl_Common.localeMatcher,
  \"type"?: localeType,
  // use either this group
  minimumIntegerDigits?: Stdlib_Intl_Common.oneTo21,
  minimumFractionDigits?: Stdlib_Intl_Common.zeroTo20,
  maximumFractionDigits?: Stdlib_Intl_Common.zeroTo20,
  // OR this group
  minimumSignificantDigits?: Stdlib_Intl_Common.oneTo21,
  maximumSignificantDigits?: Stdlib_Intl_Common.oneTo21,
}

type pluralCategories = [
  | #zero
  | #one
  | #two
  | #few
  | #many
  | #other
]

type resolvedOptions = {
  locale: string,
  pluralCategories: array<pluralCategories>,
  \"type": localeType,
  // either this group
  minimumIntegerDigits?: Stdlib_Intl_Common.oneTo21,
  minimumFractionDigits?: Stdlib_Intl_Common.zeroTo20,
  maximumFractionDigits?: Stdlib_Intl_Common.zeroTo20,
  // OR this group
  minimumSignificantDigits?: Stdlib_Intl_Common.oneTo21,
  maximumSignificantDigits?: Stdlib_Intl_Common.oneTo21,
}

type supportedLocalesOptions = {localeMatcher: Stdlib_Intl_Common.localeMatcher}

/**
Creates a new `Intl.PluralRules` instance to determine plural categories.

See [`Intl.PluralRules`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/PluralRules) on MDN.

## Examples

```rescript
let rules = Intl.PluralRules.make(~locales=["en"])
let category = rules->Intl.PluralRules.select(1.0)
category == #one
```
*/
@new external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.PluralRules"

/**
`supportedLocalesOf(locales, ~options)` filters `locales` to those supported for plural rules.

See [`Intl.PluralRules.supportedLocalesOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/PluralRules/supportedLocalesOf) on MDN.

## Examples

```rescript
Intl.PluralRules.supportedLocalesOf(["en-US", "klingon"]) == ["en-US"]
```
*/
@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => array<string> =
  "Intl.PluralRules.supportedLocalesOf"

/**
`resolvedOptions(rules)` returns the plural rule configuration in use.

## Examples

```rescript
let rules = Intl.PluralRules.make(~locales=["en"])
Intl.PluralRules.resolvedOptions(rules).locale == "en"
```
*/
@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

type rule = [#zero | #one | #two | #few | #many | #other]

/**
`select(rules, value)` returns the plural category for the given number.

## Examples

```rescript
let rules = Intl.PluralRules.make(~locales=["en"])
rules->Intl.PluralRules.select(1.) == #one
```
*/
@send external select: (t, float) => rule = "select"
/**
`selectInt(rules, value)` is like `select` but accepts an integer.

## Examples

```rescript
let rules = Intl.PluralRules.make(~locales=["en"])
rules->Intl.PluralRules.selectInt(2) == #other
```
*/
@send external selectInt: (t, int) => rule = "select"

/**
`selectRange(rules, ~start, ~end)` returns the category for numbers in the range.

## Examples

```rescript
let rules = Intl.PluralRules.make(~locales=["en"])
rules->Intl.PluralRules.selectRange(~start=1., ~end=2.) == #other
```
*/
@send
external selectRange: (t, ~start: float, ~end: float) => rule = "selectRange"

/**
`selectRangeInt(rules, ~start, ~end)` is the integer version of `selectRange`.

## Examples

```rescript
let rules = Intl.PluralRules.make(~locales=["en"])
rules->Intl.PluralRules.selectRangeInt(~start=1, ~end=1) == #other
```
*/
@send
external selectRangeInt: (t, ~start: int, ~end: int) => rule = "selectRange"

/**
  `ignore(pluralRules)` ignores the provided pluralRules and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

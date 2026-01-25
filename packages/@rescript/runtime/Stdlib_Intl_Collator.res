@notUndefined
type t

type usage = [#sort | #search]
type sensitivity = [#base | #accent | #case | #variant]
type caseFirst = [#upper | #lower | #"false"]

type options = {
  localeMatcher?: Stdlib_Intl_Common.localeMatcher,
  usage?: usage,
  sensitivity?: sensitivity,
  ignorePunctuation?: bool,
  numeric?: bool,
  caseFirst?: caseFirst,
}

type resolvedOptions = {
  locale: string,
  usage: usage,
  sensitivity: sensitivity,
  ignorePunctuation: bool,
  collation: [Stdlib_Intl_Common.collation | #default],
  numeric?: bool,
  caseFirst?: caseFirst,
}

type supportedLocalesOptions = {localeMatcher: Stdlib_Intl_Common.localeMatcher}

/**
Creates a new `Intl.Collator` instance that can compare strings using locale-aware rules.

See [`Intl.Collator`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Collator) on MDN.

## Examples

```rescript
let collator = Intl.Collator.make(~locales=["en"])
collator->Intl.Collator.compare("apple", "banana") < 0
```
*/
@new external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.Collator"

/**
`supportedLocalesOf(locales, ~options)` filters `locales` to those supported by the runtime for collation.

See [`Intl.Collator.supportedLocalesOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Collator/supportedLocalesOf) on MDN.

## Examples

```rescript
Intl.Collator.supportedLocalesOf(["en-US", "klingon"]) == ["en-US"]

```
*/
@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => array<string> =
  "Intl.Collator.supportedLocalesOf"

/**
`resolvedOptions(collator)` returns the locale and collation settings in use.

See [`Intl.Collator.prototype.resolvedOptions`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Collator/resolvedOptions) on MDN.

## Examples

```rescript
let collator = Intl.Collator.make(~locales=["en-US"])
Intl.Collator.resolvedOptions(collator).locale == "en-US"
```
*/
@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

/**
`compare(collator, a, b)` compares two strings using the rules of `collator`. Returns a negative number when `a` comes before `b`, `0` when equal, and a positive number otherwise.

## Examples

```rescript
let collator = Intl.Collator.make(~locales=["en-US"])
collator->Intl.Collator.compare("apple", "banana") < 0
```
*/
@send external compare: (t, string, string) => int = "compare"

/**
  `ignore(collator)` ignores the provided collator and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

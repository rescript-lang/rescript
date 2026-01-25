/***
Bindings to JavaScript's `Intl.Segmenter`.

See [MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Segmenter) for API details.
*/
@notUndefined
type t

type granularity = [#grapheme | #word | #sentence]

type options = {
  localeMatcher?: Stdlib_Intl_Common.localeMatcher,
  granularity?: granularity,
}

type pluralCategories = [
  | #zero
  | #one
  | #two
  | #few
  | #many
  | #other
]

type resolvedOptions = {locale: string, granularity: granularity}

type supportedLocalesOptions = {localeMatcher: Stdlib_Intl_Common.localeMatcher}

/**
Creates a new `Intl.Segmenter` instance for segmenting strings.

See [`Intl.Segmenter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Segmenter) on MDN.

## Examples

```rescript
let segmenter = Intl.Segmenter.make(~locales=["en"], ~options={granularity: #word})
Intl.Segmenter.resolvedOptions(segmenter).granularity == #word
```
*/
@new
external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.Segmenter"

/**
`supportedLocalesOf(locales, ~options)` filters `locales` to those supported for segmentation.

See [`Intl.Segmenter.supportedLocalesOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Segmenter/supportedLocalesOf) on MDN.

## Examples

```rescript
Intl.Segmenter.supportedLocalesOf(["en-US", "klingon"]) == ["en-US"]
```
*/
@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => array<string> =
  "Intl.Segmenter.supportedLocalesOf"

/**
`resolvedOptions(segmenter)` returns the locale and granularity currently in use.

See [`Intl.Segmenter.prototype.resolvedOptions`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Segmenter/resolvedOptions) on MDN.

## Examples

```rescript
let segmenter = Intl.Segmenter.make(~locales=["en"], ~options={granularity: #sentence})
Intl.Segmenter.resolvedOptions(segmenter).granularity == #sentence
```
*/
@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

/**
`segment(segmenter, input)` returns a `Segments` object describing `input`.

See [`Intl.Segmenter.prototype.segment`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Segmenter/segment) on MDN.

## Examples

```rescript
let segmenter = Intl.Segmenter.make(~locales=["en"], ~options={granularity: #word})
let segments = segmenter->Intl.Segmenter.segment("Hello world")
Intl.Segments.containingWithIndex(segments, 0).segment == "Hello"
```
*/
@send external segment: (t, string) => Stdlib_Intl_Segments.t = "segment"

/**
  `ignore(segmenter)` ignores the provided segmenter and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

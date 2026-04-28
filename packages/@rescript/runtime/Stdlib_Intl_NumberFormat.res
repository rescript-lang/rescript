module Grouping = Stdlib_Intl_NumberFormat_Grouping

@notUndefined
type t

/**
An ISO 4217 currency code. e.g. USD, EUR, CNY
*/
type currency = string
type currencyDisplay = [#symbol | #narrowSymbol | #code | #name]
type currencySign = [#accounting | #standard]
type notation = [#scientific | #standard | #engineering | #compact]

/**
Used only when notation is #compact
*/
type compactDisplay = [#short | #long]

type signDisplay = [
  | #auto
  | #always
  | #exceptZero
  | #never
  | #negative
]

type style = [#decimal | #currency | #percent | #unit]

/**
Defined in https://tc39.es/proposal-unified-intl-numberformat/section6/locales-currencies-tz_proposed_out.html#sec-issanctionedsimpleunitidentifier
Only used when style is #unit
*/
type unitSystem = string

/**
Only used when style is #unit
*/
type unitDisplay = [#long | #short | #narrow]

type rounding = [
  | #ceil
  | #floor
  | #expand
  | #trunc
  | #halfCeil
  | #halfFloor
  | #halfExpand
  | #halfTrunc
  | #halfEven
]

type roundingPriority = [#auto | #morePrecision | #lessPrecision]

type roundingIncrement = [
  | #1
  | #2
  | #5
  | #10
  | #20
  | #25
  | #50
  | #100
  | #200
  | #250
  | #500
  | #1000
  | #2000
  | #2500
  | #5000
]

type trailingZeroDisplay = [#auto | #stripIfInteger | #lessPrecision]

type options = {
  compactDisplay?: compactDisplay,
  numberingSystem?: Stdlib_Intl_Common.numberingSystem,
  currency?: currency,
  currencyDisplay?: currencyDisplay,
  currencySign?: currencySign,
  localeMatcher?: Stdlib_Intl_Common.localeMatcher,
  notation?: notation,
  signDisplay?: signDisplay,
  style?: style,
  /**
  required if style == #unit
  */
  unit?: unitSystem,
  unitDisplay?: unitDisplay,
  useGrouping?: Grouping.t,
  roundingMode?: rounding, // not available in firefox v110
  roundingPriority?: roundingPriority, // not available in firefox v110
  roundingIncrement?: roundingIncrement, // not available in firefox v110
  /**
  Supported everywhere but Firefox as of v110
  */
  trailingZeroDisplay?: trailingZeroDisplay,
  // use either this group
  minimumIntegerDigits?: Stdlib_Intl_Common.oneTo21,
  minimumFractionDigits?: Stdlib_Intl_Common.zeroTo20,
  maximumFractionDigits?: Stdlib_Intl_Common.zeroTo20,
  // OR these
  minimumSignificantDigits?: Stdlib_Intl_Common.oneTo21,
  maximumSignificantDigits?: Stdlib_Intl_Common.oneTo21,
}

type resolvedOptions = {
  // only when style == "currency"
  currency?: currency,
  currencyDisplay?: currencyDisplay,
  currencySign?: currencySign,
  // only when notation == "compact"
  compactDisplay?: compactDisplay,
  // only when style == "unit"
  unit: unitSystem,
  unitDisplay: unitDisplay,
  roundingMode?: rounding, // not available in firefox v110
  roundingPriority?: roundingPriority, // not available in firefox v110
  roundingIncrement?: roundingIncrement, // not available in firefox v110
  // either this group
  minimumIntegerDigits?: Stdlib_Intl_Common.oneTo21,
  minimumFractionDigits?: Stdlib_Intl_Common.zeroTo20,
  maximumFractionDigits?: Stdlib_Intl_Common.zeroTo20,
  // OR these
  minimumSignificantDigits?: Stdlib_Intl_Common.oneTo21,
  maximumSignificantDigits?: Stdlib_Intl_Common.oneTo21,
  // always present
  locale: string,
  notation: notation,
  numberingSystem: Stdlib_Intl_Common.numberingSystem,
  signDisplay: signDisplay,
  style: style,
  useGrouping: Grouping.t,
}

type supportedLocalesOptions = {localeMatcher: Stdlib_Intl_Common.localeMatcher}

type numberFormatPartType = [
  | #compact
  | #currency
  | #decimal
  | #exponentInteger
  | #exponentMinusSign
  | #exponentSeparator
  | #fraction
  | #group
  | #infinity
  | #integer
  | #literal
  | #minusSign
  | #nan
  | #plusSign
  | #percentSign
  | #unit
  | #unknown
]

type numberFormatPart = {
  \"type": numberFormatPartType,
  value: string,
}

type rangeSource = [#startRange | #endRange | #shared]

type numberFormatRangePart = {
  \"type": numberFormatPartType,
  value: string,
  source: rangeSource,
}

/**
Creates a new `Intl.NumberFormat` instance for locale-aware number formatting.

See [`Intl.NumberFormat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/NumberFormat) on MDN.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en-US"], ~options={style: #currency, currency: "USD"})
formatter->Intl.NumberFormat.format(1234.5) == "$1,234.50"
```
*/
@new external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.NumberFormat"

/**
`supportedLocalesOf(locales, ~options)` filters `locales` to those supported for number formatting.

See [`Intl.NumberFormat.supportedLocalesOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/NumberFormat/supportedLocalesOf) on MDN.

## Examples

```rescript
Intl.NumberFormat.supportedLocalesOf(["en-US", "klingon"]) == ["en-US"]
```
*/
@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => array<string> =
  "Intl.NumberFormat.supportedLocalesOf"

/**
`resolvedOptions(formatter)` returns the actual options being used.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en-US"])
Intl.NumberFormat.resolvedOptions(formatter).locale == "en-US"
```
*/
@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

/**
`format(formatter, value)` returns the formatted representation of `value`.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en-US"])
formatter->Intl.NumberFormat.format(1234.5) == "1,234.5"
```
*/
@send external format: (t, float) => string = "format"
/**
`formatRange(formatter, ~start, ~end)` formats numbers representing a range.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en-US"])
formatter->Intl.NumberFormat.formatRange(~start=1., ~end=2.)->String.length > 0
```
*/
@send
external formatRange: (t, ~start: float, ~end: float) => string = "formatRange"
/**
`formatToParts(formatter, value)` breaks the formatted result into parts.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en-US"])
formatter->Intl.NumberFormat.formatToParts(123)->Array.length > 0
```
*/
@send
external formatToParts: (t, float) => array<numberFormatPart> = "formatToParts"
/**
`formatRangeToParts(formatter, ~start, ~end)` returns how the range would be rendered.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en-US"])
formatter->Intl.NumberFormat.formatRangeToParts(~start=1., ~end=2.)->Array.length > 0
```
*/
@send
external formatRangeToParts: (t, ~start: float, ~end: float) => array<numberFormatRangePart> =
  "formatRangeToParts"

/**
`formatInt(formatter, value)` formats integer values.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en"])
formatter->Intl.NumberFormat.formatInt(42) == "42"
```
*/
@send external formatInt: (t, int) => string = "format"

/**
`formatIntRange(formatter, ~start, ~end)` formats integer ranges.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en"])
formatter->Intl.NumberFormat.formatIntRange(~start=1, ~end=3)->String.length > 0
```
*/
@send
external formatIntRange: (t, ~start: int, ~end: int) => string = "formatRange"
/**
`formatIntToParts(formatter, value)` returns formatting parts for an integer.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en"])
formatter->Intl.NumberFormat.formatIntToParts(123)->Array.length > 0
```
*/
@send
external formatIntToParts: (t, int) => array<numberFormatPart> = "formatToParts"

/**
`formatIntRangeToParts(formatter, ~start, ~end)` returns how the integer range would be rendered.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en"])
formatter->Intl.NumberFormat.formatIntRangeToParts(~start=1, ~end=1)->Array.length > 0
```
*/
@send
external formatIntRangeToParts: (t, ~start: int, ~end: int) => array<numberFormatRangePart> =
  "formatRangeToParts"

/**
`formatBigInt(formatter, value)` formats bigint values.
*/
@send external formatBigInt: (t, bigint) => string = "format"

/**
`formatBigIntRange(formatter, ~start, ~end)` formats a range of bigint values.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en"])
formatter->Intl.NumberFormat.formatBigIntRange(~start=1n, ~end=2n) == "1–2"
```
*/
@send
external formatBigIntRange: (t, ~start: bigint, ~end: bigint) => string = "formatRange"
/**
`formatBigIntToParts(formatter, value)` returns the bigint formatting broken into parts.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en"])
formatter->Intl.NumberFormat.formatBigIntToParts(5n)->Array.length > 0
```
*/
@send
external formatBigIntToParts: (t, bigint) => array<numberFormatPart> = "formatToParts"

/**
`formatBigIntRangeToParts(formatter, ~start, ~end)` describes how the bigint range would be rendered.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en"])
formatter->Intl.NumberFormat.formatBigIntRangeToParts(~start=3n, ~end=4n)->Array.length > 0
```
*/
@send
external formatBigIntRangeToParts: (
  t,
  ~start: bigint,
  ~end: bigint,
) => array<numberFormatRangePart> = "formatRangeToParts"

/**
`formatString(formatter, value)` interprets `value` as a number string and formats it.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en"])
formatter->Intl.NumberFormat.formatString("1234") == "1,234"
```
*/
@send external formatString: (t, string) => string = "format"

/**
`formatStringToParts(formatter, value)` returns formatting parts for a numeric string.

## Examples

```rescript
let formatter = Intl.NumberFormat.make(~locales=["en"])
formatter->Intl.NumberFormat.formatStringToParts("123")->Array.length > 0
```
*/
@send
external formatStringToParts: (t, string) => array<numberFormatPart> = "formatToParts"

/**
  `ignore(numberFormat)` ignores the provided numberFormat and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

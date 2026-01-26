/***
Bindings to JavaScript's `Intl.DateTimeFormat`.
*/

@notUndefined
type t

type dateStyle = [#full | #long | #medium | #short]
type timeStyle = [#full | #long | #medium | #short]
type dayPeriod = [#narrow | #short | #long]
type weekday = [#narrow | #short | #long]
type era = [#narrow | #short | #long]
type year = [#numeric | #"2-digit"]
type month = [#numeric | #"2-digit" | #narrow | #short | #long]
type day = [#numeric | #"2-digit"]
type hour = [#numeric | #"2-digit"]
type minute = [#numeric | #"2-digit"]
type second = [#numeric | #"2-digit"]

/**
Firefox also supports IANA time zone names here
Node v19+ supports "shortOffset", "shortGeneric", "longOffset", and "longGeneric".
*/
type timeZoneName = [
  | #short
  | #long
  | #shortOffset
  | #shortGeneric
  | #longOffset
  | #longGeneric
]

type hourCycle = [#h11 | #h12 | #h23 | #h24]
type formatMatcher = [#basic | #"best fit"]
type fractionalSecondDigits = [#0 | #1 | #2 | #3]

type options = {
  dateStyle?: dateStyle, // can be used with timeStyle, but not other options
  timeStyle?: timeStyle, // can be used with dateStyle, but not other options
  calendar?: Stdlib_Intl_Common.calendar,
  dayPeriod?: dayPeriod, // only has an effect if a 12-hour clock is used
  numberingSystem?: Stdlib_Intl_Common.numberingSystem,
  localeMatcher?: Stdlib_Intl_Common.localeMatcher,
  timeZone?: string,
  hour12?: bool,
  hourCycle?: hourCycle,
  formatMatcher?: formatMatcher,
  // date-time components
  weekday?: weekday,
  era?: era,
  year?: year,
  month?: month,
  day?: day,
  hour?: hour,
  minute?: minute,
  second?: second,
  fractionalSecondDigits?: fractionalSecondDigits,
  timeZoneName?: timeZoneName,
}

type resolvedOptions = {
  dateStyle?: dateStyle,
  timeStyle?: timeStyle,
  weekday?: weekday,
  era?: era,
  year?: year,
  month?: month,
  day?: day,
  hour?: hour,
  minute?: minute,
  second?: second,
  fractionalSecondDigits?: fractionalSecondDigits,
  timeZoneName?: timeZoneName,
  calendar: Stdlib_Intl_Common.calendar,
  hour12: bool,
  hourCycle: hourCycle,
  locale: string,
  numberingSystem: Stdlib_Intl_Common.numberingSystem,
  timeZone: string,
}

type supportedLocalesOptions = {localeMatcher: Stdlib_Intl_Common.localeMatcher}

type dateTimeComponent = [
  | #day
  | #dayPeriod
  | #era
  | #fractionalSecond
  | #hour
  | #literal
  | #minute
  | #month
  | #relatedYear
  | #second
  | #timeZone
  | #weekday
  | #year
  | #yearName
]

type dateTimePart = {
  \"type": dateTimeComponent,
  value: string,
}

type dateTimeRangeSource = [#startRange | #shared | #endRange]
type dateTimeRangePart = {
  \"type": dateTimeComponent,
  value: string,
  source: dateTimeRangeSource,
}

/**
Creates a new `Intl.DateTimeFormat` instance for formatting date values.

See [`Intl.DateTimeFormat`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat) on MDN.

## Examples

```rescript
let formatter = Intl.DateTimeFormat.make(~locales=["en-US"], ~options={timeStyle: #short})
let sampleDate = Js.Date.makeWithYMD(~year=2024, ~month=0, ~date=1)
formatter->Intl.DateTimeFormat.format(sampleDate)->String.length > 0
```
*/
@new external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.DateTimeFormat"

/**
`supportedLocalesOf(locales, ~options)` filters `locales` to those supported by the runtime for date/time formatting.

See [`Intl.DateTimeFormat.supportedLocalesOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/supportedLocalesOf) on MDN.

## Examples

```rescript
Intl.DateTimeFormat.supportedLocalesOf(["en-US", "klingon"]) == ["en-US"]
```
*/
@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => array<string> =
  "Intl.DateTimeFormat.supportedLocalesOf"

/**
`resolvedOptions(formatter)` returns the actual locale and formatting options in use.

See [`Intl.DateTimeFormat.prototype.resolvedOptions`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/resolvedOptions) on MDN.

## Examples

```rescript
let formatter = Intl.DateTimeFormat.make(~locales=["en-US"])
Intl.DateTimeFormat.resolvedOptions(formatter).locale == "en-US"
```
*/
@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

/**
`format(formatter, date)` returns the formatted string for `date`.

## Examples

```rescript
let formatter = Intl.DateTimeFormat.make(~locales=["en"])
let date = Js.Date.makeWithYMD(~year=2024, ~month=0, ~date=1)
formatter->Intl.DateTimeFormat.format(date)->String.length > 0
```
*/
@send external format: (t, Stdlib_Date.t) => string = "format"

/**
`formatToParts(formatter, date)` breaks the formatted output into an array of parts.

See [`Intl.DateTimeFormat.prototype.formatToParts`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/formatToParts) on MDN.

## Examples

```rescript
let formatter = Intl.DateTimeFormat.make(~locales=["en"])
let date = Js.Date.makeWithYMD(~year=2024, ~month=0, ~date=1)
formatter->Intl.DateTimeFormat.formatToParts(date)->Array.length > 0
```
*/
@send external formatToParts: (t, Stdlib_Date.t) => array<dateTimePart> = "formatToParts"

/**
`formatRange(formatter, ~startDate, ~endDate)` formats the range between `startDate` and `endDate`.

See [`Intl.DateTimeFormat.prototype.formatRange`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/formatRange) on MDN.

## Examples

```rescript
let formatter = Intl.DateTimeFormat.make(~locales=["en-US"], ~options={dateStyle: #short})
let startDate = Js.Date.makeWithYMD(~year=2024, ~month=0, ~date=1)
let endDate = Js.Date.makeWithYMD(~year=2024, ~month=1, ~date=1)
formatter->Intl.DateTimeFormat.formatRange(~startDate=startDate, ~endDate=endDate)->String.length > 0
```
*/
@send
external formatRange: (t, ~startDate: Stdlib_Date.t, ~endDate: Stdlib_Date.t) => string =
  "formatRange"

/**
`formatRangeToParts(formatter, ~startDate, ~endDate)` returns an array describing how the range would be rendered.

See [`Intl.DateTimeFormat.prototype.formatRangeToParts`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/formatRangeToParts) on MDN.

## Examples

```rescript
let formatter = Intl.DateTimeFormat.make(~locales=["en-US"], ~options={dateStyle: #short})
let startDate = Js.Date.makeWithYMD(~year=2024, ~month=0, ~date=1)
let endDate = Js.Date.makeWithYMD(~year=2024, ~month=1, ~date=1)
formatter->Intl.DateTimeFormat.formatRangeToParts(~startDate=startDate, ~endDate=endDate)->Array.length > 0
```
*/
@send
external formatRangeToParts: (
  t,
  ~startDate: Stdlib_Date.t,
  ~endDate: Stdlib_Date.t,
) => array<dateTimeRangePart> = "formatRangeToParts"

/**
  `ignore(dateTimeFormat)` ignores the provided dateTimeFormat and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

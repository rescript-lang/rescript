/***
Bindings to JavaScript's `Temporal.PlainYearMonth`, a month of a specific year,
like a billing period.

Compare year-months with `Temporal.PlainYearMonth.equals` and
`Temporal.PlainYearMonth.compare`. ReScript's `==`, `compare`, `<` and `>` treat
any two year-months as equal.

See [`Temporal.PlainYearMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth) on MDN.
*/

type t = Stdlib_Temporal_Common.plainYearMonth

/**
The fields of a year and month. A year and month needs `year` and `month` (or
`monthCode`). In a calendar with eras, like `#gregory`, `era` and `eraYear` can
replace `year`. `calendar` defaults to `#iso8601`, which has no eras.
*/
type fields = {
  year?: int,
  era?: string,
  eraYear?: int,
  month?: int,
  monthCode?: string,
  calendar?: Stdlib_Temporal_Common.calendar,
}

/**
The fields `with` can replace. The calendar can't be replaced, and at least
one field must be given.
*/
type updates = {
  year?: int,
  era?: string,
  eraYear?: int,
  month?: int,
  monthCode?: string,
}

type toPlainDateOptions = {day: int}

/**
`make(~year, ~month, ~calendar=?, ~referenceDay=?)` creates a year and month
from ISO 8601 year and month numbers. Months are numbered from 1.
`referenceDay` is the ISO day used to identify the month in non-ISO
calendars.

Throws a `RangeError` when a field is out of range.

See [`Temporal.PlainYearMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/PlainYearMonth) on MDN.

## Examples

```rescript
Temporal.PlainYearMonth.make(~year=2024, ~month=2)->Temporal.PlainYearMonth.toString == "2024-02"
```
*/
@new
external make: (
  ~year: int,
  ~month: int,
  ~calendar: Stdlib_Temporal_Common.calendar=?,
  ~referenceDay: int=?,
) => t = "Temporal.PlainYearMonth"

/**
`fromStringOrThrow(string)` parses an ISO 8601 year and month, like
`"2024-02"`.

Throws a `RangeError` when `string` is not a valid year and month.

See [`Temporal.PlainYearMonth.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/from) on MDN.

## Examples

```rescript
Temporal.PlainYearMonth.fromStringOrThrow("2024-02")->Temporal.PlainYearMonth.daysInMonth == 29
```
*/
@val
external fromStringOrThrow: string => t = "Temporal.PlainYearMonth.from"

/**
`fromString(string)` parses an ISO 8601 year and month like
`fromStringOrThrow`, but returns `None` instead of throwing when `string` is
not a valid year and month.

## Examples

```rescript
Temporal.PlainYearMonth.fromString("2024-02")->Option.map(Temporal.PlainYearMonth.month) ==
  Some(2)
Temporal.PlainYearMonth.fromString("2024-13") == None
```
*/
let fromString = (value: string) =>
  try Some(fromStringOrThrow(value)) catch {
  | JsExn(error) =>
    switch Stdlib_JsExn.name(error) {
    | Some("RangeError") => None
    | _ => Stdlib_JsExn.throw(error)
    }
  }

/**
`fromFields(fields, ~options=?)` creates a year and month from its fields.
Out-of-range fields are clamped unless `options.overflow` is `#reject`.

See [`Temporal.PlainYearMonth.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/from) on MDN.

## Examples

```rescript
Temporal.PlainYearMonth.fromFields({year: 2024, month: 13})->Temporal.PlainYearMonth.toString ==
  "2024-12"
```
*/
@val
external fromFields: (fields, ~options: Stdlib_Temporal_Common.overflowOptions=?) => t =
  "Temporal.PlainYearMonth.from"

/**
`compare(a, b)` compares two year-months chronologically.

See [`Temporal.PlainYearMonth.compare`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/compare) on MDN.

## Examples

```rescript
Temporal.PlainYearMonth.compare(
  Temporal.PlainYearMonth.fromStringOrThrow("2024-02"),
  Temporal.PlainYearMonth.fromStringOrThrow("2024-02"),
) == Ordering.equal
```
*/
@val
external compare: (t, t) => Stdlib_Ordering.t = "Temporal.PlainYearMonth.compare"

/**
`calendarId(yearMonth)` returns the calendar of `yearMonth`.

See [`Temporal.PlainYearMonth.prototype.calendarId`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/calendarId) on MDN.
*/
@get
external calendarId: t => Stdlib_Temporal_Common.calendar = "calendarId"

/**
`era(yearMonth)` returns the era of `yearMonth`, or `None` when its calendar
has no eras.

See [`Temporal.PlainYearMonth.prototype.era`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/era) on MDN.
*/
@get
external era: t => option<string> = "era"

/**
`eraYear(yearMonth)` returns the year of `yearMonth` within its era, or `None`
when its calendar has no eras.

See [`Temporal.PlainYearMonth.prototype.eraYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/eraYear) on MDN.
*/
@get
external eraYear: t => option<int> = "eraYear"

/**
`year(yearMonth)` returns the year of `yearMonth`.

See [`Temporal.PlainYearMonth.prototype.year`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/year) on MDN.
*/
@get
external year: t => int = "year"

/**
`month(yearMonth)` returns the month of `yearMonth`, numbered from 1.

See [`Temporal.PlainYearMonth.prototype.month`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/month) on MDN.
*/
@get
external month: t => int = "month"

/**
`monthCode(yearMonth)` returns the calendar-specific code of the month of
`yearMonth`, like `"M02"`.

See [`Temporal.PlainYearMonth.prototype.monthCode`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/monthCode) on MDN.
*/
@get
external monthCode: t => string = "monthCode"

/**
`daysInYear(yearMonth)` returns the number of days in the year of
`yearMonth`.

See [`Temporal.PlainYearMonth.prototype.daysInYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/daysInYear) on MDN.
*/
@get
external daysInYear: t => int = "daysInYear"

/**
`daysInMonth(yearMonth)` returns the number of days in `yearMonth`.

See [`Temporal.PlainYearMonth.prototype.daysInMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/daysInMonth) on MDN.
*/
@get
external daysInMonth: t => int = "daysInMonth"

/**
`monthsInYear(yearMonth)` returns the number of months in the year of
`yearMonth`.

See [`Temporal.PlainYearMonth.prototype.monthsInYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/monthsInYear) on MDN.
*/
@get
external monthsInYear: t => int = "monthsInYear"

/**
`inLeapYear(yearMonth)` returns whether the year of `yearMonth` is a leap
year.

See [`Temporal.PlainYearMonth.prototype.inLeapYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/inLeapYear) on MDN.
*/
@get
external inLeapYear: t => bool = "inLeapYear"

/**
`with(yearMonth, fields, ~options=?)` returns a copy of `yearMonth` with
`fields` replaced.

See [`Temporal.PlainYearMonth.prototype.with`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/with) on MDN.

## Examples

```rescript
Temporal.PlainYearMonth.fromStringOrThrow("2024-02")
->Temporal.PlainYearMonth.with({month: 3})
->Temporal.PlainYearMonth.toString == "2024-03"
```
*/
@send
external with: (t, updates, ~options: Stdlib_Temporal_Common.overflowOptions=?) => t = "with"

/**
`add(yearMonth, duration, ~options=?)` moves `yearMonth` forward by
`duration`. Only years and months can be added; other units throw a
`RangeError`.

See [`Temporal.PlainYearMonth.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/add) on MDN.
*/
@send
external add: (
  t,
  Stdlib_Temporal_Common.duration,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "add"

/**
`addFields(yearMonth, fields, ~options=?)` moves `yearMonth` forward by the
duration in `fields`. Only years and months can be added; other units throw a
`RangeError`.

See [`Temporal.PlainYearMonth.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/add) on MDN.

## Examples

```rescript
Temporal.PlainYearMonth.fromStringOrThrow("2024-02")
->Temporal.PlainYearMonth.addFields({months: 11.})
->Temporal.PlainYearMonth.toString == "2025-01"
```
*/
@send
external addFields: (
  t,
  Stdlib_Temporal_Common.durationFields,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "add"

/**
`subtract(yearMonth, duration, ~options=?)` moves `yearMonth` backward by
`duration`. Only years and months can be subtracted; other units throw a
`RangeError`.

See [`Temporal.PlainYearMonth.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/subtract) on MDN.
*/
@send
external subtract: (
  t,
  Stdlib_Temporal_Common.duration,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "subtract"

/**
`subtractFields(yearMonth, fields, ~options=?)` moves `yearMonth` backward by
the duration in `fields`. Only years and months can be subtracted; other
units throw a `RangeError`.

See [`Temporal.PlainYearMonth.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/subtract) on MDN.
*/
@send
external subtractFields: (
  t,
  Stdlib_Temporal_Common.durationFields,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "subtract"

/**
`until(yearMonth, other, ~options=?)` returns the duration from `yearMonth` to
`other`. The largest unit defaults to `#year`.

See [`Temporal.PlainYearMonth.prototype.until`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/until) on MDN.

## Examples

```rescript
Temporal.PlainYearMonth.fromStringOrThrow("2024-02")
->Temporal.PlainYearMonth.until(Temporal.PlainYearMonth.fromStringOrThrow("2025-05"))
->Temporal.Duration.toString == "P1Y3M"
```
*/
@send
external until: (
  t,
  t,
  ~options: Stdlib_Temporal_Common.differenceOptions<[#year | #month]>=?,
) => Stdlib_Temporal_Common.duration = "until"

/**
`since(yearMonth, other, ~options=?)` returns the duration from `other` to
`yearMonth`. The largest unit defaults to `#year`.

See [`Temporal.PlainYearMonth.prototype.since`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/since) on MDN.
*/
@send
external since: (
  t,
  t,
  ~options: Stdlib_Temporal_Common.differenceOptions<[#year | #month]>=?,
) => Stdlib_Temporal_Common.duration = "since"

/**
`equals(a, b)` returns whether two year-months are the same in the same
calendar.

See [`Temporal.PlainYearMonth.prototype.equals`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/equals) on MDN.
*/
@send
external equals: (t, t) => bool = "equals"

/**
`toPlainDate(yearMonth, options)` returns the date on `options.day` of
`yearMonth`.

See [`Temporal.PlainYearMonth.prototype.toPlainDate`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/toPlainDate) on MDN.

## Examples

```rescript
Temporal.PlainYearMonth.fromStringOrThrow("2024-02")
->Temporal.PlainYearMonth.toPlainDate({day: 15})
->Temporal.PlainDate.toString == "2024-02-15"
```
*/
@send
external toPlainDate: (t, toPlainDateOptions) => Stdlib_Temporal_Common.plainDate = "toPlainDate"

/**
`toString(yearMonth, ~options=?)` returns `yearMonth` as an ISO 8601 string.

See [`Temporal.PlainYearMonth.prototype.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/toString) on MDN.
*/
@send
external toString: (t, ~options: Stdlib_Temporal_Common.dateToStringOptions=?) => string =
  "toString"

/**
`toJSON(yearMonth)` returns `yearMonth` as an ISO 8601 string.

See [`Temporal.PlainYearMonth.prototype.toJSON`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/toJSON) on MDN.
*/
@send
external toJSON: t => string = "toJSON"

/**
`toLocaleString(yearMonth, ~locales=?, ~options=?)` returns `yearMonth`
formatted for `locales`. The formatter's calendar must match the calendar of
`yearMonth`, or a `RangeError` is thrown. Most locales default to
`#gregory`, so a year-month in the ISO 8601 calendar needs
`options.calendar` set to `#iso8601`.

See [`Temporal.PlainYearMonth.prototype.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainYearMonth/toLocaleString) on MDN.

## Examples

```rescript
Temporal.PlainYearMonth.fromStringOrThrow("2024-02")->Temporal.PlainYearMonth.toLocaleString(
  ~locales=["en-US"],
  ~options={calendar: #iso8601},
) == "2024-02"
```
*/
@send
external toLocaleString: (
  t,
  ~locales: array<string>=?,
  ~options: Stdlib_Intl_DateTimeFormat.options=?,
) => string = "toLocaleString"

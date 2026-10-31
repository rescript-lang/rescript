/***
Bindings to JavaScript's `Temporal.PlainDate`, a calendar date without a time
or time zone.

Compare dates with `Temporal.PlainDate.equals` and `Temporal.PlainDate.compare`.
ReScript's `==`, `compare`, `<` and `>` treat any two dates as equal.

See [`Temporal.PlainDate`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate) on MDN.
*/

type t = Stdlib_Temporal_Common.plainDate

type toZonedDateTimeOptions = {
  timeZone: string,
  /** Defaults to the start of the day in `timeZone`. */
  plainTime?: Stdlib_Temporal_Common.plainTime,
}

/**
`make(~year, ~month, ~day, ~calendar=?)` creates a date from ISO 8601 year,
month and day numbers. Months are numbered from 1.

Throws a `RangeError` when the date is not valid.

See [`Temporal.PlainDate`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/PlainDate) on MDN.

## Examples

```rescript
Temporal.PlainDate.make(~year=2024, ~month=2, ~day=29)->Temporal.PlainDate.toString == "2024-02-29"
```
*/
@new
external make: (
  ~year: int,
  ~month: int,
  ~day: int,
  ~calendar: Stdlib_Temporal_Common.calendar=?,
) => t = "Temporal.PlainDate"

/**
`fromStringOrThrow(string)` parses an ISO 8601 date, like `"2024-02-29"`.

Throws a `RangeError` when `string` is not a valid date.

See [`Temporal.PlainDate.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/from) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")->Temporal.PlainDate.day == 29
```
*/
@val
external fromStringOrThrow: string => t = "Temporal.PlainDate.from"

/**
`fromString(string)` parses an ISO 8601 date like `fromStringOrThrow`, but
returns `None` instead of throwing when `string` is not a valid date.

## Examples

```rescript
Temporal.PlainDate.fromString("2024-02-29")->Option.map(Temporal.PlainDate.day) == Some(29)
Temporal.PlainDate.fromString("2024-02-30") == None
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
`fromFields(fields, ~options=?)` creates a date from its fields. Out-of-range
fields are clamped unless `options.overflow` is `#reject`.

See [`Temporal.PlainDate.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/from) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromFields({year: 2024, month: 2, day: 30})->Temporal.PlainDate.toString ==
  "2024-02-29"
```
*/
@val
external fromFields: (
  Stdlib_Temporal_Common.dateFields,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "Temporal.PlainDate.from"

/**
`compare(a, b)` compares two dates chronologically.

See [`Temporal.PlainDate.compare`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/compare) on MDN.

## Examples

```rescript
["2024-03-01", "2024-01-01"]
->Array.map(Temporal.PlainDate.fromStringOrThrow)
->Array.toSorted(Temporal.PlainDate.compare)
->Array.map(date => date->Temporal.PlainDate.toString) == ["2024-01-01", "2024-03-01"]
```
*/
@val
external compare: (t, t) => Stdlib_Ordering.t = "Temporal.PlainDate.compare"

/**
`calendarId(date)` returns the calendar of `date`.

See [`Temporal.PlainDate.prototype.calendarId`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/calendarId) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")->Temporal.PlainDate.calendarId == #iso8601
```
*/
@get
external calendarId: t => Stdlib_Temporal_Common.calendar = "calendarId"

/**
`era(date)` returns the era of `date`, or `None` when its calendar has no eras.

See [`Temporal.PlainDate.prototype.era`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/era) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")->Temporal.PlainDate.era == None
```
*/
@get
external era: t => option<string> = "era"

/**
`eraYear(date)` returns the year of `date` within its era, or `None` when its
calendar has no eras.

See [`Temporal.PlainDate.prototype.eraYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/eraYear) on MDN.
*/
@get
external eraYear: t => option<int> = "eraYear"

/**
`year(date)` returns the year of `date`.

See [`Temporal.PlainDate.prototype.year`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/year) on MDN.
*/
@get
external year: t => int = "year"

/**
`month(date)` returns the month of `date`, numbered from 1.

See [`Temporal.PlainDate.prototype.month`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/month) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")->Temporal.PlainDate.month == 2
```
*/
@get
external month: t => int = "month"

/**
`monthCode(date)` returns the calendar-specific code of the month of `date`,
like `"M02"`.

See [`Temporal.PlainDate.prototype.monthCode`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/monthCode) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")->Temporal.PlainDate.monthCode == "M02"
```
*/
@get
external monthCode: t => string = "monthCode"

/**
`day(date)` returns the day of the month of `date`.

See [`Temporal.PlainDate.prototype.day`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/day) on MDN.
*/
@get
external day: t => int = "day"

/**
`dayOfWeek(date)` returns the day of the week of `date`, from 1 (Monday) to 7
(Sunday) in the ISO 8601 calendar.

See [`Temporal.PlainDate.prototype.dayOfWeek`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/dayOfWeek) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")->Temporal.PlainDate.dayOfWeek == 4
```
*/
@get
external dayOfWeek: t => int = "dayOfWeek"

/**
`dayOfYear(date)` returns the day of the year of `date`, numbered from 1.

See [`Temporal.PlainDate.prototype.dayOfYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/dayOfYear) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")->Temporal.PlainDate.dayOfYear == 60
```
*/
@get
external dayOfYear: t => int = "dayOfYear"

/**
`weekOfYear(date)` returns the week of the year of `date`, or `None` when its
calendar has no defined weeks.

See [`Temporal.PlainDate.prototype.weekOfYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/weekOfYear) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")->Temporal.PlainDate.weekOfYear == Some(9)
```
*/
@get
external weekOfYear: t => option<int> = "weekOfYear"

/**
`yearOfWeek(date)` returns the year that `weekOfYear(date)` belongs to, or
`None` when its calendar has no defined weeks.

See [`Temporal.PlainDate.prototype.yearOfWeek`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/yearOfWeek) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-12-30")->Temporal.PlainDate.yearOfWeek == Some(2025)
```
*/
@get
external yearOfWeek: t => option<int> = "yearOfWeek"

/**
`daysInWeek(date)` returns the number of days in the week of `date`.

See [`Temporal.PlainDate.prototype.daysInWeek`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/daysInWeek) on MDN.
*/
@get
external daysInWeek: t => int = "daysInWeek"

/**
`daysInMonth(date)` returns the number of days in the month of `date`.

See [`Temporal.PlainDate.prototype.daysInMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/daysInMonth) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-01")->Temporal.PlainDate.daysInMonth == 29
```
*/
@get
external daysInMonth: t => int = "daysInMonth"

/**
`daysInYear(date)` returns the number of days in the year of `date`.

See [`Temporal.PlainDate.prototype.daysInYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/daysInYear) on MDN.
*/
@get
external daysInYear: t => int = "daysInYear"

/**
`monthsInYear(date)` returns the number of months in the year of `date`.

See [`Temporal.PlainDate.prototype.monthsInYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/monthsInYear) on MDN.
*/
@get
external monthsInYear: t => int = "monthsInYear"

/**
`inLeapYear(date)` returns whether the year of `date` is a leap year.

See [`Temporal.PlainDate.prototype.inLeapYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/inLeapYear) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")->Temporal.PlainDate.inLeapYear == true
```
*/
@get
external inLeapYear: t => bool = "inLeapYear"

/**
`toPlainYearMonth(date)` returns the year and month of `date`.

See [`Temporal.PlainDate.prototype.toPlainYearMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/toPlainYearMonth) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")
->Temporal.PlainDate.toPlainYearMonth
->Temporal.PlainYearMonth.toString == "2024-02"
```
*/
@send
external toPlainYearMonth: t => Stdlib_Temporal_Common.plainYearMonth = "toPlainYearMonth"

/**
`toPlainMonthDay(date)` returns the month and day of `date`.

See [`Temporal.PlainDate.prototype.toPlainMonthDay`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/toPlainMonthDay) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")
->Temporal.PlainDate.toPlainMonthDay
->Temporal.PlainMonthDay.toString == "02-29"
```
*/
@send
external toPlainMonthDay: t => Stdlib_Temporal_Common.plainMonthDay = "toPlainMonthDay"

/**
`add(date, duration, ~options=?)` moves `date` forward by `duration`. A day
that doesn't exist in the resulting month is clamped unless
`options.overflow` is `#reject`.

See [`Temporal.PlainDate.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/add) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-01-31")
->Temporal.PlainDate.add(Temporal.Duration.fromFields({months: 1.}))
->Temporal.PlainDate.toString == "2024-02-29"
```
*/
@send
external add: (
  t,
  Stdlib_Temporal_Common.duration,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "add"

/**
`addFields(date, fields, ~options=?)` moves `date` forward by the duration in
`fields`.

See [`Temporal.PlainDate.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/add) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")
->Temporal.PlainDate.addFields({days: 1.})
->Temporal.PlainDate.toString == "2024-03-01"
```
*/
@send
external addFields: (
  t,
  Stdlib_Temporal_Common.durationFields,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "add"

/**
`subtract(date, duration, ~options=?)` moves `date` backward by `duration`.

See [`Temporal.PlainDate.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/subtract) on MDN.
*/
@send
external subtract: (
  t,
  Stdlib_Temporal_Common.duration,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "subtract"

/**
`subtractFields(date, fields, ~options=?)` moves `date` backward by the
duration in `fields`.

See [`Temporal.PlainDate.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/subtract) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")
->Temporal.PlainDate.subtractFields({years: 1.})
->Temporal.PlainDate.toString == "2023-02-28"
```
*/
@send
external subtractFields: (
  t,
  Stdlib_Temporal_Common.durationFields,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "subtract"

/**
`with(date, fields, ~options=?)` returns a copy of `date` with `fields`
replaced.

See [`Temporal.PlainDate.prototype.with`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/with) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")
->Temporal.PlainDate.with({day: 1})
->Temporal.PlainDate.toString == "2024-02-01"
```
*/
@send
external with: (
  t,
  Stdlib_Temporal_Common.dateUpdates,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "with"

/**
`withCalendar(date, calendar)` returns the same date in `calendar`.

See [`Temporal.PlainDate.prototype.withCalendar`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/withCalendar) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")
->Temporal.PlainDate.withCalendar(#gregory)
->Temporal.PlainDate.calendarId == #gregory
```
*/
@send
external withCalendar: (t, Stdlib_Temporal_Common.calendar) => t = "withCalendar"

/**
`until(date, other, ~options=?)` returns the duration from `date` to `other`.
The largest unit defaults to `#day`.

See [`Temporal.PlainDate.prototype.until`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/until) on MDN.

## Examples

```rescript
let start = Temporal.PlainDate.fromStringOrThrow("2024-01-01")
let end = Temporal.PlainDate.fromStringOrThrow("2024-03-01")
start->Temporal.PlainDate.until(end)->Temporal.Duration.toString == "P60D"
start->Temporal.PlainDate.until(end, ~options={largestUnit: #month})->Temporal.Duration.toString ==
  "P2M"
```
*/
@send
external until: (
  t,
  t,
  ~options: Stdlib_Temporal_Common.differenceOptions<Stdlib_Temporal_Common.dateUnit>=?,
) => Stdlib_Temporal_Common.duration = "until"

/**
`since(date, other, ~options=?)` returns the duration from `other` to `date`.
The largest unit defaults to `#day`.

See [`Temporal.PlainDate.prototype.since`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/since) on MDN.

## Examples

```rescript
let start = Temporal.PlainDate.fromStringOrThrow("2024-01-01")
let end = Temporal.PlainDate.fromStringOrThrow("2024-03-01")
end->Temporal.PlainDate.since(start, ~options={largestUnit: #month})->Temporal.Duration.toString ==
  "P2M"
```
*/
@send
external since: (
  t,
  t,
  ~options: Stdlib_Temporal_Common.differenceOptions<Stdlib_Temporal_Common.dateUnit>=?,
) => Stdlib_Temporal_Common.duration = "since"

/**
`equals(a, b)` returns whether two dates have the same date and calendar.

See [`Temporal.PlainDate.prototype.equals`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/equals) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")->Temporal.PlainDate.equals(
  Temporal.PlainDate.make(~year=2024, ~month=2, ~day=29),
) == true
```
*/
@send
external equals: (t, t) => bool = "equals"

/**
`toPlainDateTime(date, ~time=?)` combines `date` with `time`, which defaults to
midnight.

See [`Temporal.PlainDate.prototype.toPlainDateTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/toPlainDateTime) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")
->Temporal.PlainDate.toPlainDateTime(~time=Temporal.PlainTime.make(~hour=9, ~minute=30))
->Temporal.PlainDateTime.toString == "2024-02-29T09:30:00"
```
*/
@send
external toPlainDateTime: (
  t,
  ~time: Stdlib_Temporal_Common.plainTime=?,
) => Stdlib_Temporal_Common.plainDateTime = "toPlainDateTime"

/**
`toZonedDateTime(date, options)` returns `date` at `options.plainTime` in
`options.timeZone`.

See [`Temporal.PlainDate.prototype.toZonedDateTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/toZonedDateTime) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")
->Temporal.PlainDate.toZonedDateTime({timeZone: "UTC"})
->Temporal.ZonedDateTime.toString == "2024-02-29T00:00:00+00:00[UTC]"
```
*/
@send
external toZonedDateTime: (t, toZonedDateTimeOptions) => Stdlib_Temporal_Common.zonedDateTime =
  "toZonedDateTime"

/**
`toString(date, ~options=?)` returns `date` as an ISO 8601 string.

See [`Temporal.PlainDate.prototype.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/toString) on MDN.

## Examples

```rescript
let date = Temporal.PlainDate.fromStringOrThrow("2024-02-29")
date->Temporal.PlainDate.toString == "2024-02-29"
date->Temporal.PlainDate.toString(~options={calendarName: #always}) == "2024-02-29[u-ca=iso8601]"
```
*/
@send
external toString: (t, ~options: Stdlib_Temporal_Common.dateToStringOptions=?) => string =
  "toString"

/**
`toJSON(date)` returns `date` as an ISO 8601 string.

See [`Temporal.PlainDate.prototype.toJSON`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/toJSON) on MDN.
*/
@send
external toJSON: t => string = "toJSON"

/**
`toLocaleString(date, ~locales=?, ~options=?)` returns `date` formatted for
`locales`. Time options, like `hour` or `timeStyle`, throw a `TypeError`.

See [`Temporal.PlainDate.prototype.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDate/toLocaleString) on MDN.

## Examples

```rescript
Temporal.PlainDate.fromStringOrThrow("2024-02-29")->Temporal.PlainDate.toLocaleString(
  ~locales=["en-US"],
) == "2/29/2024"
```
*/
@send
external toLocaleString: (
  t,
  ~locales: array<string>=?,
  ~options: Stdlib_Intl_DateTimeFormat.options=?,
) => string = "toLocaleString"

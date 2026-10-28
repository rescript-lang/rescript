/***
Bindings to JavaScript's `Temporal.PlainDateTime`, a calendar date and
wall-clock time without a time zone.

Compare date-times with `Temporal.PlainDateTime.equals` and
`Temporal.PlainDateTime.compare`. ReScript's `==`, `compare`, `<` and `>` treat
any two date-times as equal.

See [`Temporal.PlainDateTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime) on MDN.
*/

type t = Stdlib_Temporal_Common.plainDateTime

type toZonedDateTimeOptions = {disambiguation?: Stdlib_Temporal_Common.disambiguation}

/**
`make(~year, ~month, ~day, ~hour=?, ~minute=?, ~second=?, ~millisecond=?, ~microsecond=?, ~nanosecond=?, ~calendar=?)`
creates a date-time from ISO 8601 fields. Months are numbered from 1, and
omitted time fields default to `0`.

Throws a `RangeError` when a field is out of range.

See [`Temporal.PlainDateTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/PlainDateTime) on MDN.

## Examples

```rescript
Temporal.PlainDateTime.make(
  ~year=2024,
  ~month=2,
  ~day=29,
  ~hour=9,
  ~minute=30,
)->Temporal.PlainDateTime.toString == "2024-02-29T09:30:00"
```
*/
@new
external make: (
  ~year: int,
  ~month: int,
  ~day: int,
  ~hour: int=?,
  ~minute: int=?,
  ~second: int=?,
  ~millisecond: int=?,
  ~microsecond: int=?,
  ~nanosecond: int=?,
  ~calendar: Stdlib_Temporal_Common.calendar=?,
) => t = "Temporal.PlainDateTime"

/**
`fromStringOrThrow(string)` parses an ISO 8601 date-time, like
`"2024-02-29T09:30"`.

Throws a `RangeError` when `string` is not a valid date-time.

See [`Temporal.PlainDateTime.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/from) on MDN.

## Examples

```rescript
Temporal.PlainDateTime.fromStringOrThrow("2024-02-29T09:30")->Temporal.PlainDateTime.hour == 9
```
*/
@val
external fromStringOrThrow: string => t = "Temporal.PlainDateTime.from"

/**
`fromString(string)` parses an ISO 8601 date-time like `fromStringOrThrow`, but
returns `None` instead of throwing when `string` is not a valid date-time.

## Examples

```rescript
Temporal.PlainDateTime.fromString("2024-02-29T09:30")->Option.map(Temporal.PlainDateTime.hour) ==
  Some(9)
Temporal.PlainDateTime.fromString("2024-02-29T25:00") == None
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
`fromFields(fields, ~options=?)` creates a date-time from its fields. Omitted
time fields default to `0`, and out-of-range fields are clamped unless
`options.overflow` is `#reject`.

See [`Temporal.PlainDateTime.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/from) on MDN.

## Examples

```rescript
Temporal.PlainDateTime.fromFields({
  year: 2024,
  month: 2,
  day: 29,
  hour: 9,
})->Temporal.PlainDateTime.toString == "2024-02-29T09:00:00"
```
*/
@val
external fromFields: (
  Stdlib_Temporal_Common.dateTimeFields,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "Temporal.PlainDateTime.from"

/**
`compare(a, b)` compares two date-times chronologically.

See [`Temporal.PlainDateTime.compare`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/compare) on MDN.

## Examples

```rescript
Temporal.PlainDateTime.compare(
  Temporal.PlainDateTime.fromStringOrThrow("2024-02-29T09:30"),
  Temporal.PlainDateTime.fromStringOrThrow("2024-02-29T09:00"),
) == Ordering.greater
```
*/
@val
external compare: (t, t) => Stdlib_Ordering.t = "Temporal.PlainDateTime.compare"

/**
`calendarId(dateTime)` returns the calendar of `dateTime`.

See [`Temporal.PlainDateTime.prototype.calendarId`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/calendarId) on MDN.
*/
@get
external calendarId: t => Stdlib_Temporal_Common.calendar = "calendarId"

/**
`era(dateTime)` returns the era of `dateTime`, or `None` when its calendar has
no eras.

See [`Temporal.PlainDateTime.prototype.era`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/era) on MDN.
*/
@get
external era: t => option<string> = "era"

/**
`eraYear(dateTime)` returns the year of `dateTime` within its era, or `None`
when its calendar has no eras.

See [`Temporal.PlainDateTime.prototype.eraYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/eraYear) on MDN.
*/
@get
external eraYear: t => option<int> = "eraYear"

/**
`year(dateTime)` returns the year of `dateTime`.

See [`Temporal.PlainDateTime.prototype.year`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/year) on MDN.
*/
@get
external year: t => int = "year"

/**
`month(dateTime)` returns the month of `dateTime`, numbered from 1.

See [`Temporal.PlainDateTime.prototype.month`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/month) on MDN.
*/
@get
external month: t => int = "month"

/**
`monthCode(dateTime)` returns the calendar-specific code of the month of
`dateTime`, like `"M02"`.

See [`Temporal.PlainDateTime.prototype.monthCode`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/monthCode) on MDN.
*/
@get
external monthCode: t => string = "monthCode"

/**
`day(dateTime)` returns the day of the month of `dateTime`.

See [`Temporal.PlainDateTime.prototype.day`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/day) on MDN.
*/
@get
external day: t => int = "day"

/**
`hour(dateTime)` returns the hour of `dateTime`, from 0 to 23.

See [`Temporal.PlainDateTime.prototype.hour`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/hour) on MDN.
*/
@get
external hour: t => int = "hour"

/**
`minute(dateTime)` returns the minute of `dateTime`, from 0 to 59.

See [`Temporal.PlainDateTime.prototype.minute`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/minute) on MDN.
*/
@get
external minute: t => int = "minute"

/**
`second(dateTime)` returns the second of `dateTime`, from 0 to 59.

See [`Temporal.PlainDateTime.prototype.second`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/second) on MDN.
*/
@get
external second: t => int = "second"

/**
`millisecond(dateTime)` returns the millisecond of `dateTime`, from 0 to 999.

See [`Temporal.PlainDateTime.prototype.millisecond`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/millisecond) on MDN.
*/
@get
external millisecond: t => int = "millisecond"

/**
`microsecond(dateTime)` returns the microsecond of `dateTime`, from 0 to 999.

See [`Temporal.PlainDateTime.prototype.microsecond`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/microsecond) on MDN.
*/
@get
external microsecond: t => int = "microsecond"

/**
`nanosecond(dateTime)` returns the nanosecond of `dateTime`, from 0 to 999.

See [`Temporal.PlainDateTime.prototype.nanosecond`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/nanosecond) on MDN.
*/
@get
external nanosecond: t => int = "nanosecond"

/**
`dayOfWeek(dateTime)` returns the day of the week of `dateTime`, from 1
(Monday) to 7 (Sunday) in the ISO 8601 calendar.

See [`Temporal.PlainDateTime.prototype.dayOfWeek`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/dayOfWeek) on MDN.
*/
@get
external dayOfWeek: t => int = "dayOfWeek"

/**
`dayOfYear(dateTime)` returns the day of the year of `dateTime`, numbered
from 1.

See [`Temporal.PlainDateTime.prototype.dayOfYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/dayOfYear) on MDN.
*/
@get
external dayOfYear: t => int = "dayOfYear"

/**
`weekOfYear(dateTime)` returns the week of the year of `dateTime`, or `None`
when its calendar has no defined weeks.

See [`Temporal.PlainDateTime.prototype.weekOfYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/weekOfYear) on MDN.
*/
@get
external weekOfYear: t => option<int> = "weekOfYear"

/**
`yearOfWeek(dateTime)` returns the year that `weekOfYear(dateTime)` belongs
to, or `None` when its calendar has no defined weeks.

See [`Temporal.PlainDateTime.prototype.yearOfWeek`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/yearOfWeek) on MDN.
*/
@get
external yearOfWeek: t => option<int> = "yearOfWeek"

/**
`daysInWeek(dateTime)` returns the number of days in the week of `dateTime`.

See [`Temporal.PlainDateTime.prototype.daysInWeek`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/daysInWeek) on MDN.
*/
@get
external daysInWeek: t => int = "daysInWeek"

/**
`daysInMonth(dateTime)` returns the number of days in the month of
`dateTime`.

See [`Temporal.PlainDateTime.prototype.daysInMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/daysInMonth) on MDN.
*/
@get
external daysInMonth: t => int = "daysInMonth"

/**
`daysInYear(dateTime)` returns the number of days in the year of `dateTime`.

See [`Temporal.PlainDateTime.prototype.daysInYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/daysInYear) on MDN.
*/
@get
external daysInYear: t => int = "daysInYear"

/**
`monthsInYear(dateTime)` returns the number of months in the year of
`dateTime`.

See [`Temporal.PlainDateTime.prototype.monthsInYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/monthsInYear) on MDN.
*/
@get
external monthsInYear: t => int = "monthsInYear"

/**
`inLeapYear(dateTime)` returns whether the year of `dateTime` is a leap year.

See [`Temporal.PlainDateTime.prototype.inLeapYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/inLeapYear) on MDN.
*/
@get
external inLeapYear: t => bool = "inLeapYear"

/**
`with(dateTime, fields, ~options=?)` returns a copy of `dateTime` with
`fields` replaced.

See [`Temporal.PlainDateTime.prototype.with`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/with) on MDN.

## Examples

```rescript
Temporal.PlainDateTime.fromStringOrThrow("2024-02-29T09:30")
->Temporal.PlainDateTime.with({hour: 12})
->Temporal.PlainDateTime.toString == "2024-02-29T12:30:00"
```
*/
@send
external with: (
  t,
  Stdlib_Temporal_Common.dateTimeUpdates,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "with"

/**
`withPlainTime(dateTime, ~time=?)` returns `dateTime` with its time replaced
by `time`, which defaults to midnight.

See [`Temporal.PlainDateTime.prototype.withPlainTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/withPlainTime) on MDN.

## Examples

```rescript
Temporal.PlainDateTime.fromStringOrThrow("2024-02-29T09:30")
->Temporal.PlainDateTime.withPlainTime
->Temporal.PlainDateTime.toString == "2024-02-29T00:00:00"
```
*/
@send
external withPlainTime: (t, ~time: Stdlib_Temporal_Common.plainTime=?) => t = "withPlainTime"

/**
`withCalendar(dateTime, calendar)` returns the same date-time in `calendar`.

See [`Temporal.PlainDateTime.prototype.withCalendar`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/withCalendar) on MDN.
*/
@send
external withCalendar: (t, Stdlib_Temporal_Common.calendar) => t = "withCalendar"

/**
`add(dateTime, duration, ~options=?)` moves `dateTime` forward by `duration`.

See [`Temporal.PlainDateTime.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/add) on MDN.
*/
@send
external add: (
  t,
  Stdlib_Temporal_Common.duration,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "add"

/**
`addFields(dateTime, fields, ~options=?)` moves `dateTime` forward by the
duration in `fields`.

See [`Temporal.PlainDateTime.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/add) on MDN.

## Examples

```rescript
Temporal.PlainDateTime.fromStringOrThrow("2024-02-29T23:00")
->Temporal.PlainDateTime.addFields({hours: 2.})
->Temporal.PlainDateTime.toString == "2024-03-01T01:00:00"
```
*/
@send
external addFields: (
  t,
  Stdlib_Temporal_Common.durationFields,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "add"

/**
`subtract(dateTime, duration, ~options=?)` moves `dateTime` backward by
`duration`.

See [`Temporal.PlainDateTime.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/subtract) on MDN.
*/
@send
external subtract: (
  t,
  Stdlib_Temporal_Common.duration,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "subtract"

/**
`subtractFields(dateTime, fields, ~options=?)` moves `dateTime` backward by
the duration in `fields`.

See [`Temporal.PlainDateTime.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/subtract) on MDN.
*/
@send
external subtractFields: (
  t,
  Stdlib_Temporal_Common.durationFields,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "subtract"

/**
`until(dateTime, other, ~options=?)` returns the duration from `dateTime` to
`other`. The largest unit defaults to `#day`.

See [`Temporal.PlainDateTime.prototype.until`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/until) on MDN.

## Examples

```rescript
Temporal.PlainDateTime.fromStringOrThrow("2024-02-29T09:00")
->Temporal.PlainDateTime.until(Temporal.PlainDateTime.fromStringOrThrow("2024-03-01T10:30"))
->Temporal.Duration.toString == "P1DT1H30M"
```
*/
@send
external until: (
  t,
  t,
  ~options: Stdlib_Temporal_Common.differenceOptions<Stdlib_Temporal_Common.dateTimeUnit>=?,
) => Stdlib_Temporal_Common.duration = "until"

/**
`since(dateTime, other, ~options=?)` returns the duration from `other` to
`dateTime`. The largest unit defaults to `#day`.

See [`Temporal.PlainDateTime.prototype.since`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/since) on MDN.
*/
@send
external since: (
  t,
  t,
  ~options: Stdlib_Temporal_Common.differenceOptions<Stdlib_Temporal_Common.dateTimeUnit>=?,
) => Stdlib_Temporal_Common.duration = "since"

/**
`round(dateTime, options)` rounds `dateTime` to `options.smallestUnit`.

See [`Temporal.PlainDateTime.prototype.round`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/round) on MDN.

## Examples

```rescript
Temporal.PlainDateTime.fromStringOrThrow("2024-02-29T13:00")
->Temporal.PlainDateTime.round({smallestUnit: #day})
->Temporal.PlainDateTime.toString == "2024-03-01T00:00:00"
```
*/
@send
external round: (
  t,
  Stdlib_Temporal_Common.roundingOptions<[#day | Stdlib_Temporal_Common.timeUnit]>,
) => t = "round"

/**
`equals(a, b)` returns whether two date-times have the same date, time and
calendar.

See [`Temporal.PlainDateTime.prototype.equals`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/equals) on MDN.
*/
@send
external equals: (t, t) => bool = "equals"

/**
`toZonedDateTime(dateTime, timeZone, ~options=?)` returns `dateTime` in
`timeZone`. A wall-clock time that is skipped or repeated in `timeZone` is
resolved by `options.disambiguation`.

See [`Temporal.PlainDateTime.prototype.toZonedDateTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/toZonedDateTime) on MDN.

## Examples

```rescript
Temporal.PlainDateTime.fromStringOrThrow("2024-02-29T09:30")
->Temporal.PlainDateTime.toZonedDateTime("Asia/Seoul")
->Temporal.ZonedDateTime.toString == "2024-02-29T09:30:00+09:00[Asia/Seoul]"
```
*/
@send
external toZonedDateTime: (
  t,
  string,
  ~options: toZonedDateTimeOptions=?,
) => Stdlib_Temporal_Common.zonedDateTime = "toZonedDateTime"

/**
`toPlainDate(dateTime)` returns the date of `dateTime`.

See [`Temporal.PlainDateTime.prototype.toPlainDate`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/toPlainDate) on MDN.
*/
@send
external toPlainDate: t => Stdlib_Temporal_Common.plainDate = "toPlainDate"

/**
`toPlainTime(dateTime)` returns the time of `dateTime`.

See [`Temporal.PlainDateTime.prototype.toPlainTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/toPlainTime) on MDN.

## Examples

```rescript
Temporal.PlainDateTime.fromStringOrThrow("2024-02-29T09:30")
->Temporal.PlainDateTime.toPlainTime
->Temporal.PlainTime.toString == "09:30:00"
```
*/
@send
external toPlainTime: t => Stdlib_Temporal_Common.plainTime = "toPlainTime"

/**
`toString(dateTime, ~options=?)` returns `dateTime` as an ISO 8601 string.

See [`Temporal.PlainDateTime.prototype.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/toString) on MDN.

## Examples

```rescript
Temporal.PlainDateTime.fromStringOrThrow("2024-02-29T09:30")->Temporal.PlainDateTime.toString(
  ~options={smallestUnit: #minute},
) == "2024-02-29T09:30"
```
*/
@send
external toString: (t, ~options: Stdlib_Temporal_Common.dateTimeToStringOptions=?) => string =
  "toString"

/**
`toJSON(dateTime)` returns `dateTime` as an ISO 8601 string.

See [`Temporal.PlainDateTime.prototype.toJSON`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/toJSON) on MDN.
*/
@send
external toJSON: t => string = "toJSON"

/**
`toLocaleString(dateTime, ~locales=?, ~options=?)` returns `dateTime`
formatted for `locales`.

See [`Temporal.PlainDateTime.prototype.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainDateTime/toLocaleString) on MDN.
*/
@send
external toLocaleString: (
  t,
  ~locales: array<string>=?,
  ~options: Stdlib_Intl_DateTimeFormat.options=?,
) => string = "toLocaleString"

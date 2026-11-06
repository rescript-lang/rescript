/***
Bindings to JavaScript's `Temporal.ZonedDateTime`, an exact point in time
together with the time zone and calendar used to read it as a date and
wall-clock time.

Compare date-times with `Temporal.ZonedDateTime.equals` and
`Temporal.ZonedDateTime.compare`. ReScript's `==`, `compare`, `<` and `>` treat
any two date-times as equal.

See [`Temporal.ZonedDateTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime) on MDN.
*/

type t = Stdlib_Temporal_Common.zonedDateTime

type fields = {
  ...Stdlib_Temporal_Common.dateTimeFields,
  timeZone: string,
  offset?: string,
}

/**
The fields `with` can replace. The time zone and calendar can't be replaced,
and at least one field must be given.
*/
type updates = {
  ...Stdlib_Temporal_Common.dateTimeUpdates,
  offset?: string,
}

/**
How a UTC offset in the input that conflicts with the time zone is handled:
`#use` keeps the exact time given by the offset, `#ignore` keeps the
wall-clock time, `#prefer` uses the offset when it is valid for the time zone,
and `#reject` throws a `RangeError`. Parsing and `fromFields` default to
`#reject`, and `with` defaults to `#prefer`.
*/
type offsetResolution = [#use | #ignore | #prefer | #reject]

type fromOptions = {
  overflow?: Stdlib_Temporal_Common.overflow,
  disambiguation?: Stdlib_Temporal_Common.disambiguation,
  offset?: offsetResolution,
}

type toStringOptions = {
  ...Stdlib_Temporal_Common.dateTimeToStringOptions,
  offset?: [#auto | #never],
  timeZoneName?: [#auto | #never | #critical],
}

type transitionDirection = [#next | #previous]

/**
`fromEpochNanoseconds(nanoseconds, ~timeZone, ~calendar=?)` creates a
date-time from nanoseconds since the UNIX epoch, read in `timeZone`.

See [`Temporal.ZonedDateTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/ZonedDateTime) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromEpochNanoseconds(
  0n,
  ~timeZone="UTC",
)->Temporal.ZonedDateTime.toString == "1970-01-01T00:00:00+00:00[UTC]"
```
*/
@new
external fromEpochNanoseconds: (
  bigint,
  ~timeZone: string,
  ~calendar: Stdlib_Temporal_Common.calendar=?,
) => t = "Temporal.ZonedDateTime"

/**
`fromStringOrThrow(string, ~options=?)` parses an ISO 8601 date-time with a
time zone annotation, like `"2024-02-29T09:30:00+09:00[Asia/Seoul]"`.

Throws a `RangeError` when `string` is not a valid zoned date-time.

See [`Temporal.ZonedDateTime.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/from) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromStringOrThrow(
  "2024-02-29T09:30:00+09:00[Asia/Seoul]",
)->Temporal.ZonedDateTime.timeZoneId == "Asia/Seoul"
```
*/
@val
external fromStringOrThrow: (string, ~options: fromOptions=?) => t = "Temporal.ZonedDateTime.from"

/**
`fromString(string, ~options=?)` parses an ISO 8601 date-time with a time zone
annotation like `fromStringOrThrow`, but returns `None` instead of throwing
when `string` is not a valid zoned date-time.

## Examples

```rescript
Temporal.ZonedDateTime.fromString("2024-02-29T09:30:00+09:00[Asia/Seoul]")->Option.map(
  Temporal.ZonedDateTime.timeZoneId,
) == Some("Asia/Seoul")
Temporal.ZonedDateTime.fromString("2024-02-29T09:30:00+09:00") == None
```
*/
let fromString = (value: string, ~options=?) =>
  try Some(fromStringOrThrow(value, ~options?)) catch {
  | JsExn(error) =>
    switch Stdlib_JsExn.name(error) {
    | Some("RangeError") => None
    | _ => Stdlib_JsExn.throw(error)
    }
  }

/**
`fromFields(fields, ~options=?)` creates a date-time from its fields.

See [`Temporal.ZonedDateTime.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/from) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromFields({
  timeZone: "Asia/Seoul",
  year: 2024,
  month: 2,
  day: 29,
  hour: 9,
})->Temporal.ZonedDateTime.toString == "2024-02-29T09:00:00+09:00[Asia/Seoul]"
```
*/
@val
external fromFields: (fields, ~options: fromOptions=?) => t = "Temporal.ZonedDateTime.from"

/**
`compare(a, b)` compares the exact times of two date-times, ignoring their
time zones and calendars.

See [`Temporal.ZonedDateTime.compare`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/compare) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.compare(
  Temporal.ZonedDateTime.fromStringOrThrow("2024-02-29T09:00:00+09:00[Asia/Seoul]"),
  Temporal.ZonedDateTime.fromStringOrThrow("2024-02-29T00:00:00+00:00[UTC]"),
) == Ordering.equal
```
*/
@val
external compare: (t, t) => Stdlib_Ordering.t = "Temporal.ZonedDateTime.compare"

/**
`calendarId(dateTime)` returns the calendar of `dateTime`.

See [`Temporal.ZonedDateTime.prototype.calendarId`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/calendarId) on MDN.
*/
@get
external calendarId: t => Stdlib_Temporal_Common.calendar = "calendarId"

/**
`timeZoneId(dateTime)` returns the time zone of `dateTime`.

See [`Temporal.ZonedDateTime.prototype.timeZoneId`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/timeZoneId) on MDN.
*/
@get
external timeZoneId: t => string = "timeZoneId"

/**
`era(dateTime)` returns the era of `dateTime`, or `None` when its calendar has
no eras.

See [`Temporal.ZonedDateTime.prototype.era`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/era) on MDN.
*/
@get
external era: t => option<string> = "era"

/**
`eraYear(dateTime)` returns the year of `dateTime` within its era, or `None`
when its calendar has no eras.

See [`Temporal.ZonedDateTime.prototype.eraYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/eraYear) on MDN.
*/
@get
external eraYear: t => option<int> = "eraYear"

/**
`year(dateTime)` returns the year of `dateTime`.

See [`Temporal.ZonedDateTime.prototype.year`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/year) on MDN.
*/
@get
external year: t => int = "year"

/**
`month(dateTime)` returns the month of `dateTime`, numbered from 1.

See [`Temporal.ZonedDateTime.prototype.month`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/month) on MDN.
*/
@get
external month: t => int = "month"

/**
`monthCode(dateTime)` returns the calendar-specific code of the month of
`dateTime`, like `"M02"`.

See [`Temporal.ZonedDateTime.prototype.monthCode`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/monthCode) on MDN.
*/
@get
external monthCode: t => string = "monthCode"

/**
`day(dateTime)` returns the day of the month of `dateTime`.

See [`Temporal.ZonedDateTime.prototype.day`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/day) on MDN.
*/
@get
external day: t => int = "day"

/**
`hour(dateTime)` returns the hour of `dateTime`, from 0 to 23.

See [`Temporal.ZonedDateTime.prototype.hour`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/hour) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromStringOrThrow(
  "2024-02-29T00:30:00+00:00[UTC]",
)
->Temporal.ZonedDateTime.withTimeZone("Asia/Seoul")
->Temporal.ZonedDateTime.hour == 9
```
*/
@get
external hour: t => int = "hour"

/**
`minute(dateTime)` returns the minute of `dateTime`, from 0 to 59.

See [`Temporal.ZonedDateTime.prototype.minute`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/minute) on MDN.
*/
@get
external minute: t => int = "minute"

/**
`second(dateTime)` returns the second of `dateTime`, from 0 to 59.

See [`Temporal.ZonedDateTime.prototype.second`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/second) on MDN.
*/
@get
external second: t => int = "second"

/**
`millisecond(dateTime)` returns the millisecond of `dateTime`, from 0 to 999.

See [`Temporal.ZonedDateTime.prototype.millisecond`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/millisecond) on MDN.
*/
@get
external millisecond: t => int = "millisecond"

/**
`microsecond(dateTime)` returns the microsecond of `dateTime`, from 0 to 999.

See [`Temporal.ZonedDateTime.prototype.microsecond`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/microsecond) on MDN.
*/
@get
external microsecond: t => int = "microsecond"

/**
`nanosecond(dateTime)` returns the nanosecond of `dateTime`, from 0 to 999.

See [`Temporal.ZonedDateTime.prototype.nanosecond`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/nanosecond) on MDN.
*/
@get
external nanosecond: t => int = "nanosecond"

/**
`epochMilliseconds(dateTime)` returns the milliseconds since the UNIX epoch,
rounded towards the beginning of time.

See [`Temporal.ZonedDateTime.prototype.epochMilliseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/epochMilliseconds) on MDN.
*/
@get
external epochMilliseconds: t => float = "epochMilliseconds"

/**
`epochNanoseconds(dateTime)` returns the nanoseconds since the UNIX epoch.

See [`Temporal.ZonedDateTime.prototype.epochNanoseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/epochNanoseconds) on MDN.
*/
@get
external epochNanoseconds: t => bigint = "epochNanoseconds"

/**
`dayOfWeek(dateTime)` returns the day of the week of `dateTime`, from 1
(Monday) to 7 (Sunday) in the ISO 8601 calendar.

See [`Temporal.ZonedDateTime.prototype.dayOfWeek`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/dayOfWeek) on MDN.
*/
@get
external dayOfWeek: t => int = "dayOfWeek"

/**
`dayOfYear(dateTime)` returns the day of the year of `dateTime`, numbered
from 1.

See [`Temporal.ZonedDateTime.prototype.dayOfYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/dayOfYear) on MDN.
*/
@get
external dayOfYear: t => int = "dayOfYear"

/**
`weekOfYear(dateTime)` returns the week of the year of `dateTime`, or `None`
when its calendar has no defined weeks.

See [`Temporal.ZonedDateTime.prototype.weekOfYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/weekOfYear) on MDN.
*/
@get
external weekOfYear: t => option<int> = "weekOfYear"

/**
`yearOfWeek(dateTime)` returns the year that `weekOfYear(dateTime)` belongs
to, or `None` when its calendar has no defined weeks.

See [`Temporal.ZonedDateTime.prototype.yearOfWeek`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/yearOfWeek) on MDN.
*/
@get
external yearOfWeek: t => option<int> = "yearOfWeek"

/**
`hoursInDay(dateTime)` returns the length of the day of `dateTime` in its time
zone, which differs from 24 on days with an offset change.

See [`Temporal.ZonedDateTime.prototype.hoursInDay`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/hoursInDay) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromStringOrThrow(
  "2024-03-10T12:00:00-04:00[America/New_York]",
)->Temporal.ZonedDateTime.hoursInDay == 23.
```
*/
@get
external hoursInDay: t => float = "hoursInDay"

/**
`daysInWeek(dateTime)` returns the number of days in the week of `dateTime`.

See [`Temporal.ZonedDateTime.prototype.daysInWeek`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/daysInWeek) on MDN.
*/
@get
external daysInWeek: t => int = "daysInWeek"

/**
`daysInMonth(dateTime)` returns the number of days in the month of
`dateTime`.

See [`Temporal.ZonedDateTime.prototype.daysInMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/daysInMonth) on MDN.
*/
@get
external daysInMonth: t => int = "daysInMonth"

/**
`daysInYear(dateTime)` returns the number of days in the year of `dateTime`.

See [`Temporal.ZonedDateTime.prototype.daysInYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/daysInYear) on MDN.
*/
@get
external daysInYear: t => int = "daysInYear"

/**
`monthsInYear(dateTime)` returns the number of months in the year of
`dateTime`.

See [`Temporal.ZonedDateTime.prototype.monthsInYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/monthsInYear) on MDN.
*/
@get
external monthsInYear: t => int = "monthsInYear"

/**
`inLeapYear(dateTime)` returns whether the year of `dateTime` is a leap year.

See [`Temporal.ZonedDateTime.prototype.inLeapYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/inLeapYear) on MDN.
*/
@get
external inLeapYear: t => bool = "inLeapYear"

/**
`offsetNanoseconds(dateTime)` returns the UTC offset of `dateTime` in
nanoseconds.

See [`Temporal.ZonedDateTime.prototype.offsetNanoseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/offsetNanoseconds) on MDN.
*/
@get
external offsetNanoseconds: t => float = "offsetNanoseconds"

/**
`offset(dateTime)` returns the UTC offset of `dateTime`, like `"+09:00"`.

See [`Temporal.ZonedDateTime.prototype.offset`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/offset) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromStringOrThrow(
  "2024-02-29T09:30:00+09:00[Asia/Seoul]",
)->Temporal.ZonedDateTime.offset == "+09:00"
```
*/
@get
external offset: t => string = "offset"

/**
`with(dateTime, fields, ~options=?)` returns a copy of `dateTime` with `fields`
replaced, keeping its time zone and calendar.

See [`Temporal.ZonedDateTime.prototype.with`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/with) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromStringOrThrow("2024-02-29T09:30:00+09:00[Asia/Seoul]")
->Temporal.ZonedDateTime.with({hour: 12})
->Temporal.ZonedDateTime.toString == "2024-02-29T12:30:00+09:00[Asia/Seoul]"
```
*/
@send
external with: (t, updates, ~options: fromOptions=?) => t = "with"

/**
`withPlainTime(dateTime, ~time=?)` returns `dateTime` with its wall-clock time
replaced by `time`, which defaults to the start of the day.

See [`Temporal.ZonedDateTime.prototype.withPlainTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/withPlainTime) on MDN.
*/
@send
external withPlainTime: (t, ~time: Stdlib_Temporal_Common.plainTime=?) => t = "withPlainTime"

/**
`withTimeZone(dateTime, timeZone)` returns the same exact time read in
`timeZone`.

See [`Temporal.ZonedDateTime.prototype.withTimeZone`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/withTimeZone) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromStringOrThrow("2024-02-29T09:30:00+09:00[Asia/Seoul]")
->Temporal.ZonedDateTime.withTimeZone("UTC")
->Temporal.ZonedDateTime.toString == "2024-02-29T00:30:00+00:00[UTC]"
```
*/
@send
external withTimeZone: (t, string) => t = "withTimeZone"

/**
`withCalendar(dateTime, calendar)` returns the same exact time in `calendar`.

See [`Temporal.ZonedDateTime.prototype.withCalendar`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/withCalendar) on MDN.
*/
@send
external withCalendar: (t, Stdlib_Temporal_Common.calendar) => t = "withCalendar"

/**
`add(dateTime, duration, ~options=?)` moves `dateTime` forward by `duration`.
Calendar units keep the wall-clock time across offset changes, while time
units add exact time.

See [`Temporal.ZonedDateTime.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/add) on MDN.
*/
@send
external add: (
  t,
  Stdlib_Temporal_Common.duration,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "add"

/**
`addFields(dateTime, fields, ~options=?)` moves `dateTime` forward by the
duration in `fields`. Calendar units keep the wall-clock time across offset
changes, while time units add exact time.

See [`Temporal.ZonedDateTime.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/add) on MDN.

## Examples

```rescript
let beforeDst = Temporal.ZonedDateTime.fromStringOrThrow(
  "2024-03-09T12:00:00-05:00[America/New_York]",
)
beforeDst->Temporal.ZonedDateTime.addFields({days: 1.})->Temporal.ZonedDateTime.toString ==
  "2024-03-10T12:00:00-04:00[America/New_York]"
beforeDst->Temporal.ZonedDateTime.addFields({hours: 24.})->Temporal.ZonedDateTime.toString ==
  "2024-03-10T13:00:00-04:00[America/New_York]"
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

See [`Temporal.ZonedDateTime.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/subtract) on MDN.
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

See [`Temporal.ZonedDateTime.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/subtract) on MDN.
*/
@send
external subtractFields: (
  t,
  Stdlib_Temporal_Common.durationFields,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "subtract"

/**
`until(dateTime, other, ~options=?)` returns the duration from `dateTime` to
`other`. The largest unit defaults to `#hour`.

See [`Temporal.ZonedDateTime.prototype.until`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/until) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromStringOrThrow("2024-02-29T09:00:00+09:00[Asia/Seoul]")
->Temporal.ZonedDateTime.until(
  Temporal.ZonedDateTime.fromStringOrThrow("2024-02-29T10:30:00+09:00[Asia/Seoul]"),
)
->Temporal.Duration.toString == "PT1H30M"
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
`dateTime`. The largest unit defaults to `#hour`.

See [`Temporal.ZonedDateTime.prototype.since`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/since) on MDN.
*/
@send
external since: (
  t,
  t,
  ~options: Stdlib_Temporal_Common.differenceOptions<Stdlib_Temporal_Common.dateTimeUnit>=?,
) => Stdlib_Temporal_Common.duration = "since"

/**
`round(dateTime, options)` rounds `dateTime` to `options.smallestUnit`.

See [`Temporal.ZonedDateTime.prototype.round`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/round) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromStringOrThrow("2024-02-29T09:30:00+09:00[Asia/Seoul]")
->Temporal.ZonedDateTime.round({smallestUnit: #hour})
->Temporal.ZonedDateTime.toString == "2024-02-29T10:00:00+09:00[Asia/Seoul]"
```
*/
@send
external round: (
  t,
  Stdlib_Temporal_Common.roundingOptions<[#day | Stdlib_Temporal_Common.timeUnit]>,
) => t = "round"

/**
`equals(a, b)` returns whether two date-times have the same exact time, time
zone and calendar.

See [`Temporal.ZonedDateTime.prototype.equals`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/equals) on MDN.
*/
@send
external equals: (t, t) => bool = "equals"

/**
`startOfDay(dateTime)` returns the first moment of the day of `dateTime` in
its time zone.

See [`Temporal.ZonedDateTime.prototype.startOfDay`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/startOfDay) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromStringOrThrow("2024-02-29T09:30:00+09:00[Asia/Seoul]")
->Temporal.ZonedDateTime.startOfDay
->Temporal.ZonedDateTime.toString == "2024-02-29T00:00:00+09:00[Asia/Seoul]"
```
*/
@send
external startOfDay: t => t = "startOfDay"

/**
`getTimeZoneTransition(dateTime, direction)` returns the next or previous
offset change in the time zone of `dateTime`, or `None` when there is none.

See [`Temporal.ZonedDateTime.prototype.getTimeZoneTransition`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/getTimeZoneTransition) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromStringOrThrow("2024-01-01T00:00:00-05:00[America/New_York]")
->Temporal.ZonedDateTime.getTimeZoneTransition(#next)
->Option.map(transition => transition->Temporal.ZonedDateTime.toString) ==
  Some("2024-03-10T03:00:00-04:00[America/New_York]")

Temporal.ZonedDateTime.fromStringOrThrow("2024-01-01T00:00:00+00:00[UTC]")
->Temporal.ZonedDateTime.getTimeZoneTransition(#next)
->Option.isNone == true
```
*/
@return(nullable) @send
external getTimeZoneTransition: (t, transitionDirection) => option<t> = "getTimeZoneTransition"

/**
`toInstant(dateTime)` returns the exact time of `dateTime`.

See [`Temporal.ZonedDateTime.prototype.toInstant`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/toInstant) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromStringOrThrow("2024-02-29T09:30:00+09:00[Asia/Seoul]")
->Temporal.ZonedDateTime.toInstant
->Temporal.Instant.toString == "2024-02-29T00:30:00Z"
```
*/
@send
external toInstant: t => Stdlib_Temporal_Common.instant = "toInstant"

/**
`toPlainDate(dateTime)` returns the date of `dateTime` in its time zone.

See [`Temporal.ZonedDateTime.prototype.toPlainDate`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/toPlainDate) on MDN.
*/
@send
external toPlainDate: t => Stdlib_Temporal_Common.plainDate = "toPlainDate"

/**
`toPlainTime(dateTime)` returns the wall-clock time of `dateTime` in its time
zone.

See [`Temporal.ZonedDateTime.prototype.toPlainTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/toPlainTime) on MDN.
*/
@send
external toPlainTime: t => Stdlib_Temporal_Common.plainTime = "toPlainTime"

/**
`toPlainDateTime(dateTime)` returns the date and wall-clock time of `dateTime`
in its time zone.

See [`Temporal.ZonedDateTime.prototype.toPlainDateTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/toPlainDateTime) on MDN.

## Examples

```rescript
Temporal.ZonedDateTime.fromStringOrThrow("2024-02-29T09:30:00+09:00[Asia/Seoul]")
->Temporal.ZonedDateTime.toPlainDateTime
->Temporal.PlainDateTime.toString == "2024-02-29T09:30:00"
```
*/
@send
external toPlainDateTime: t => Stdlib_Temporal_Common.plainDateTime = "toPlainDateTime"

/**
`toString(dateTime, ~options=?)` returns `dateTime` as an ISO 8601 string with
its UTC offset and time zone annotation.

See [`Temporal.ZonedDateTime.prototype.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/toString) on MDN.

## Examples

```rescript
let dateTime = Temporal.ZonedDateTime.fromStringOrThrow("2024-02-29T09:30:00+09:00[Asia/Seoul]")
dateTime->Temporal.ZonedDateTime.toString(~options={smallestUnit: #minute}) ==
  "2024-02-29T09:30+09:00[Asia/Seoul]"
dateTime->Temporal.ZonedDateTime.toString(~options={offset: #never, timeZoneName: #never}) ==
  "2024-02-29T09:30:00"
```
*/
@send
external toString: (t, ~options: toStringOptions=?) => string = "toString"

/**
`toJSON(dateTime)` returns `dateTime` as an ISO 8601 string with its UTC
offset and time zone annotation.

See [`Temporal.ZonedDateTime.prototype.toJSON`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/toJSON) on MDN.
*/
@send
external toJSON: t => string = "toJSON"

/**
`toLocaleString(dateTime, ~locales=?, ~options=?)` returns `dateTime`
formatted for `locales` in its own time zone. Passing `options.timeZone`
throws a `TypeError`.

See [`Temporal.ZonedDateTime.prototype.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/ZonedDateTime/toLocaleString) on MDN.
*/
@send
external toLocaleString: (
  t,
  ~locales: array<string>=?,
  ~options: Stdlib_Intl_DateTimeFormat.options=?,
) => string = "toLocaleString"

/***
Bindings to JavaScript's `Temporal.Duration`, a length of time such as
"1 hour and 30 minutes".

Compare the lengths of durations with `Temporal.Duration.compare`, or with
`Temporal.Duration.compareWithOptions` when they have years, months or weeks.
ReScript's `==`, `compare`, `<` and `>` treat any two durations as equal.

See [`Temporal.Duration`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration) on MDN.
*/

type t = Stdlib_Temporal_Common.duration

/**
The date that calendar units, like months, are measured from. Create one with
`relativeToPlainDate` or `relativeToZonedDateTime`.
*/
@notUndefined
type relativeTo

type relativeToOptions = {relativeTo?: relativeTo}

type roundingOptions = {
  largestUnit?: Stdlib_Temporal_Common.dateTimeUnit,
  smallestUnit?: Stdlib_Temporal_Common.dateTimeUnit,
  roundingIncrement?: int,
  roundingMode?: Stdlib_Temporal_Common.roundingMode,
  relativeTo?: relativeTo,
}

type totalOptions = {
  unit: Stdlib_Temporal_Common.dateTimeUnit,
  relativeTo?: relativeTo,
}

type toStringOptions = {
  fractionalSecondDigits?: Stdlib_Temporal_Common.fractionalSecondDigits,
  smallestUnit?: [#second | #millisecond | #microsecond | #nanosecond],
  roundingMode?: Stdlib_Temporal_Common.roundingMode,
}

/**
`make(~years=?, ~months=?, ~weeks=?, ~days=?, ~hours=?, ~minutes=?, ~seconds=?, ~milliseconds=?, ~microseconds=?, ~nanoseconds=?)`
creates a duration. Omitted fields default to `0.`, so `make()` is a zero
duration.

Throws a `RangeError` when a field is not an integer or the fields have mixed signs.

See [`Temporal.Duration`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/Duration) on MDN.

## Examples

```rescript
Temporal.Duration.make()->Temporal.Duration.toString == "PT0S"
Temporal.Duration.make(~hours=1., ~minutes=30.)->Temporal.Duration.toString == "PT1H30M"
```
*/
@new
external make: (
  ~years: float=?,
  ~months: float=?,
  ~weeks: float=?,
  ~days: float=?,
  ~hours: float=?,
  ~minutes: float=?,
  ~seconds: float=?,
  ~milliseconds: float=?,
  ~microseconds: float=?,
  ~nanoseconds: float=?,
) => t = "Temporal.Duration"

/**
`fromStringOrThrow(string)` parses an ISO 8601 duration, like `"PT1H30M"`.

Throws a `RangeError` when `string` is not a valid duration.

See [`Temporal.Duration.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/from) on MDN.

## Examples

```rescript
Temporal.Duration.fromStringOrThrow("PT1H30M")->Temporal.Duration.minutes == 30.
```
*/
@val
external fromStringOrThrow: string => t = "Temporal.Duration.from"

/**
`fromString(string)` parses an ISO 8601 duration like `fromStringOrThrow`, but
returns `None` instead of throwing when `string` is not a valid duration.

## Examples

```rescript
Temporal.Duration.fromString("PT1H30M")->Option.map(Temporal.Duration.minutes) == Some(30.)
Temporal.Duration.fromString("1 hour") == None
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
`fromFields(fields)` creates a duration from its fields.

Throws a `RangeError` when a field is not an integer or the fields have mixed signs.

See [`Temporal.Duration.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/from) on MDN.

## Examples

```rescript
Temporal.Duration.fromFields({hours: 1., minutes: 30.})->Temporal.Duration.toString == "PT1H30M"
```
*/
@val
external fromFields: Stdlib_Temporal_Common.durationFields => t = "Temporal.Duration.from"

/**
`compare(a, b)` compares the lengths of two durations.

Throws a `RangeError` when either duration has years, months or weeks. Use
`compareWithOptions` to compare those relative to a date. Days count as 24
hours, so `P1D` and `PT24H` compare as equal.

See [`Temporal.Duration.compare`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/compare) on MDN.

## Examples

```rescript
let ninetyMinutes = Temporal.Duration.fromFields({minutes: 90.})
let oneHour = Temporal.Duration.fromFields({hours: 1.})
Temporal.Duration.compare(ninetyMinutes, oneHour) == Ordering.greater
Temporal.Duration.compare(
  Temporal.Duration.fromFields({days: 1.}),
  Temporal.Duration.fromFields({hours: 24.}),
) == Ordering.equal
```
*/
@val
external compare: (t, t) => Stdlib_Ordering.t = "Temporal.Duration.compare"

/**
`compareWithOptions(a, b, options)` compares the lengths of two durations,
measuring calendar units from `options.relativeTo`.

See [`Temporal.Duration.compare`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/compare) on MDN.

## Examples

```rescript
let oneMonth = Temporal.Duration.fromFields({months: 1.})
let thirtyDays = Temporal.Duration.fromFields({days: 30.})
let february = Temporal.PlainDate.fromStringOrThrow("2024-02-01")

Temporal.Duration.compareWithOptions(
  oneMonth,
  thirtyDays,
  {relativeTo: Temporal.Duration.relativeToPlainDate(february)},
) == Ordering.less
```
*/
@val
external compareWithOptions: (t, t, relativeToOptions) => Stdlib_Ordering.t =
  "Temporal.Duration.compare"

/**
`relativeToPlainDate(date)` measures calendar units from `date`.
*/
external relativeToPlainDate: Stdlib_Temporal_Common.plainDate => relativeTo = "%identity"

/**
`relativeToZonedDateTime(dateTime)` measures calendar units from `dateTime`,
taking its time zone into account.
*/
external relativeToZonedDateTime: Stdlib_Temporal_Common.zonedDateTime => relativeTo = "%identity"

/**
`years(duration)` returns the number of years in `duration`.

See [`Temporal.Duration.prototype.years`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/years) on MDN.
*/
@get
external years: t => float = "years"

/**
`months(duration)` returns the number of months in `duration`.

See [`Temporal.Duration.prototype.months`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/months) on MDN.
*/
@get
external months: t => float = "months"

/**
`weeks(duration)` returns the number of weeks in `duration`.

See [`Temporal.Duration.prototype.weeks`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/weeks) on MDN.
*/
@get
external weeks: t => float = "weeks"

/**
`days(duration)` returns the number of days in `duration`.

See [`Temporal.Duration.prototype.days`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/days) on MDN.
*/
@get
external days: t => float = "days"

/**
`hours(duration)` returns the number of hours in `duration`.

See [`Temporal.Duration.prototype.hours`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/hours) on MDN.
*/
@get
external hours: t => float = "hours"

/**
`minutes(duration)` returns the number of minutes in `duration`.

See [`Temporal.Duration.prototype.minutes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/minutes) on MDN.
*/
@get
external minutes: t => float = "minutes"

/**
`seconds(duration)` returns the number of seconds in `duration`.

See [`Temporal.Duration.prototype.seconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/seconds) on MDN.
*/
@get
external seconds: t => float = "seconds"

/**
`milliseconds(duration)` returns the number of milliseconds in `duration`.

See [`Temporal.Duration.prototype.milliseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/milliseconds) on MDN.
*/
@get
external milliseconds: t => float = "milliseconds"

/**
`microseconds(duration)` returns the number of microseconds in `duration`.

See [`Temporal.Duration.prototype.microseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/microseconds) on MDN.
*/
@get
external microseconds: t => float = "microseconds"

/**
`nanoseconds(duration)` returns the number of nanoseconds in `duration`.

See [`Temporal.Duration.prototype.nanoseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/nanoseconds) on MDN.
*/
@get
external nanoseconds: t => float = "nanoseconds"

/**
`sign(duration)` returns `1` for a positive duration, `-1` for a negative one
and `0` for a zero duration.

See [`Temporal.Duration.prototype.sign`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/sign) on MDN.

## Examples

```rescript
Temporal.Duration.fromFields({hours: -1.})->Temporal.Duration.sign == -1
```
*/
@get
external sign: t => int = "sign"

/**
`blank(duration)` returns whether every field of `duration` is zero.

See [`Temporal.Duration.prototype.blank`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/blank) on MDN.

## Examples

```rescript
Temporal.Duration.make()->Temporal.Duration.blank == true
```
*/
@get
external blank: t => bool = "blank"

/**
`with(duration, fields)` returns a copy of `duration` with `fields` replaced.

See [`Temporal.Duration.prototype.with`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/with) on MDN.

## Examples

```rescript
Temporal.Duration.fromFields({hours: 1.})
->Temporal.Duration.with({minutes: 15.})
->Temporal.Duration.toString == "PT1H15M"
```
*/
@send
external with: (t, Stdlib_Temporal_Common.durationFields) => t = "with"

/**
`add(a, b)` returns the sum of two durations.

Throws a `RangeError` when either duration has years, months or weeks.

See [`Temporal.Duration.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/add) on MDN.

## Examples

```rescript
Temporal.Duration.fromFields({hours: 1.})
->Temporal.Duration.add(Temporal.Duration.fromFields({minutes: 90.}))
->Temporal.Duration.toString == "PT2H30M"
```
*/
@send
external add: (t, t) => t = "add"

/**
`addFields(duration, fields)` returns the sum of `duration` and the duration
in `fields`.

Throws a `RangeError` when `duration` or `fields` has years, months or weeks.

See [`Temporal.Duration.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/add) on MDN.

## Examples

```rescript
Temporal.Duration.fromFields({hours: 1.})
->Temporal.Duration.addFields({minutes: 90.})
->Temporal.Duration.toString == "PT2H30M"
```
*/
@send
external addFields: (t, Stdlib_Temporal_Common.durationFields) => t = "add"

/**
`subtract(a, b)` returns the difference of two durations.

Throws a `RangeError` when either duration has years, months or weeks.

See [`Temporal.Duration.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/subtract) on MDN.

## Examples

```rescript
Temporal.Duration.fromFields({hours: 2.})
->Temporal.Duration.subtract(Temporal.Duration.fromFields({minutes: 30.}))
->Temporal.Duration.toString == "PT1H30M"
```
*/
@send
external subtract: (t, t) => t = "subtract"

/**
`subtractFields(duration, fields)` returns the difference of `duration` and
the duration in `fields`.

Throws a `RangeError` when `duration` or `fields` has years, months or weeks.

See [`Temporal.Duration.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/subtract) on MDN.

## Examples

```rescript
Temporal.Duration.fromFields({hours: 2.})
->Temporal.Duration.subtractFields({minutes: 30.})
->Temporal.Duration.toString == "PT1H30M"
```
*/
@send
external subtractFields: (t, Stdlib_Temporal_Common.durationFields) => t = "subtract"

/**
`negated(duration)` returns `duration` with its sign reversed.

See [`Temporal.Duration.prototype.negated`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/negated) on MDN.

## Examples

```rescript
Temporal.Duration.fromFields({hours: 1.})->Temporal.Duration.negated->Temporal.Duration.toString ==
  "-PT1H"
```
*/
@send
external negated: t => t = "negated"

/**
`abs(duration)` returns `duration` with a positive sign.

See [`Temporal.Duration.prototype.abs`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/abs) on MDN.

## Examples

```rescript
Temporal.Duration.fromFields({hours: -1.})->Temporal.Duration.abs->Temporal.Duration.toString ==
  "PT1H"
```
*/
@send
external abs: t => t = "abs"

/**
`round(duration, options)` rounds `duration` and balances it between
`options.largestUnit` and `options.smallestUnit`.

Throws a `RangeError` when calendar units are involved and
`options.relativeTo` is missing.

See [`Temporal.Duration.prototype.round`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/round) on MDN.

## Examples

```rescript
Temporal.Duration.fromFields({minutes: 130.})
->Temporal.Duration.round({largestUnit: #hour})
->Temporal.Duration.toString == "PT2H10M"
```
*/
@send
external round: (t, roundingOptions) => t = "round"

/**
`total(duration, options)` returns the length of `duration` in `options.unit`.

Throws a `RangeError` when calendar units are involved and
`options.relativeTo` is missing.

See [`Temporal.Duration.prototype.total`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/total) on MDN.

## Examples

```rescript
Temporal.Duration.fromFields({hours: 1., minutes: 30.})->Temporal.Duration.total({unit: #minute}) ==
  90.
```
*/
@send
external total: (t, totalOptions) => float = "total"

/**
`toString(duration, ~options=?)` returns `duration` as an ISO 8601 string.

See [`Temporal.Duration.prototype.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/toString) on MDN.

## Examples

```rescript
let duration = Temporal.Duration.fromFields({seconds: 1., milliseconds: 500.})
duration->Temporal.Duration.toString == "PT1.5S"
duration->Temporal.Duration.toString(~options={fractionalSecondDigits: #3}) == "PT1.500S"
```
*/
@send
external toString: (t, ~options: toStringOptions=?) => string = "toString"

/**
`toJSON(duration)` returns `duration` as an ISO 8601 string, like `toString`.

See [`Temporal.Duration.prototype.toJSON`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/toJSON) on MDN.
*/
@send
external toJSON: t => string = "toJSON"

/**
`toLocaleString(duration, ~locales=?)` returns `duration` formatted for
`locales`.

See [`Temporal.Duration.prototype.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Duration/toLocaleString) on MDN.
*/
@send
external toLocaleString: (t, ~locales: array<string>=?) => string = "toLocaleString"

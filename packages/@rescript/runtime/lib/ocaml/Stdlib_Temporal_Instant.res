/***
Bindings to JavaScript's `Temporal.Instant`, an exact point in time with
nanosecond precision, independent of any time zone or calendar.

Compare instants with `Temporal.Instant.equals` and `Temporal.Instant.compare`.
ReScript's `==`, `compare`, `<` and `>` treat any two instants as equal.

See [`Temporal.Instant`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant) on MDN.
*/

type t = Stdlib_Temporal_Common.instant

type toStringOptions = {
  ...Stdlib_Temporal_Common.timeToStringOptions,
  timeZone?: string,
}

/**
`fromStringOrThrow(string)` parses an ISO 8601 date-time with a UTC offset,
like `"2024-01-01T00:00:00Z"`.

Throws a `RangeError` when `string` is not a valid instant.

See [`Temporal.Instant.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/from) on MDN.

## Examples

```rescript
Temporal.Instant.fromStringOrThrow("2024-01-01T00:00:00Z")->Temporal.Instant.epochMilliseconds ==
  1704067200000.
```
*/
@val
external fromStringOrThrow: string => t = "Temporal.Instant.from"

/**
`fromString(string)` parses an ISO 8601 date-time with a UTC offset like
`fromStringOrThrow`, but returns `None` instead of throwing when `string` is
not a valid instant.

## Examples

```rescript
Temporal.Instant.fromString("2024-01-01T00:00:00Z")->Option.map(Temporal.Instant.epochMilliseconds) ==
  Some(1704067200000.)
Temporal.Instant.fromString("2024-01-01") == None
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
`fromEpochMilliseconds(milliseconds)` creates an instant from milliseconds
since the UNIX epoch.

See [`Temporal.Instant.fromEpochMilliseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/fromEpochMilliseconds) on MDN.

## Examples

```rescript
Temporal.Instant.fromEpochMilliseconds(0.)->Temporal.Instant.toString == "1970-01-01T00:00:00Z"
```
*/
@val
external fromEpochMilliseconds: float => t = "Temporal.Instant.fromEpochMilliseconds"

/**
`fromEpochNanoseconds(nanoseconds)` creates an instant from nanoseconds since
the UNIX epoch.

See [`Temporal.Instant.fromEpochNanoseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/fromEpochNanoseconds) on MDN.

## Examples

```rescript
Temporal.Instant.fromEpochNanoseconds(1_000_000_000n)->Temporal.Instant.epochMilliseconds == 1000.
```
*/
@val
external fromEpochNanoseconds: bigint => t = "Temporal.Instant.fromEpochNanoseconds"

/**
`compare(a, b)` compares two instants chronologically.

See [`Temporal.Instant.compare`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/compare) on MDN.

## Examples

```rescript
Temporal.Instant.compare(
  Temporal.Instant.fromEpochMilliseconds(0.),
  Temporal.Instant.fromEpochMilliseconds(1.),
) == Ordering.less
```
*/
@val
external compare: (t, t) => Stdlib_Ordering.t = "Temporal.Instant.compare"

/**
`epochMilliseconds(instant)` returns the milliseconds since the UNIX epoch,
rounded towards the beginning of time.

See [`Temporal.Instant.prototype.epochMilliseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/epochMilliseconds) on MDN.
*/
@get
external epochMilliseconds: t => float = "epochMilliseconds"

/**
`epochNanoseconds(instant)` returns the nanoseconds since the UNIX epoch.

See [`Temporal.Instant.prototype.epochNanoseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/epochNanoseconds) on MDN.

## Examples

```rescript
Temporal.Instant.fromEpochMilliseconds(1.)->Temporal.Instant.epochNanoseconds == 1_000_000n
```
*/
@get
external epochNanoseconds: t => bigint = "epochNanoseconds"

/**
`add(instant, duration)` moves `instant` forward by `duration`.

Throws a `RangeError` when `duration` has years, months, weeks or days.

See [`Temporal.Instant.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/add) on MDN.

## Examples

```rescript
Temporal.Instant.fromEpochMilliseconds(0.)
->Temporal.Instant.add(Temporal.Duration.fromFields({hours: 1.}))
->Temporal.Instant.toString == "1970-01-01T01:00:00Z"
```
*/
@send
external add: (t, Stdlib_Temporal_Common.duration) => t = "add"

/**
`addFields(instant, fields)` moves `instant` forward by the duration in
`fields`.

Throws a `RangeError` when `fields` has years, months, weeks or days.

See [`Temporal.Instant.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/add) on MDN.

## Examples

```rescript
Temporal.Instant.fromEpochMilliseconds(0.)
->Temporal.Instant.addFields({minutes: 90.})
->Temporal.Instant.toString == "1970-01-01T01:30:00Z"
```
*/
@send
external addFields: (t, Stdlib_Temporal_Common.durationFields) => t = "add"

/**
`subtract(instant, duration)` moves `instant` backward by `duration`.

Throws a `RangeError` when `duration` has years, months, weeks or days.

See [`Temporal.Instant.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/subtract) on MDN.
*/
@send
external subtract: (t, Stdlib_Temporal_Common.duration) => t = "subtract"

/**
`subtractFields(instant, fields)` moves `instant` backward by the duration in
`fields`.

Throws a `RangeError` when `fields` has years, months, weeks or days.

See [`Temporal.Instant.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/subtract) on MDN.

## Examples

```rescript
Temporal.Instant.fromEpochMilliseconds(0.)
->Temporal.Instant.subtractFields({seconds: 1.})
->Temporal.Instant.toString == "1969-12-31T23:59:59Z"
```
*/
@send
external subtractFields: (t, Stdlib_Temporal_Common.durationFields) => t = "subtract"

/**
`until(instant, other, ~options=?)` returns the duration from `instant` to
`other`. The largest unit defaults to `#second`.

See [`Temporal.Instant.prototype.until`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/until) on MDN.

## Examples

```rescript
let start = Temporal.Instant.fromEpochMilliseconds(0.)
let end = Temporal.Instant.fromEpochMilliseconds(5_400_000.)
start->Temporal.Instant.until(end)->Temporal.Duration.toString == "PT5400S"
start->Temporal.Instant.until(end, ~options={largestUnit: #hour})->Temporal.Duration.toString ==
  "PT1H30M"
```
*/
@send
external until: (
  t,
  t,
  ~options: Stdlib_Temporal_Common.differenceOptions<Stdlib_Temporal_Common.timeUnit>=?,
) => Stdlib_Temporal_Common.duration = "until"

/**
`since(instant, other, ~options=?)` returns the duration from `other` to
`instant`. The largest unit defaults to `#second`.

See [`Temporal.Instant.prototype.since`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/since) on MDN.

## Examples

```rescript
let start = Temporal.Instant.fromEpochMilliseconds(0.)
let end = Temporal.Instant.fromEpochMilliseconds(5_400_000.)
end->Temporal.Instant.since(start, ~options={largestUnit: #minute})->Temporal.Duration.toString ==
  "PT90M"
```
*/
@send
external since: (
  t,
  t,
  ~options: Stdlib_Temporal_Common.differenceOptions<Stdlib_Temporal_Common.timeUnit>=?,
) => Stdlib_Temporal_Common.duration = "since"

/**
`round(instant, options)` rounds `instant` to `options.smallestUnit`.

See [`Temporal.Instant.prototype.round`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/round) on MDN.

## Examples

```rescript
Temporal.Instant.fromStringOrThrow("2024-01-01T10:20:30.5Z")
->Temporal.Instant.round({smallestUnit: #second})
->Temporal.Instant.toString == "2024-01-01T10:20:31Z"
```
*/
@send
external round: (t, Stdlib_Temporal_Common.roundingOptions<Stdlib_Temporal_Common.timeUnit>) => t =
  "round"

/**
`equals(a, b)` returns whether two instants are the same point in time.

See [`Temporal.Instant.prototype.equals`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/equals) on MDN.

## Examples

```rescript
Temporal.Instant.fromStringOrThrow("1970-01-01T09:00:00+09:00")->Temporal.Instant.equals(
  Temporal.Instant.fromEpochMilliseconds(0.),
) == true
```
*/
@send
external equals: (t, t) => bool = "equals"

/**
`toString(instant, ~options=?)` returns `instant` as an ISO 8601 string, in UTC
unless `options.timeZone` is given.

See [`Temporal.Instant.prototype.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/toString) on MDN.

## Examples

```rescript
let epoch = Temporal.Instant.fromEpochMilliseconds(0.)
epoch->Temporal.Instant.toString == "1970-01-01T00:00:00Z"
epoch->Temporal.Instant.toString(~options={timeZone: "Asia/Seoul"}) == "1970-01-01T09:00:00+09:00"
```
*/
@send
external toString: (t, ~options: toStringOptions=?) => string = "toString"

/**
`toJSON(instant)` returns `instant` as an ISO 8601 string in UTC.

See [`Temporal.Instant.prototype.toJSON`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/toJSON) on MDN.
*/
@send
external toJSON: t => string = "toJSON"

/**
`toLocaleString(instant, ~locales=?, ~options=?)` returns `instant` formatted
for `locales`.

See [`Temporal.Instant.prototype.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/toLocaleString) on MDN.
*/
@send
external toLocaleString: (
  t,
  ~locales: array<string>=?,
  ~options: Stdlib_Intl_DateTimeFormat.options=?,
) => string = "toLocaleString"

/**
`toZonedDateTimeISO(instant, timeZone)` returns the date and time of `instant`
in `timeZone`, using the ISO 8601 calendar.

See [`Temporal.Instant.prototype.toZonedDateTimeISO`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Instant/toZonedDateTimeISO) on MDN.

## Examples

```rescript
Temporal.Instant.fromEpochMilliseconds(0.)
->Temporal.Instant.toZonedDateTimeISO("UTC")
->Temporal.ZonedDateTime.toString == "1970-01-01T00:00:00+00:00[UTC]"
```
*/
@send
external toZonedDateTimeISO: (t, string) => Stdlib_Temporal_Common.zonedDateTime =
  "toZonedDateTimeISO"

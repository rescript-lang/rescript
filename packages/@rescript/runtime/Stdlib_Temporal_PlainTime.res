/***
Bindings to JavaScript's `Temporal.PlainTime`, a wall-clock time without a date
or time zone.

Compare times with `Temporal.PlainTime.equals` and `Temporal.PlainTime.compare`.
ReScript's `==`, `compare`, `<` and `>` treat any two times as equal.

See [`Temporal.PlainTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime) on MDN.
*/

type t = Stdlib_Temporal_Common.plainTime

/**
`make(~hour=?, ~minute=?, ~second=?, ~millisecond=?, ~microsecond=?, ~nanosecond=?)`
creates a time. Omitted fields default to `0`.

Throws a `RangeError` when a field is out of range.

See [`Temporal.PlainTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/PlainTime) on MDN.

## Examples

```rescript
Temporal.PlainTime.make(~hour=9, ~minute=30)->Temporal.PlainTime.toString == "09:30:00"
```
*/
@new
external make: (
  ~hour: int=?,
  ~minute: int=?,
  ~second: int=?,
  ~millisecond: int=?,
  ~microsecond: int=?,
  ~nanosecond: int=?,
) => t = "Temporal.PlainTime"

/**
`fromStringOrThrow(string)` parses an ISO 8601 time, like `"09:30"`.

Throws a `RangeError` when `string` is not a valid time.

See [`Temporal.PlainTime.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/from) on MDN.

## Examples

```rescript
Temporal.PlainTime.fromStringOrThrow("09:30")->Temporal.PlainTime.minute == 30
```
*/
@val
external fromStringOrThrow: string => t = "Temporal.PlainTime.from"

/**
`fromString(string)` parses an ISO 8601 time like `fromStringOrThrow`, but
returns `None` instead of throwing when `string` is not a valid time.

## Examples

```rescript
Temporal.PlainTime.fromString("09:30")->Option.map(Temporal.PlainTime.minute) == Some(30)
Temporal.PlainTime.fromString("25:00") == None
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
`fromFields(fields, ~options=?)` creates a time from its fields. Omitted fields
default to `0`, but at least one field must be given. Out-of-range fields are
clamped unless `options.overflow` is `#reject`.

See [`Temporal.PlainTime.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/from) on MDN.

## Examples

```rescript
Temporal.PlainTime.fromFields({hour: 25})->Temporal.PlainTime.toString == "23:00:00"
```
*/
@val
external fromFields: (
  Stdlib_Temporal_Common.timeFields,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "Temporal.PlainTime.from"

/**
`compare(a, b)` compares two times chronologically.

See [`Temporal.PlainTime.compare`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/compare) on MDN.

## Examples

```rescript
Temporal.PlainTime.compare(
  Temporal.PlainTime.make(~hour=9),
  Temporal.PlainTime.make(~hour=17),
) == Ordering.less
```
*/
@val
external compare: (t, t) => Stdlib_Ordering.t = "Temporal.PlainTime.compare"

/**
`hour(time)` returns the hour of `time`, from 0 to 23.

See [`Temporal.PlainTime.prototype.hour`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/hour) on MDN.
*/
@get
external hour: t => int = "hour"

/**
`minute(time)` returns the minute of `time`, from 0 to 59.

See [`Temporal.PlainTime.prototype.minute`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/minute) on MDN.
*/
@get
external minute: t => int = "minute"

/**
`second(time)` returns the second of `time`, from 0 to 59.

See [`Temporal.PlainTime.prototype.second`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/second) on MDN.
*/
@get
external second: t => int = "second"

/**
`millisecond(time)` returns the millisecond of `time`, from 0 to 999.

See [`Temporal.PlainTime.prototype.millisecond`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/millisecond) on MDN.
*/
@get
external millisecond: t => int = "millisecond"

/**
`microsecond(time)` returns the microsecond of `time`, from 0 to 999.

See [`Temporal.PlainTime.prototype.microsecond`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/microsecond) on MDN.
*/
@get
external microsecond: t => int = "microsecond"

/**
`nanosecond(time)` returns the nanosecond of `time`, from 0 to 999.

See [`Temporal.PlainTime.prototype.nanosecond`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/nanosecond) on MDN.
*/
@get
external nanosecond: t => int = "nanosecond"

/**
`add(time, duration)` moves `time` forward by `duration`, wrapping around
midnight.

See [`Temporal.PlainTime.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/add) on MDN.
*/
@send
external add: (t, Stdlib_Temporal_Common.duration) => t = "add"

/**
`addFields(time, fields)` moves `time` forward by the duration in `fields`,
wrapping around midnight.

See [`Temporal.PlainTime.prototype.add`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/add) on MDN.

## Examples

```rescript
Temporal.PlainTime.fromStringOrThrow("23:30")
->Temporal.PlainTime.addFields({hours: 1.})
->Temporal.PlainTime.toString == "00:30:00"
```
*/
@send
external addFields: (t, Stdlib_Temporal_Common.durationFields) => t = "add"

/**
`subtract(time, duration)` moves `time` backward by `duration`, wrapping around
midnight.

See [`Temporal.PlainTime.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/subtract) on MDN.
*/
@send
external subtract: (t, Stdlib_Temporal_Common.duration) => t = "subtract"

/**
`subtractFields(time, fields)` moves `time` backward by the duration in
`fields`, wrapping around midnight.

See [`Temporal.PlainTime.prototype.subtract`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/subtract) on MDN.

## Examples

```rescript
Temporal.PlainTime.fromStringOrThrow("00:30")
->Temporal.PlainTime.subtractFields({hours: 1.})
->Temporal.PlainTime.toString == "23:30:00"
```
*/
@send
external subtractFields: (t, Stdlib_Temporal_Common.durationFields) => t = "subtract"

/**
`with(time, fields, ~options=?)` returns a copy of `time` with `fields`
replaced.

See [`Temporal.PlainTime.prototype.with`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/with) on MDN.

## Examples

```rescript
Temporal.PlainTime.fromStringOrThrow("09:30")
->Temporal.PlainTime.with({minute: 0})
->Temporal.PlainTime.toString == "09:00:00"
```
*/
@send
external with: (
  t,
  Stdlib_Temporal_Common.timeFields,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "with"

/**
`until(time, other, ~options=?)` returns the duration from `time` to `other`.
The largest unit defaults to `#hour`.

See [`Temporal.PlainTime.prototype.until`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/until) on MDN.

## Examples

```rescript
Temporal.PlainTime.fromStringOrThrow("09:00")
->Temporal.PlainTime.until(Temporal.PlainTime.fromStringOrThrow("10:30"))
->Temporal.Duration.toString == "PT1H30M"
```
*/
@send
external until: (
  t,
  t,
  ~options: Stdlib_Temporal_Common.differenceOptions<Stdlib_Temporal_Common.timeUnit>=?,
) => Stdlib_Temporal_Common.duration = "until"

/**
`since(time, other, ~options=?)` returns the duration from `other` to `time`.
The largest unit defaults to `#hour`.

See [`Temporal.PlainTime.prototype.since`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/since) on MDN.

## Examples

```rescript
Temporal.PlainTime.fromStringOrThrow("10:30")
->Temporal.PlainTime.since(
  Temporal.PlainTime.fromStringOrThrow("09:00"),
  ~options={largestUnit: #minute},
)
->Temporal.Duration.toString == "PT90M"
```
*/
@send
external since: (
  t,
  t,
  ~options: Stdlib_Temporal_Common.differenceOptions<Stdlib_Temporal_Common.timeUnit>=?,
) => Stdlib_Temporal_Common.duration = "since"

/**
`round(time, options)` rounds `time` to `options.smallestUnit`.

See [`Temporal.PlainTime.prototype.round`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/round) on MDN.

## Examples

```rescript
Temporal.PlainTime.fromStringOrThrow("09:30")
->Temporal.PlainTime.round({smallestUnit: #hour})
->Temporal.PlainTime.toString == "10:00:00"
```
*/
@send
external round: (t, Stdlib_Temporal_Common.roundingOptions<Stdlib_Temporal_Common.timeUnit>) => t =
  "round"

/**
`equals(a, b)` returns whether two times are the same.

See [`Temporal.PlainTime.prototype.equals`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/equals) on MDN.

## Examples

```rescript
Temporal.PlainTime.fromStringOrThrow("09:30")->Temporal.PlainTime.equals(
  Temporal.PlainTime.make(~hour=9, ~minute=30),
) == true
```
*/
@send
external equals: (t, t) => bool = "equals"

/**
`toString(time, ~options=?)` returns `time` as an ISO 8601 string.

See [`Temporal.PlainTime.prototype.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/toString) on MDN.

## Examples

```rescript
let time = Temporal.PlainTime.fromStringOrThrow("09:30")
time->Temporal.PlainTime.toString == "09:30:00"
time->Temporal.PlainTime.toString(~options={smallestUnit: #minute}) == "09:30"
```
*/
@send
external toString: (t, ~options: Stdlib_Temporal_Common.timeToStringOptions=?) => string =
  "toString"

/**
`toJSON(time)` returns `time` as an ISO 8601 string.

See [`Temporal.PlainTime.prototype.toJSON`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/toJSON) on MDN.
*/
@send
external toJSON: t => string = "toJSON"

/**
`toLocaleString(time, ~locales=?, ~options=?)` returns `time` formatted for
`locales`. Date options, like `year` or `dateStyle`, throw a `TypeError`.

See [`Temporal.PlainTime.prototype.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainTime/toLocaleString) on MDN.
*/
@send
external toLocaleString: (
  t,
  ~locales: array<string>=?,
  ~options: Stdlib_Intl_DateTimeFormat.options=?,
) => string = "toLocaleString"

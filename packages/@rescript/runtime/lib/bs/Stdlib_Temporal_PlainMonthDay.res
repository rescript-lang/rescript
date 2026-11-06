/***
Bindings to JavaScript's `Temporal.PlainMonthDay`, a day of a month without a
year, like a birthday.

Compare month-days with `Temporal.PlainMonthDay.equals`. ReScript's `==`,
`compare`, `<` and `>` treat any two month-days as equal.

See [`Temporal.PlainMonthDay`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay) on MDN.
*/

type t = Stdlib_Temporal_Common.plainMonthDay

/**
The year to combine with a month and day. In a calendar with eras, like
`#gregory`, `era` and `eraYear` can replace `year`.
*/
type toPlainDateOptions = {
  year?: int,
  era?: string,
  eraYear?: int,
}

/**
`make(~month, ~day, ~calendar=?, ~referenceYear=?)` creates a month and day
from ISO 8601 month and day numbers. Months are numbered from 1.
`referenceYear` is the ISO year used to identify the day in non-ISO
calendars.

Throws a `RangeError` when a field is out of range.

See [`Temporal.PlainMonthDay`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay/PlainMonthDay) on MDN.

## Examples

```rescript
Temporal.PlainMonthDay.make(~month=12, ~day=25)->Temporal.PlainMonthDay.toString == "12-25"
```
*/
@new
external make: (
  ~month: int,
  ~day: int,
  ~calendar: Stdlib_Temporal_Common.calendar=?,
  ~referenceYear: int=?,
) => t = "Temporal.PlainMonthDay"

/**
`fromStringOrThrow(string)` parses an ISO 8601 month and day, like `"12-25"`.

Throws a `RangeError` when `string` is not a valid month and day.

See [`Temporal.PlainMonthDay.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay/from) on MDN.

## Examples

```rescript
Temporal.PlainMonthDay.fromStringOrThrow("12-25")->Temporal.PlainMonthDay.day == 25
```
*/
@val
external fromStringOrThrow: string => t = "Temporal.PlainMonthDay.from"

/**
`fromString(string)` parses an ISO 8601 month and day like
`fromStringOrThrow`, but returns `None` instead of throwing when `string` is
not a valid month and day.

## Examples

```rescript
Temporal.PlainMonthDay.fromString("12-25")->Option.map(Temporal.PlainMonthDay.day) == Some(25)
Temporal.PlainMonthDay.fromString("13-01") == None
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
`fromFields(fields, ~options=?)` creates a month and day from its fields.
Out-of-range fields are clamped unless `options.overflow` is `#reject`.

See [`Temporal.PlainMonthDay.from`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay/from) on MDN.

## Examples

```rescript
Temporal.PlainMonthDay.fromFields({month: 2, day: 29})->Temporal.PlainMonthDay.toString == "02-29"
```
*/
@val
external fromFields: (
  Stdlib_Temporal_Common.dateFields,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "Temporal.PlainMonthDay.from"

/**
`calendarId(monthDay)` returns the calendar of `monthDay`.

See [`Temporal.PlainMonthDay.prototype.calendarId`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay/calendarId) on MDN.
*/
@get
external calendarId: t => Stdlib_Temporal_Common.calendar = "calendarId"

/**
`monthCode(monthDay)` returns the calendar-specific code of the month of
`monthDay`, like `"M12"`.

See [`Temporal.PlainMonthDay.prototype.monthCode`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay/monthCode) on MDN.

## Examples

```rescript
Temporal.PlainMonthDay.fromStringOrThrow("12-25")->Temporal.PlainMonthDay.monthCode == "M12"
```
*/
@get
external monthCode: t => string = "monthCode"

/**
`day(monthDay)` returns the day of the month of `monthDay`.

See [`Temporal.PlainMonthDay.prototype.day`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay/day) on MDN.
*/
@get
external day: t => int = "day"

/**
`with(monthDay, fields, ~options=?)` returns a copy of `monthDay` with `fields`
replaced.

See [`Temporal.PlainMonthDay.prototype.with`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay/with) on MDN.

## Examples

```rescript
Temporal.PlainMonthDay.fromStringOrThrow("12-25")
->Temporal.PlainMonthDay.with({day: 24})
->Temporal.PlainMonthDay.toString == "12-24"
```
*/
@send
external with: (
  t,
  Stdlib_Temporal_Common.dateUpdates,
  ~options: Stdlib_Temporal_Common.overflowOptions=?,
) => t = "with"

/**
`equals(a, b)` returns whether two month-days are the same in the same
calendar.

See [`Temporal.PlainMonthDay.prototype.equals`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay/equals) on MDN.
*/
@send
external equals: (t, t) => bool = "equals"

/**
`toPlainDate(monthDay, options)` returns the date of `monthDay` in
`options.year`. A day that doesn't exist in that year is clamped.

See [`Temporal.PlainMonthDay.prototype.toPlainDate`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay/toPlainDate) on MDN.

## Examples

```rescript
Temporal.PlainMonthDay.fromStringOrThrow("02-29")
->Temporal.PlainMonthDay.toPlainDate({year: 2023})
->Temporal.PlainDate.toString == "2023-02-28"
```
*/
@send
external toPlainDate: (t, toPlainDateOptions) => Stdlib_Temporal_Common.plainDate = "toPlainDate"

/**
`toString(monthDay, ~options=?)` returns `monthDay` as an ISO 8601 string.

See [`Temporal.PlainMonthDay.prototype.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay/toString) on MDN.
*/
@send
external toString: (t, ~options: Stdlib_Temporal_Common.dateToStringOptions=?) => string =
  "toString"

/**
`toJSON(monthDay)` returns `monthDay` as an ISO 8601 string.

See [`Temporal.PlainMonthDay.prototype.toJSON`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay/toJSON) on MDN.
*/
@send
external toJSON: t => string = "toJSON"

/**
`toLocaleString(monthDay, ~locales=?, ~options=?)` returns `monthDay`
formatted for `locales`. The formatter's calendar must match the calendar of
`monthDay`, or a `RangeError` is thrown. Most locales default to `#gregory`,
so a month-day in the ISO 8601 calendar needs `options.calendar` set to
`#iso8601`.

See [`Temporal.PlainMonthDay.prototype.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/PlainMonthDay/toLocaleString) on MDN.

## Examples

```rescript
Temporal.PlainMonthDay.fromStringOrThrow("12-25")->Temporal.PlainMonthDay.toLocaleString(
  ~locales=["en-US"],
  ~options={calendar: #iso8601},
) == "12-25"
```
*/
@send
external toLocaleString: (
  t,
  ~locales: array<string>=?,
  ~options: Stdlib_Intl_DateTimeFormat.options=?,
) => string = "toLocaleString"

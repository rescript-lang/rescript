/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/***
Provide bindings to JS date. (See
[`Date`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
on MDN.) JavaScript stores dates as the number of milliseconds since the UNIX
*epoch*, midnight 1 January 1970, UTC.
*/

@deprecated({
  reason: "Use `Date.t` instead.",
  migrate: %replace.type(: Date.t),
})
type t = Stdlib_Date.t

/**
Returns the primitive value of this date, equivalent to `getTime()`. (See
[`Date.valueOf`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/valueOf)
on MDN.)

## Examples

```rescript
Js.Date.valueOf(exampleDate) == 123456654321.0
```
*/
@deprecated({
  reason: "Use `Date.getTime` instead.",
  migrate: Date.getTime(),
})
@send
external valueOf: t => float = "valueOf"

/**
Returns a date representing the current time. See [`Date()`
Constructor](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date)
on MDN.

## Examples

```rescript
let now = Js.Date.make()
```
*/
@deprecated({
  reason: "Use `Date.make` instead.",
  migrate: Date.make(),
})
@new
external make: unit => t = "Date"

/**
Returns a date representing the given argument, which is a number of
milliseconds since the epoch. See [`Date()`
Constructor](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date)
on MDN.

## Examples

```rescript
Js.Date.fromFloat(123456654321.0) == exampleDate
```
*/
@deprecated({
  reason: "Use `Date.fromTime` instead.",
  migrate: Date.fromTime(),
})
@new
external fromFloat: float => t = "Date"

/**
Returns a `Js.Date.t` represented by the given string. The string can be in
“IETF-compliant RFC 2822 timestamps, and also strings in a version of ISO8601.”
Returns `NaN` if given an invalid date string. According to the [`Date()`
Constructor](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date)
documentation on MDN, its use is discouraged.

## Examples

```rescript
Js.Date.fromString("Thu, 29 Nov 1973 21:30:54.321 GMT") == exampleDate
Js.Date.fromString("1973-11-29T21:30:54.321Z00:00") == exampleDate
Js.Date.fromString("Thor, 32 Lok -19 60:70:80 XYZ") // returns NaN
```
*/
@deprecated({
  reason: "Use `Date.fromString` instead.",
  migrate: Date.fromString(),
})
@new
external fromString: string => t = "Date"

/**
Returns a date representing midnight of the first day of the given month and
year in the current time zone. Fractional parts of arguments are ignored. See
[`Date()`
Constructor](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date)
on MDN.

## Examples

```rescript
let november1 = Js.Date.makeWithYM(~year=2020.0, ~month=10.0, ())
```
*/
@deprecated({
  reason: "Use `Date.makeWithYM` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.makeWithYM(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
  ),
})
@new
external makeWithYM: (~year: float, ~month: float, unit) => t = "Date"

/**
Returns a date representing midnight of the given date of the given month and
year in the current time zone. Fractional parts of arguments are ignored. See
[`Date()`
Constructor](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date)
on MDN.
*/
@deprecated({
  reason: "Use `Date.makeWithYMD` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.makeWithYMD(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
    ~day=Float.toInt(%insert.labelledArgument("date")),
  ),
})
@new
external makeWithYMD: (~year: float, ~month: float, ~date: float, unit) => t = "Date"

/**
Returns a date representing the given date of the given month and year, at zero
minutes and zero seconds past the given `hours`, in the current time zone.
Fractional parts of arguments are ignored. See [`Date()`
Constructor](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date)
on MDN. Fractional parts of the arguments are ignored.
*/
@deprecated({
  reason: "Use `Date.makeWithYMDH` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.makeWithYMDH(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
    ~day=Float.toInt(%insert.labelledArgument("date")),
    ~hours=Float.toInt(%insert.labelledArgument("hours")),
  ),
})
@new
external makeWithYMDH: (~year: float, ~month: float, ~date: float, ~hours: float, unit) => t =
  "Date"

/**
Returns a date representing the given date of the given month and year, at zero
seconds past the given time in hours and minutes in the current time zone.
Fractional parts of arguments are ignored. See [`Date()`
Constructor](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date)
on MDN.
*/
@deprecated({
  reason: "Use `Date.makeWithYMDHM` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.makeWithYMDHM(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
    ~day=Float.toInt(%insert.labelledArgument("date")),
    ~hours=Float.toInt(%insert.labelledArgument("hours")),
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
  ),
})
@new
external makeWithYMDHM: (
  ~year: float,
  ~month: float,
  ~date: float,
  ~hours: float,
  ~minutes: float,
  unit,
) => t = "Date"

/**
Returns a date representing the given date of the given month and year, at the
given time in hours, minutes, and seconds in the current time zone. Fractional
parts of arguments are ignored. See [`Date()`
Constructor](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/Date)
on MDN.

## Examples

```rescript
Js.Date.makeWithYMDHMS(
  ~year=1973.0,
  ~month=11.0,
  ~date=29.0,
  ~hours=21.0,
  ~minutes=30.0,
  ~seconds=54.321,
  (),
) == exampleDate
```
*/
@deprecated({
  reason: "Use `Date.makeWithYMDHMS` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.makeWithYMDHMS(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
    ~day=Float.toInt(%insert.labelledArgument("date")),
    ~hours=Float.toInt(%insert.labelledArgument("hours")),
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
    ~seconds=Float.toInt(%insert.labelledArgument("seconds")),
  ),
})
@new
external makeWithYMDHMS: (
  ~year: float,
  ~month: float,
  ~date: float,
  ~hours: float,
  ~minutes: float,
  ~seconds: float,
  unit,
) => t = "Date"

/**
Returns a float representing the number of milliseconds past the epoch for
midnight of the first day of the given month and year in UTC. Fractional parts
of arguments are ignored. See
[`Date.UTC`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/UTC)
on MDN.

## Examples

```rescript
let november1 = Js.Date.utcWithYM(~year=2020.0, ~month=10.0, ())
```
*/
@deprecated({
  reason: "Use `Date.UTC.makeWithYM` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.UTC.makeWithYM(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
  ),
})
@val("Date.UTC")
external utcWithYM: (~year: float, ~month: float, unit) => float = ""

/**
Returns a float representing the number of milliseconds past the epoch for
midnight of the given date of the given month and year in UTC. Fractional parts
of arguments are ignored. See
[`Date.UTC`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/UTC)
on MDN.
*/
@deprecated({
  reason: "Use `Date.UTC.makeWithYMD` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.UTC.makeWithYMD(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
    ~day=Float.toInt(%insert.labelledArgument("date")),
  ),
})
@val("Date.UTC")
external utcWithYMD: (~year: float, ~month: float, ~date: float, unit) => float = ""

/**
Returns a float representing the number of milliseconds past the epoch for
midnight of the given date of the given month and year, at zero minutes and
seconds past the given hours in UTC. Fractional parts of arguments are ignored.
See
[`Date.UTC`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/UTC)
on MDN.
*/
@deprecated({
  reason: "Use `Date.UTC.makeWithYMDH` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.UTC.makeWithYMDH(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
    ~day=Float.toInt(%insert.labelledArgument("date")),
    ~hours=Float.toInt(%insert.labelledArgument("hours")),
  ),
})
@val("Date.UTC")
external utcWithYMDH: (~year: float, ~month: float, ~date: float, ~hours: float, unit) => float = ""

/**
Returns a float representing the number of milliseconds past the epoch for
midnight of the given date of the given month and year, at zero seconds past
the given number of minutes past the given hours in UTC. Fractional parts of
arguments are ignored. See
[`Date.UTC`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/UTC)
on MDN.
*/
@deprecated({
  reason: "Use `Date.UTC.makeWithYMDHM` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.UTC.makeWithYMDHM(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
    ~day=Float.toInt(%insert.labelledArgument("date")),
    ~hours=Float.toInt(%insert.labelledArgument("hours")),
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
  ),
})
@val("Date.UTC")
external utcWithYMDHM: (
  ~year: float,
  ~month: float,
  ~date: float,
  ~hours: float,
  ~minutes: float,
  unit,
) => float = ""

/**
Returns a float representing the number of milliseconds past the epoch for
midnight of the given date of the given month and year, at the given time in
hours, minutes and seconds in UTC. Fractional parts of arguments are ignored.

See
[`Date.UTC`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/UTC)
on MDN.
*/
@deprecated({
  reason: "Use `Date.UTC.makeWithYMDHMS` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.UTC.makeWithYMDHMS(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
    ~day=Float.toInt(%insert.labelledArgument("date")),
    ~hours=Float.toInt(%insert.labelledArgument("hours")),
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
    ~seconds=Float.toInt(%insert.labelledArgument("seconds")),
  ),
})
@val("Date.UTC")
external utcWithYMDHMS: (
  ~year: float,
  ~month: float,
  ~date: float,
  ~hours: float,
  ~minutes: float,
  ~seconds: float,
  unit,
) => float = ""

/** Returns the current time as number of milliseconds since Unix epoch. */
@deprecated({
  reason: "Use `Date.now` instead.",
  migrate: Date.now(),
})
@val("Date.now")
external now: unit => float = ""

@new
@deprecated({
  reason: "Use `Date.fromString` instead.",
  migrate: Date.fromString(),
})
external parse: string => t = "Date"

/**
Returns a float with the number of milliseconds past the epoch represented by
the given string. The string can be in “IETF-compliant RFC 2822 timestamps, and
also strings in a version of ISO8601.” Returns `NaN` if given an invalid date
string. According to the
[`Date.parse`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse)
documentation on MDN, its use is discouraged. Returns `NaN` if passed invalid
date string.
*/
@deprecated({
  reason: "Use `Date.fromString` + `Date.getTime` instead.",
  migrate: Date.getTime(Date.fromString(%insert.unlabelledArgument(0))),
})
@val("parse")
@scope("Date")
external parseAsFloat: string => float = ""

/**
Returns the day of the month for its argument. The argument is evaluated in the
current time zone. See
[`Date.getDate`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getDate)
on MDN.

## Examples

```rescript
Js.Date.getDate(exampleDate) == 29.0
```
*/
@deprecated({
  reason: "Use `Date.getDate` instead.",
  migrate: Date.getDate(),
})
@send
external getDate: t => float = "getDate"

/**
Returns the day of the week (0.0-6.0) for its argument, where 0.0 represents
Sunday. The argument is evaluated in the current time zone.  See
[`Date.getDay`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getDay)
on MDN.

## Examples

```rescript
Js.Date.getDay(exampleDate) == 4.0
```
*/
@deprecated({
  reason: "Use `Date.getDay` instead.",
  migrate: Date.getDay(),
})
@send
external getDay: t => float = "getDay"

/**
Returns the full year (as opposed to the range 0-99) for its argument. The
argument is evaluated in the current time zone. See
[`Date.getFullYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getFullYear)
on MDN.

## Examples

```rescript
Js.Date.getFullYear(exampleDate) == 1973.0
```
*/
@deprecated({
  reason: "Use `Date.getFullYear` instead.",
  migrate: Date.getFullYear(),
})
@send
external getFullYear: t => float = "getFullYear"

/**
Returns the hours for its argument, evaluated in the current time zone. See
[`Date.getHours`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getHours)
on MDN.

## Examples

```rescript
Js.Date.getHours(exampleDate) == 22.0 // Vienna is in GMT+01:00
```
*/
@deprecated({
  reason: "Use `Date.getHours` instead.",
  migrate: Date.getHours(),
})
@send
external getHours: t => float = "getHours"

/**
Returns the number of milliseconds for its argument, evaluated in the current
time zone. See
[`Date.getMilliseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getMilliseconds)
on MDN.

## Examples

```rescript
Js.Date.getMilliseconds(exampleDate) == 321.0
```
*/
@deprecated({
  reason: "Use `Date.getMilliseconds` instead.",
  migrate: Date.getMilliseconds(),
})
@send
external getMilliseconds: t => float = "getMilliseconds"

/**
Returns the number of minutes for its argument, evaluated in the current time
zone. See
[`Date.getMinutes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getMinutes)
on MDN.

## Examples

```rescript
Js.Date.getMinutes(exampleDate) == 30.0
```
*/
@deprecated({
  reason: "Use `Date.getMinutes` instead.",
  migrate: Date.getMinutes(),
})
@send
external getMinutes: t => float = "getMinutes"

/**
Returns the month (0.0-11.0) for its argument, evaluated in the current time
zone. January is month zero.  See
[`Date.getMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getMonth)
on MDN.

## Examples

```rescript
Js.Date.getMonth(exampleDate) == 10.0
```
*/
@deprecated({
  reason: "Use `Date.getMonth` instead.",
  migrate: Date.getMonth(),
})
@send
external getMonth: t => float = "getMonth"

/**
Returns the seconds for its argument, evaluated in the current time zone. See
[`Date.getSeconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getSeconds)
on MDN.

## Examples

```rescript
Js.Date.getSeconds(exampleDate) == 54.0
```
*/
@deprecated({
  reason: "Use `Date.getSeconds` instead.",
  migrate: Date.getSeconds(),
})
@send
external getSeconds: t => float = "getSeconds"

/**
Returns the number of milliseconds since Unix epoch, evaluated in UTC.  See
[`Date.getTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getTime)
on MDN.

## Examples

```rescript
Js.Date.getTime(exampleDate) == 123456654321.0
```
*/
@deprecated({
  reason: "Use `Date.getTime` instead.",
  migrate: Date.getTime(),
})
@send
external getTime: t => float = "getTime"

/**
Returns the time zone offset in minutes from the current time zone to UTC. See
[`Date.getTimezoneOffset`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getTimezoneOffset)
on MDN.

## Examples

```rescript
Js.Date.getTimezoneOffset(exampleDate) == -60.0
```
*/
@deprecated({
  reason: "Use `Date.getTimezoneOffset` instead.",
  migrate: Date.getTimezoneOffset(),
})
@send
external getTimezoneOffset: t => float = "getTimezoneOffset"

/**
Returns the day of the month of the argument, evaluated in UTC. See
[`Date.getUTCDate`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getUTCDate)
on MDN.

## Examples

```rescript
Js.Date.getUTCDate(exampleDate) == 29.0
```
*/
@deprecated({
  reason: "Use `Date.getUTCDate` instead.",
  migrate: Date.getUTCDate(),
})
@send
external getUTCDate: t => float = "getUTCDate"

/**
Returns the day of the week of the argument, evaluated in UTC. The range of the
return value is 0.0-6.0, where Sunday is zero. See
[`Date.getUTCDay`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getUTCDay)
on MDN.

## Examples

```rescript
Js.Date.getUTCDay(exampleDate) == 4.0
```
*/
@deprecated({
  reason: "Use `Date.getUTCDay` instead.",
  migrate: Date.getUTCDay(),
})
@send
external getUTCDay: t => float = "getUTCDay"

/**
Returns the full year (as opposed to the range 0-99) for its argument. The
argument is evaluated in UTC.  See
[`Date.getUTCFullYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getUTCFullYear)
on MDN.

## Examples

```rescript
Js.Date.getUTCFullYear(exampleDate) == 1973.0
```
*/
@deprecated({
  reason: "Use `Date.getUTCFullYear` instead.",
  migrate: Date.getUTCFullYear(),
})
@send
external getUTCFullYear: t => float = "getUTCFullYear"

/**
Returns the hours for its argument, evaluated in the current time zone. See
[`Date.getUTCHours`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getUTCHours)
on MDN.

## Examples

```rescript
Js.Date.getUTCHours(exampleDate) == 21.0
```
*/
@deprecated({
  reason: "Use `Date.getUTCHours` instead.",
  migrate: Date.getUTCHours(),
})
@send
external getUTCHours: t => float = "getUTCHours"

/**
Returns the number of milliseconds for its argument, evaluated in UTC. See
[`Date.getUTCMilliseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getUTCMilliseconds)
on MDN.

## Examples

```rescript
Js.Date.getUTCMilliseconds(exampleDate) == 321.0
```
*/
@deprecated({
  reason: "Use `Date.getUTCMilliseconds` instead.",
  migrate: Date.getUTCMilliseconds(),
})
@send
external getUTCMilliseconds: t => float = "getUTCMilliseconds"

/**
Returns the number of minutes for its argument, evaluated in UTC. See
[`Date.getUTCMinutes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getUTCMinutes)
on MDN.

## Examples

```rescript
Js.Date.getUTCMinutes(exampleDate) == 30.0
```
*/
@deprecated({
  reason: "Use `Date.getUTCMinutes` instead.",
  migrate: Date.getUTCMinutes(),
})
@send
external getUTCMinutes: t => float = "getUTCMinutes"

/**
Returns the month (0.0-11.0) for its argument, evaluated in UTC. January is
month zero. See
[`Date.getUTCMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getUTCMonth)
on MDN.

## Examples

```rescript
Js.Date.getUTCMonth(exampleDate) == 10.0
```
*/
@deprecated({
  reason: "Use `Date.getUTCMonth` instead.",
  migrate: Date.getUTCMonth(),
})
@send
external getUTCMonth: t => float = "getUTCMonth"

/**
Returns the seconds for its argument, evaluated in UTC. See
[`Date.getUTCSeconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getUTCSeconds)
on MDN.

## Examples

```rescript
Js.Date.getUTCSeconds(exampleDate) == 54.0
```
*/
@deprecated({
  reason: "Use `Date.getUTCSeconds` instead.",
  migrate: Date.getUTCSeconds(),
})
@send
external getUTCSeconds: t => float = "getUTCSeconds"

@send @deprecated({reason: "Use `getFullYear` instead.", migrate: Date.getFullYear()})
external getYear: t => float = "getYear"

/**
Sets the given `Date`’s day of month to the value in the second argument
according to the current time zone. Returns the number of milliseconds since
the epoch of the updated `Date`. *This function modifies the original `Date`.*
See
[`Date.setDate`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setDate)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let twoWeeksBefore = Js.Date.setDate(date1, 15.0)
date1 == Js.Date.fromString("1973-11-15T21:30:54.321Z00:00")
twoWeeksBefore == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setDate` instead.",
  migrate: Date.setDate(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setDate: (t, float) => float = "setDate"

/**
Sets the given `Date`’s year to the value in the second argument according to
the current time zone. Returns the number of milliseconds since the epoch of
the updated `Date`. *This function modifies the original `Date`.* See
[`Date.setFullYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setFullYear)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let nextYear = Js.Date.setFullYear(date1, 1974.0)
date1 == Js.Date.fromString("1974-11-15T21:30:54.321Z00:00")
nextYear == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setFullYear` instead.",
  migrate: Date.setFullYear(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setFullYear: (t, float) => float = "setFullYear"

/**
Sets the given `Date`’s year and month to the values in the labeled arguments
according to the current time zone. Returns the number of milliseconds since
the epoch of the updated `Date`. *This function modifies the original `Date`.*
See
[`Date.setFullYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setFullYear)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let future = Js.Date.setFullYearM(date1, ~year=1974.0, ~month=0.0, ())
date1 == Js.Date.fromString("1974-01-22T21:30:54.321Z00:00")
future == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setFullYearM` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setFullYearM(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
  ),
})
@send
external setFullYearM: (t, ~year: float, ~month: float, unit) => float = "setFullYear"

/**
Sets the given `Date`’s year, month, and day of month to the values in the
labeled arguments according to the current time zone. Returns the number of
milliseconds since the epoch of the updated `Date`. *This function modifies the
original `Date`.* See
[`Date.setFullYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setFullYear)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let future = Js.Date.setFullYearMD(date1, ~year=1974.0, ~month=0.0, ~date=7.0, ())
date1 == Js.Date.fromString("1974-01-07T21:30:54.321Z00:00")
future == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setFullYearMD` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setFullYearMD(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
    ~day=Float.toInt(%insert.labelledArgument("date")),
  ),
})
external setFullYearMD: (t, ~year: float, ~month: float, ~date: float, unit) => float =
  "setFullYear"

/**
Sets the given `Date`’s hours to the value in the second argument according to
the current time zone. Returns the number of milliseconds since the epoch of
the updated `Date`. *This function modifies the original `Date`.* See
[`Date.setHours`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setHours)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let nextHour = Js.Date.setHours(date1, 22.0)
date1 == Js.Date.fromString("1973-11-29T22:30:54.321Z00:00")
nextHour == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setHours` instead.",
  migrate: Date.setHours(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setHours: (t, float) => float = "setHours"

/**
Sets the given `Date`’s hours and minutes to the values in the labeled
arguments according to the current time zone. Returns the number of
milliseconds since the epoch of the updated `Date`. *This function modifies the
original `Date`.* See
[`Date.setHours`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setHours)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setHoursM(date1, ~hours=22.0, ~minutes=46.0, ())
date1 == Js.Date.fromString("1973-11-29T22:46:54.321Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setHoursM` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setHoursM(
    ~hours=Float.toInt(%insert.labelledArgument("hours")),
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
  ),
})
@send
external setHoursM: (t, ~hours: float, ~minutes: float, unit) => float = "setHours"

/**
Sets the given `Date`’s hours, minutes, and seconds to the values in the
labeled arguments according to the current time zone. Returns the number of
milliseconds since the epoch of the updated `Date`. *This function modifies the
original `Date`.* See
[`Date.setHours`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setHours)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setHoursMS(date1, ~hours=22.0, ~minutes=46.0, ~seconds=37.0, ())
date1 == Js.Date.fromString("1973-11-29T22:46:37.321Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setHoursMS` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setHoursMS(
    ~hours=Float.toInt(%insert.labelledArgument("hours")),
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
    ~seconds=Float.toInt(%insert.labelledArgument("seconds")),
  ),
})
external setHoursMS: (t, ~hours: float, ~minutes: float, ~seconds: float, unit) => float =
  "setHours"

/**
Sets the given `Date`’s hours, minutes, seconds, and milliseconds to the values
in the labeled arguments according to the current time zone. Returns the number
of milliseconds since the epoch of the updated `Date`. *This function modifies
the original `Date`.* See
[`Date.setHours`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setHours)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setHoursMSMs(
  date1,
  ~hours=22.0,
  ~minutes=46.0,
  ~seconds=37.0,
  ~milliseconds=494.0,
  (),
)
date1 == Js.Date.fromString("1973-11-29T22:46:37.494Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setHoursMSMs` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setHoursMSMs(
    ~hours=Float.toInt(%insert.labelledArgument("hours")),
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
    ~seconds=Float.toInt(%insert.labelledArgument("seconds")),
    ~milliseconds=Float.toInt(%insert.labelledArgument("milliseconds")),
  ),
})
external setHoursMSMs: (
  t,
  ~hours: float,
  ~minutes: float,
  ~seconds: float,
  ~milliseconds: float,
  unit,
) => float = "setHours"

/**
Sets the given `Date`’s milliseconds to the value in the second argument
according to the current time zone. Returns the number of milliseconds since
the epoch of the updated `Date`. *This function modifies the original `Date`.*
See
[`Date.setMilliseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setMilliseconds)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setMilliseconds(date1, 494.0)
date1 == Js.Date.fromString("1973-11-29T21:30:54.494Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setMilliseconds` instead.",
  migrate: Date.setMilliseconds(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setMilliseconds: (t, float) => float = "setMilliseconds"

/**
Sets the given `Date`’s minutes to the value in the second argument according
to the current time zone. Returns the number of milliseconds since the epoch of
the updated `Date`. *This function modifies the original `Date`.* See
[`Date.setMinutes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setMinutes)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setMinutes(date1, 34.0)
date1 == Js.Date.fromString("1973-11-29T21:34:54.494Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setMinutes` instead.",
  migrate: Date.setMinutes(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setMinutes: (t, float) => float = "setMinutes"

/**
Sets the given `Date`’s minutes and seconds to the values in the labeled
arguments according to the current time zone. Returns the number of
milliseconds since the epoch of the updated `Date`. *This function modifies the
original `Date`.* See
[`Date.setMinutes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setMinutes)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setMinutesS(date1, ~minutes=34.0, ~seconds=56.0, ())
date1 == Js.Date.fromString("1973-11-29T21:34:56.494Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setMinutesS` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setMinutesS(
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
    ~seconds=Float.toInt(%insert.labelledArgument("seconds")),
  ),
})
external setMinutesS: (t, ~minutes: float, ~seconds: float, unit) => float = "setMinutes"

/**
Sets the given `Date`’s minutes, seconds, and milliseconds to the values in the
labeled arguments according to the current time zone. Returns the number of
milliseconds since the epoch of the updated `Date`. *This function modifies the
original `Date`.* See
[`Date.setMinutes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setMinutes)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setMinutesSMs(date1, ~minutes=34.0, ~seconds=56.0, ~milliseconds=789.0, ())
date1 == Js.Date.fromString("1973-11-29T21:34:56.789Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setMinutesSMs` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setMinutesSMs(
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
    ~seconds=Float.toInt(%insert.labelledArgument("seconds")),
    ~milliseconds=Float.toInt(%insert.labelledArgument("milliseconds")),
  ),
})
external setMinutesSMs: (t, ~minutes: float, ~seconds: float, ~milliseconds: float, unit) => float =
  "setMinutes"

/**
Sets the given `Date`’s month to the value in the second argument according to
the current time zone. Returns the number of milliseconds since the epoch of
the updated `Date`. *This function modifies the original `Date`.* See
[`Date.setMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setMonth)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setMonth(date1, 11.0)
date1 == Js.Date.fromString("1973-12-29T21:34:56.789Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setMonth` instead.",
  migrate: Date.setMonth(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setMonth: (t, float) => float = "setMonth"

/**
Sets the given `Date`’s month and day of month to the values in the labeled
arguments according to the current time zone. Returns the number of
milliseconds since the epoch of the updated `Date`. *This function modifies the
original `Date`.* See
[`Date.setMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setMonth)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setMonthD(date1, ~month=11.0, ~date=8.0, ())
date1 == Js.Date.fromString("1973-12-08T21:34:56.789Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@deprecated("Use `Date.setMonth` then `Date.setDate`. No direct 1:1 migration available.") @send
external setMonthD: (t, ~month: float, ~date: float, unit) => float = "setMonth"

/**
Sets the given `Date`’s seconds to the value in the second argument according
to the current time zone. Returns the number of milliseconds since the epoch of
the updated `Date`. *This function modifies the original `Date`.* See
[`Date.setSeconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setSeconds)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setSeconds(date1, 56.0)
date1 == Js.Date.fromString("1973-12-29T21:30:56.321Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setSeconds` instead.",
  migrate: Date.setSeconds(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setSeconds: (t, float) => float = "setSeconds"

/**
Sets the given `Date`’s seconds and milliseconds to the values in the labeled
arguments according to the current time zone. Returns the number of
milliseconds since the epoch of the updated `Date`. *This function modifies the
original `Date`.* See
[`Date.setSeconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setSeconds)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setSecondsMs(date1, ~seconds=56.0, ~milliseconds=789.0, ())
date1 == Js.Date.fromString("1973-12-29T21:30:56.789Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setSecondsMs` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setSecondsMs(
    ~seconds=Float.toInt(%insert.labelledArgument("seconds")),
    ~milliseconds=Float.toInt(%insert.labelledArgument("milliseconds")),
  ),
})
external setSecondsMs: (t, ~seconds: float, ~milliseconds: float, unit) => float = "setSeconds"

/**
Sets the given `Date`’s value in terms of milliseconds since the epoch. Returns
the number of milliseconds since the epoch of the updated `Date`. *This
function modifies the original `Date`.* See
[`Date.setTime`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setTime)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setTime(date1, 198765432101.0)

date1 == Js.Date.fromString("1976-04-19T12:37:12.101Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@send @deprecated
external setTime: (t, float) => float = "setTime"

/**
Sets the given `Date`’s day of month to the value in the second argument
according to UTC. Returns the number of milliseconds since the epoch of the
updated `Date`. *This function modifies the original `Date`.* See
[`Date.setUTCDate`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCDate)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let twoWeeksBefore = Js.Date.setUTCDate(date1, 15.0)
date1 == Js.Date.fromString("1973-11-15T21:30:54.321Z00:00")
twoWeeksBefore == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setUTCDate` instead.",
  migrate: Date.setUTCDate(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setUTCDate: (t, float) => float = "setUTCDate"

/**
Sets the given `Date`’s year to the value in the second argument according to
UTC. Returns the number of milliseconds since the epoch of the updated `Date`.
*This function modifies the original `Date`.* See
[`Date.setUTCFullYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCFullYear)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let nextYear = Js.Date.setUTCFullYear(date1, 1974.0)
date1 == Js.Date.fromString("1974-11-15T21:30:54.321Z00:00")
nextYear == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setUTCFullYear` instead.",
  migrate: Date.setUTCFullYear(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setUTCFullYear: (t, float) => float = "setUTCFullYear"

/**
Sets the given `Date`’s year and month to the values in the labeled arguments
according to UTC. Returns the number of milliseconds since the epoch of the
updated `Date`. *This function modifies the original `Date`.* See
[`Date.setUTCFullYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCFullYear)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let future = Js.Date.setUTCFullYearM(date1, ~year=1974.0, ~month=0.0, ())
date1 == Js.Date.fromString("1974-01-22T21:30:54.321Z00:00")
future == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setUTCFullYearM` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setUTCFullYearM(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
  ),
})
external setUTCFullYearM: (t, ~year: float, ~month: float, unit) => float = "setUTCFullYear"

/**
Sets the given `Date`’s year, month, and day of month to the values in the
labeled arguments according to UTC. Returns the number of milliseconds since
the epoch of the updated `Date`. *This function modifies the original `Date`.*
See
[`Date.setUTCFullYear`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCFullYear)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let future = Js.Date.setUTCFullYearMD(date1, ~year=1974.0, ~month=0.0, ~date=7.0, ())
date1 == Js.Date.fromString("1974-01-07T21:30:54.321Z00:00")
future == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setUTCFullYearMD` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setUTCFullYearMD(
    ~year=Float.toInt(%insert.labelledArgument("year")),
    ~month=Float.toInt(%insert.labelledArgument("month")),
    ~day=Float.toInt(%insert.labelledArgument("date")),
  ),
})
external setUTCFullYearMD: (t, ~year: float, ~month: float, ~date: float, unit) => float =
  "setUTCFullYear"

/**
Sets the given `Date`’s hours to the value in the second argument according to
UTC. Returns the number of milliseconds since the epoch of the updated `Date`.
*This function modifies the original `Date`.* See
[`Date.setUTCHours`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCHours)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let nextHour = Js.Date.setUTCHours(date1, 22.0)
date1 == Js.Date.fromString("1973-11-29T22:30:54.321Z00:00")
nextHour == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setUTCHours` instead.",
  migrate: Date.setUTCHours(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setUTCHours: (t, float) => float = "setUTCHours"

/**
Sets the given `Date`’s hours and minutes to the values in the labeled
arguments according to UTC. Returns the number of milliseconds since the epoch
of the updated `Date`. *This function modifies the original `Date`.* See
[`Date.setUTCHours`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCHours)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setUTCHoursM(date1, ~hours=22.0, ~minutes=46.0, ())
date1 == Js.Date.fromString("1973-11-29T22:46:54.321Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setUTCHoursM` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setUTCHoursM(
    ~hours=Float.toInt(%insert.labelledArgument("hours")),
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
  ),
})
external setUTCHoursM: (t, ~hours: float, ~minutes: float, unit) => float = "setUTCHours"

/**
Sets the given `Date`’s hours, minutes, and seconds to the values in the
labeled arguments according to UTC. Returns the number of milliseconds since
the epoch of the updated `Date`. *This function modifies the original `Date`.*

See
[`Date.setUTCHours`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCHours)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setUTCHoursMS(date1, ~hours=22.0, ~minutes=46.0, ~seconds=37.0, ())
date1 == Js.Date.fromString("1973-11-29T22:46:37.321Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setUTCHoursMS` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setUTCHoursMS(
    ~hours=Float.toInt(%insert.labelledArgument("hours")),
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
    ~seconds=Float.toInt(%insert.labelledArgument("seconds")),
  ),
})
external setUTCHoursMS: (t, ~hours: float, ~minutes: float, ~seconds: float, unit) => float =
  "setUTCHours"

/**
Sets the given `Date`’s hours, minutes, seconds, and milliseconds to the values
in the labeled arguments according to UTC. Returns the number of milliseconds
since the epoch of the updated `Date`. *This function modifies the original
`Date`.* See
[`Date.setUTCHours`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCHours)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setUTCHoursMSMs(
  date1,
  ~hours=22.0,
  ~minutes=46.0,
  ~seconds=37.0,
  ~milliseconds=494.0,
  (),
)
date1 == Js.Date.fromString("1973-11-29T22:46:37.494Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setUTCHoursMSMs` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setUTCHoursMSMs(
    ~hours=Float.toInt(%insert.labelledArgument("hours")),
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
    ~seconds=Float.toInt(%insert.labelledArgument("seconds")),
    ~milliseconds=Float.toInt(%insert.labelledArgument("milliseconds")),
  ),
})
external setUTCHoursMSMs: (
  t,
  ~hours: float,
  ~minutes: float,
  ~seconds: float,
  ~milliseconds: float,
  unit,
) => float = "setUTCHours"

/**
Sets the given `Date`’s milliseconds to the value in the second argument
according to UTC. Returns the number of milliseconds since the epoch of the
updated `Date`. *This function modifies the original `Date`.* See
[`Date.setUTCMilliseconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCMilliseconds)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setUTCMilliseconds(date1, 494.0)
date1 == Js.Date.fromString("1973-11-29T21:30:54.494Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setUTCMilliseconds` instead.",
  migrate: Date.setUTCMilliseconds(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setUTCMilliseconds: (t, float) => float = "setUTCMilliseconds"

/**
Sets the given `Date`’s minutes to the value in the second argument according
to the current time zone. Returns the number of milliseconds since the epoch of
the updated `Date`. *This function modifies the original `Date`.* See
[`Date.setUTCMinutes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCMinutes)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setUTCMinutes(date1, 34.0)
date1 == Js.Date.fromString("1973-11-29T21:34:54.494Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setUTCMinutes` instead.",
  migrate: Date.setUTCMinutes(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setUTCMinutes: (t, float) => float = "setUTCMinutes"

/**
Sets the given `Date`’s minutes and seconds to the values in the labeled
arguments according to UTC. Returns the number of milliseconds since the epoch
of the updated `Date`. *This function modifies the original `Date`.* See
[`Date.setUTCMinutes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCMinutes)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setUTCMinutesS(date1, ~minutes=34.0, ~seconds=56.0, ())
date1 == Js.Date.fromString("1973-11-29T21:34:56.494Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setUTCMinutesS` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setUTCMinutesS(
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
    ~seconds=Float.toInt(%insert.labelledArgument("seconds")),
  ),
})
external setUTCMinutesS: (t, ~minutes: float, ~seconds: float, unit) => float = "setUTCMinutes"

/**
Sets the given `Date`’s minutes, seconds, and milliseconds to the values in the
labeled arguments according to UTC. Returns the number of milliseconds since
the epoch of the updated `Date`. *This function modifies the original `Date`.*
See
[`Date.setUTCMinutes`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCMinutes)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setUTCMinutesSMs(
  date1,
  ~minutes=34.0,
  ~seconds=56.0,
  ~milliseconds=789.0,
  (),
)
date1 == Js.Date.fromString("1973-11-29T21:34:56.789Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setUTCMinutesSMs` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setUTCMinutesSMs(
    ~minutes=Float.toInt(%insert.labelledArgument("minutes")),
    ~seconds=Float.toInt(%insert.labelledArgument("seconds")),
    ~milliseconds=Float.toInt(%insert.labelledArgument("milliseconds")),
  ),
})
external setUTCMinutesSMs: (
  t,
  ~minutes: float,
  ~seconds: float,
  ~milliseconds: float,
  unit,
) => float = "setUTCMinutes"

/**
Sets the given `Date`’s month to the value in the second argument according to
UTC. Returns the number of milliseconds since the epoch of the updated `Date`.
*This function modifies the original `Date`.* See
[`Date.setUTCMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCMonth)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setUTCMonth(date1, 11.0)
date1 == Js.Date.fromString("1973-12-29T21:34:56.789Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setUTCMonth` instead.",
  migrate: Date.setUTCMonth(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setUTCMonth: (t, float) => float = "setUTCMonth"

/**
Sets the given `Date`’s month and day of month to the values in the labeled
arguments according to UTC. Returns the number of milliseconds since the epoch
of the updated `Date`. *This function modifies the original `Date`.* See
[`Date.setUTCMonth`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCMonth)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setUTCMonthD(date1, ~month=11.0, ~date=8.0, ())
date1 == Js.Date.fromString("1973-12-08T21:34:56.789Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@deprecated("Use `Date.setUTCMonth` then `Date.setUTCDate`. No direct 1:1 migration available.")
@send
external setUTCMonthD: (t, ~month: float, ~date: float, unit) => float = "setUTCMonth"

/**
Sets the given `Date`’s seconds to the value in the second argument according
to UTC. Returns the number of milliseconds since the epoch of the updated
`Date`. *This function modifies the original `Date`.* See
[`Date.setUTCSeconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCSeconds)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setUTCSeconds(date1, 56.0)
date1 == Js.Date.fromString("1973-12-29T21:30:56.321Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@deprecated({
  reason: "Use `Date.setUTCSeconds` instead.",
  migrate: Date.setUTCSeconds(Float.toInt(%insert.unlabelledArgument(1))),
})
@send
external setUTCSeconds: (t, float) => float = "setUTCSeconds"

/**
Sets the given `Date`’s seconds and milliseconds to the values in the labeled
arguments according to UTC. Returns the number of milliseconds since the epoch
of the updated `Date`. *This function modifies the original `Date`.* See
[`Date.setUTCSeconds`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/setUTCSeconds)
on MDN.

## Examples

```rescript
let date1 = Js.Date.fromFloat(123456654321.0) // 29 November 1973 21:30:54.321 GMT
let futureTime = Js.Date.setUTCSecondsMs(date1, ~seconds=56.0, ~milliseconds=789.0, ())
date1 == Js.Date.fromString("1973-12-29T21:30:56.789Z00:00")
futureTime == Js.Date.getTime(date1)
```
*/
@send
@deprecated({
  reason: "Use `Date.setUTCSecondsMs` instead.",
  migrate: @apply.transforms(["dropUnitArgumentsInApply"])
  Date.setUTCSecondsMs(
    ~seconds=Float.toInt(%insert.labelledArgument("seconds")),
    ~milliseconds=Float.toInt(%insert.labelledArgument("milliseconds")),
  ),
})
external setUTCSecondsMs: (t, ~seconds: float, ~milliseconds: float, unit) => float =
  "setUTCSeconds"

/** Same as [`setTime()`](#settime). */
@deprecated @send
external setUTCTime: (t, float) => float = "setTime"

@send @deprecated("Use `setFullYear` instead") external setYear: (t, float) => float = "setYear"

/**
Returns the date (day of week, year, month, and day of month) portion of a
`Date` in English. See
[`Date.toDateString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toDateString)
on MDN.

## Examples

```rescript
Js.Date.toDateString(exampleDate) == "Thu Nov 29 1973"
```
*/
@deprecated({
  reason: "Use `Date.toDateString` instead.",
  migrate: Date.toDateString(),
})
@send
external toDateString: t => string = "toDateString"

@send
@deprecated({
  reason: "Use `Date.toUTCString` instead.",
  migrate: Date.toUTCString(),
})
external toGMTString: t => string = "toGMTString"

/**
Returns a simplified version of the ISO 8601 format for the date. See
[`Date.toISOString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toISOString)
on MDN.

## Examples

```rescript
Js.Date.toISOString(exampleDate) == "1973-11-29T21:30:54.321Z"
```
*/
@deprecated({
  reason: "Use `Date.toISOString` instead.",
  migrate: Date.toISOString(),
})
@send
external toISOString: t => string = "toISOString"

@deprecated({
  reason: "This method is unsafe. It will be changed to return option in a future release. Please use toJSONUnsafe instead.",
  migrate: Date.toJSON(),
})
@send
external toJSON: t => string = "toJSON"

/**
Returns a string representation of the given date. See
[`Date.toJSON`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toJSON)
on MDN.
*/
@deprecated({
  reason: "Use `Date.toJSON` instead.",
  migrate: Date.toJSON(),
})
@send
external toJSONUnsafe: t => string = "toJSON"

/**
Returns the year, month, and day for the given `Date` in the current locale
format. See
[`Date.toLocaleDateString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
on MDN.

## Examples

```rescript
Js.Date.toLocaleDateString(exampleDate) == "11/29/1973" // for en_US.utf8
Js.Date.toLocaleDateString(exampleDate) == "29.11.73" // for de_DE.utf8
```
*/
@deprecated({
  reason: "Use `Date.toLocaleDateString` instead.",
  migrate: Date.toLocaleDateString(),
})
@send
external toLocaleDateString: t => string = "toLocaleDateString"

/* TODO: has overloads with somewhat poor browser support */

/**
Returns the time and date for the given `Date` in the current locale format.
See
[`Date.toLocaleString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
on MDN.

## Examples

```rescript
Js.Date.toLocaleString(exampleDate) == "11/29/1973, 10:30:54 PM" // for en_US.utf8
Js.Date.toLocaleString(exampleDate) == "29.11.1973, 22:30:54" // for de_DE.utf8
```
*/
@deprecated({
  reason: "Use `Date.toLocaleString` instead.",
  migrate: Date.toLocaleString(),
})
@send
external toLocaleString: t => string = "toLocaleString"

/* TODO: has overloads with somewhat poor browser support */

/**
Returns the time of day for the given `Date` in the current locale format. See
[`Date.toLocaleTimeString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleTimeString)
on MDN.

## Examples

```rescript
Js.Date.toLocaleString(exampleDate) == "10:30:54 PM" // for en_US.utf8
Js.Date.toLocaleString(exampleDate) == "22:30:54" // for de_DE.utf8
```
*/
@deprecated({
  reason: "Use `Date.toLocaleTimeString` instead.",
  migrate: Date.toLocaleTimeString(),
})
@send
external toLocaleTimeString: t => string = "toLocaleTimeString"

/* TODO: has overloads with somewhat poor browser support */

/**
Returns a string representing the date and time of day for the given `Date` in
the current locale and time zone. See
[`Date.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
on MDN.

## Examples

```rescript
Js.Date.toString(
  exampleDate,
) == "Thu Nov 29 1973 22:30:54 GMT+0100 (Central European Standard Time)"
```
*/
@deprecated({
  reason: "Use `Date.toString` instead.",
  migrate: Date.toString(),
})
@send
external toString: t => string = "toString"

/**
Returns a string representing the time of day for the given `Date` in the
current locale and time zone.  See
[`Date.toTimeString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toTimeString)
on MDN.

## Examples

```rescript
Js.Date.toTimeString(exampleDate) == "22:30:54 GMT+0100 (Central European Standard Time)"
```
*/
@deprecated({
  reason: "Use `Date.toTimeString` instead.",
  migrate: Date.toTimeString(),
})
@send
external toTimeString: t => string = "toTimeString"

/**
Returns a string representing the date and time of day for the given `Date` in
the current locale and UTC (GMT time zone). See
[`Date.toUTCString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toUTCString)
on MDN.

## Examples

```rescript
Js.Date.toUTCString(exampleDate) == "Thu, 29 Nov 1973 21:30:54 GMT"
```
*/
@deprecated({
  reason: "Use `Date.toUTCString` instead.",
  migrate: Date.toUTCString(),
})
@send
external toUTCString: t => string = "toUTCString"

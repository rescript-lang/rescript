/***
Bindings to JavaScript's `Temporal` API for dates, times, time zones and
durations.

The bindings call the global `Temporal` object, which Node.js provides from
version 26. On other runtimes, check support or install a polyfill that
defines `globalThis.Temporal`.

Temporal values are immutable. Compare them with the `equals` and `compare`
functions of their module. ReScript's `==`, `compare`, `<` and `>` don't look
inside Temporal values and treat any two of them as equal.

Strings are parsed with `fromStringOrThrow`, which throws a `RangeError` for
invalid input, or with `fromString`, which returns `None` instead. Other
functions throw a `RangeError` for values outside the range Temporal
supports, and a `TypeError` for arguments they can't use, like a record
without the fields they need.

See [`Temporal`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal) on MDN.

## Examples

```rescript
let date = Temporal.PlainDate.fromStringOrThrow("2024-02-29")
let nextYear = date->Temporal.PlainDate.addFields({years: 1.})
nextYear->Temporal.PlainDate.toString == "2025-02-28"
```
*/

module Common = Stdlib_Temporal_Common
module Duration = Stdlib_Temporal_Duration
module Instant = Stdlib_Temporal_Instant
module Now = Stdlib_Temporal_Now
module PlainDate = Stdlib_Temporal_PlainDate
module PlainDateTime = Stdlib_Temporal_PlainDateTime
module PlainMonthDay = Stdlib_Temporal_PlainMonthDay
module PlainTime = Stdlib_Temporal_PlainTime
module PlainYearMonth = Stdlib_Temporal_PlainYearMonth
module ZonedDateTime = Stdlib_Temporal_ZonedDateTime

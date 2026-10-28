/***
Bindings to JavaScript's `Temporal.Now`, which reads the current time from the
system clock. Without `~timeZone`, the functions use the system time zone.

See [`Temporal.Now`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Now) on MDN.
*/

/**
`timeZoneId()` returns the system time zone, like `"Asia/Seoul"`.

See [`Temporal.Now.timeZoneId`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Now/timeZoneId) on MDN.
*/
@val
external timeZoneId: unit => string = "Temporal.Now.timeZoneId"

/**
`instant()` returns the current exact time.

See [`Temporal.Now.instant`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Now/instant) on MDN.
*/
@val
external instant: unit => Stdlib_Temporal_Common.instant = "Temporal.Now.instant"

/**
`zonedDateTimeISO(~timeZone=?)` returns the current date and time in
`timeZone`, using the ISO 8601 calendar.

See [`Temporal.Now.zonedDateTimeISO`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Now/zonedDateTimeISO) on MDN.

## Examples

```rescript
Temporal.Now.zonedDateTimeISO(~timeZone="UTC")->Temporal.ZonedDateTime.timeZoneId == "UTC"
```
*/
@val
external zonedDateTimeISO: (~timeZone: string=?) => Stdlib_Temporal_Common.zonedDateTime =
  "Temporal.Now.zonedDateTimeISO"

/**
`plainDateTimeISO(~timeZone=?)` returns the current date and wall-clock time
in `timeZone`, using the ISO 8601 calendar.

See [`Temporal.Now.plainDateTimeISO`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Now/plainDateTimeISO) on MDN.
*/
@val
external plainDateTimeISO: (~timeZone: string=?) => Stdlib_Temporal_Common.plainDateTime =
  "Temporal.Now.plainDateTimeISO"

/**
`plainDateISO(~timeZone=?)` returns the current date in `timeZone`, using the
ISO 8601 calendar.

See [`Temporal.Now.plainDateISO`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Now/plainDateISO) on MDN.
*/
@val
external plainDateISO: (~timeZone: string=?) => Stdlib_Temporal_Common.plainDate =
  "Temporal.Now.plainDateISO"

/**
`plainTimeISO(~timeZone=?)` returns the current wall-clock time in
`timeZone`.

See [`Temporal.Now.plainTimeISO`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal/Now/plainTimeISO) on MDN.
*/
@val
external plainTimeISO: (~timeZone: string=?) => Stdlib_Temporal_Common.plainTime =
  "Temporal.Now.plainTimeISO"

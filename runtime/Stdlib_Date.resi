/***
  Functions for interacting with JavaScript Dates.
*/

/**
A type representing a JavaScript date.
*/
type t

/**
Time, in milliseconds, since / until the UNIX epoch (January 1, 1970 00:00:00 UTC).
Positive numbers represent dates after, negative numbers dates before epoch.
*/
type msSinceEpoch = float

/**
A type representing date time format options.

Note: There are some properties missing:
- fractionalSecondDigits
- dayPeriod
- calendar
- numberingSystem
- localeMatcher
- timeZone
- hour12
- hourCycle
- formatMatcher

See full spec at https://tc39.es/ecma402/#datetimeformat-objects
*/
type localeOptions = {
  dateStyle?: [#full | #long | #medium | #short],
  timeStyle?: [#full | #long | #medium | #short],
  weekday?: [#long | #narrow | #short],
  era?: [#long | #narrow | #short],
  year?: [#"2-digit" | #numeric],
  month?: [#"2-digit" | #long | #narrow | #numeric | #short],
  day?: [#"2-digit" | #numeric],
  hour?: [#"2-digit" | #numeric],
  minute?: [#"2-digit" | #numeric],
  second?: [#"2-digit" | #numeric],
  timeZoneName?: [#long | #short],
}

/**
`make()`

Creates a date object with the current date time as value.

## Examples
```rescript
Date.make()
```
*/
@new
external make: unit => t = "Date"

/**
`fromString(dateTimeString)`

Creates a date object from given date time string.
The string has to be in the ISO 8601 format YYYY-MM-DDTHH:mm:ss.sssZ (https://tc39.es/ecma262/#sec-date-time-string-format).

Invalid date time strings will create invalid dates.
You can use the result like any valid date, but many functions like `toString` will return "Invalid Date" or functions like `Date.getTime` will return NaN.

## Examples
```rescript
Date.fromString("2023") // 2023-01-01T00:00:00.000Z

Date.fromString("2023-02-20") // 2023-02-20T00:00:00.000Z

Date.fromString("2023-02-20T16:40:00.00Z") // 2023-02-20T16:40:00.000Z

Date.fromString("") // Invalid Date

Date.fromString("")->Date.getTime // NaN
```
*/
@new
external fromString: string => t = "Date"

/**
`fromTime(msSinceEpoch)`

Creates a date object from the given time in milliseconds since / until UNIX epoch (January 1, 1970 00:00:00 UTC).
Positive numbers create dates after epoch, negative numbers create dates before epoch.

## Examples
```rescript
Date.fromTime(0.0)
// 1970-01-01T00:00:00.000Z

Date.fromTime(-86_400_000.0)
// 1969-12-31T00:00:00.000Z

Date.fromTime(86_400_000.0)
// 1970-01-02T00:00:00.000Z
```
*/
@new
external fromTime: msSinceEpoch => t = "Date"

/**
Creates a date object with the given year and month.
Be aware of using a value for year < 100, because it behaves inconsistent (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#interpretation_of_two-digit_years).
Months are 0-indexed (0 = January, 11 = December).
Values, which are out of range, will be carried over to the next bigger unit (s. example).

## Examples
```rescript
Date.makeWithYM(~year=2023, ~month=0)
// 2023-01-01T00:00:00.000Z

Date.makeWithYM(~year=2023, ~month=11)
// 2023-12-01T00:00:00.000Z

Date.makeWithYM(~year=2023, ~month=12)
// 2024-01-01T00:00:00.000Z

Date.makeWithYM(~year=2023, ~month=-1)
// 2022-12-01T00:00:00.000Z

// Note: The output depends on your local time zone.
// In nodejs you can change it by using the TZ env (`export TZ='Europe/London' && node index.bs.js`)

```
*/
@new
external makeWithYM: (~year: int, ~month: int) => t = "Date"

/**
Creates a date object with the given year, month and date (day of month).
Be aware of using a value for year < 100, because it behaves inconsistent (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#interpretation_of_two-digit_years).
Months are 0-indexed (0 = January, 11 = December).
Values, which are out of range, will be carried over to the next bigger unit (s. example).

## Examples
```rescript
Date.makeWithYMD(~year=2023, ~month=1, ~date=20)
// 2023-02-20T00:00:00.000Z

Date.makeWithYMD(~year=2023, ~month=1, ~date=-1)
// 2022-11-29T00:00:00.000Z

Date.makeWithYMD(~year=2023, ~month=1, ~date=29)
// 2023-03-01T00:00:00.000Z
```
*/
@new
external makeWithYMD: (~year: int, ~month: int, ~date: int) => t = "Date"

/**
Creates a date object with the given year, month, date (day of month) and hours.
Be aware of using a value for year < 100, because it behaves inconsistent (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#interpretation_of_two-digit_years).
Months are 0-indexed (0 = January, 11 = December).
Values, which are out of range, will be carried over to the next bigger unit (s. example).

## Examples
```rescript
Date.makeWithYMDH(~year=2023, ~month=1, ~date=20, ~hours=16)
// 2023-02-20T16:00:00.000Z

Date.makeWithYMDH(~year=2023, ~month=1, ~date=20, ~hours=24)
// 2023-02-21T00:00:00.000Z

Date.makeWithYMDH(~year=2023, ~month=1, ~date=20, ~hours=-1)
// 2023-02-19T23:00:00.000Z

// Note: The output depends on your local time zone.
// In nodejs you can change it by using the TZ env (`export TZ='Europe/London' && node index.bs.js`)

```
*/
@new
external makeWithYMDH: (~year: int, ~month: int, ~date: int, ~hours: int) => t = "Date"

/**
Creates a date object with the given year, month, date (day of month), hours and minutes.
Be aware of using a value for year < 100, because it behaves inconsistent (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#interpretation_of_two-digit_years).
Months are 0-indexed (0 = January, 11 = December).
Values, which are out of range, will be carried over to the next bigger unit (s. example).

## Examples
```rescript
Date.makeWithYMDHM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40)
// 2023-02-20T16:40:00.000Z

Date.makeWithYMDHM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=60)
// 2023-02-20T17:00:00.000Z

Date.makeWithYMDHM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=-1)
// 2023-02-20T15:59:00.000Z

// Note: The output depends on your local time zone.
// In nodejs you can change it by using the TZ env (`export TZ='Europe/London' && node index.bs.js`)

```
*/
@new
external makeWithYMDHM: (~year: int, ~month: int, ~date: int, ~hours: int, ~minutes: int) => t =
  "Date"

/**
Creates a date object with the given year, month, date (day of month), hours, minutes and seconds.
Be aware of using a value for year < 100, because it behaves inconsistent (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#interpretation_of_two-digit_years).
Months are 0-indexed (0 = January, 11 = December).
Values, which are out of range, will be carried over to the next bigger unit (s. example).

## Examples
```rescript
Date.makeWithYMDHMS(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=0)
// 2023-02-20T16:40:00.000Z

Date.makeWithYMDHMS(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=60)
// 2023-02-20T16:41:00.000Z

Date.makeWithYMDHMS(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=-1)
// 2023-02-20T16:39:59.000Z

// Note: The output depends on your local time zone.
// In nodejs you can change it by using the TZ env (`export TZ='Europe/London' && node index.bs.js`)

```
*/
@new
external makeWithYMDHMS: (
  ~year: int,
  ~month: int,
  ~date: int,
  ~hours: int,
  ~minutes: int,
  ~seconds: int,
) => t = "Date"

/**
Creates a date object with the given year, month, date (day of month), hours, minutes, seconds and milliseconds.
Be aware of using a value for year < 100, because it behaves inconsistent (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#interpretation_of_two-digit_years).
Months are 0-indexed (0 = January, 11 = December).
Values, which are out of range, will be carried over to the next bigger unit (s. example).

## Examples
```rescript
Date.makeWithYMDHMSM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=0, ~milliseconds=0)
// 2023-02-20T16:40:00.000Z

Date.makeWithYMDHMSM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=0, ~milliseconds=1000)
// 2023-02-20T16:40:01.000Z

Date.makeWithYMDHMSM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=0, ~milliseconds=-1)
// 2023-02-20T16:39:59.999Z

// Note: The output depends on your local time zone.
// In nodejs you can change it by using the TZ env (`export TZ='Europe/London' && node index.bs.js`)

```
*/
@new
external makeWithYMDHMSM: (
  ~year: int,
  ~month: int,
  ~date: int,
  ~hours: int,
  ~minutes: int,
  ~seconds: int,
  ~milliseconds: int,
) => t = "Date"
module UTC: {
  /**
  Returns the time, in milliseconds, since UNIX epoch (January 1, 1970 00:00:00 UTC).
  Be aware of using a value for year < 100, because it behaves inconsistent (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#interpretation_of_two-digit_years).
  Months are 0-indexed (0 = January, 11 = December).
  Values, which are out of range, will be carried over to the next bigger unit (s. example).

  ## Examples
  ```rescript
  Date.UTC.makeWithYM(~year=2023, ~month=0)
  // 1672531200000

  Date.UTC.makeWithYM(~year=2023, ~month=11)
  // 1701388800000

  Date.UTC.makeWithYM(~year=2023, ~month=12)
  // 1704067200000

  Date.UTC.makeWithYM(~year=2023, ~month=-1)
  // 1669852800000
  ```
  */
  @val
  external makeWithYM: (~year: int, ~month: int) => msSinceEpoch = "Date.UTC"

  /**
  Returns the time, in milliseconds, since UNIX epoch (January 1, 1970 00:00:00 UTC).
  Be aware of using a value for year < 100, because it behaves inconsistent (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#interpretation_of_two-digit_years).
  Months are 0-indexed (0 = January, 11 = December).
  Values, which are out of range, will be carried over to the next bigger unit (s. example).

  ## Examples
  ```rescript
  Date.UTC.makeWithYMD(~year=2023, ~month=1, ~date=20)
  // 1676851200000

  Date.UTC.makeWithYMD(~year=2023, ~month=1, ~date=-1)
  // 1675036800000

  Date.UTC.makeWithYMD(~year=2023, ~month=1, ~date=29)
  // 1677628800000
  ```
  */
  @val
  external makeWithYMD: (~year: int, ~month: int, ~date: int) => msSinceEpoch = "Date.UTC"

  /**
  Returns the time, in milliseconds, since UNIX epoch (January 1, 1970 00:00:00 UTC).
  Be aware of using a value for year < 100, because it behaves inconsistent (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#interpretation_of_two-digit_years).
  Months are 0-indexed (0 = January, 11 = December).
  Values, which are out of range, will be carried over to the next bigger unit (s. example).

  ## Examples
  ```rescript
  Date.UTC.makeWithYMDH(~year=2023, ~month=1, ~date=20, ~hours=16)
  // 1676908800000

  Date.UTC.makeWithYMDH(~year=2023, ~month=1, ~date=20, ~hours=24)
  // 1676937600000

  Date.UTC.makeWithYMDH(~year=2023, ~month=1, ~date=20, ~hours=-1)
  // 1676847600000
  ```
  */
  @val
  external makeWithYMDH: (~year: int, ~month: int, ~date: int, ~hours: int) => msSinceEpoch =
    "Date.UTC"

  /**
  Returns the time, in milliseconds, since UNIX epoch (January 1, 1970 00:00:00 UTC).
  Be aware of using a value for year < 100, because it behaves inconsistent (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#interpretation_of_two-digit_years).
  Months are 0-indexed (0 = January, 11 = December).
  Values, which are out of range, will be carried over to the next bigger unit (s. example).

  ## Examples
  ```rescript
  Date.UTC.makeWithYMDHM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40)
  // 1676911200000

  Date.UTC.makeWithYMDHM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=60)
  // 1676912400000

  Date.UTC.makeWithYMDHM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=-1)
  // 1676908740000
  ```
  */
  @val
  external makeWithYMDHM: (
    ~year: int,
    ~month: int,
    ~date: int,
    ~hours: int,
    ~minutes: int,
  ) => msSinceEpoch = "Date.UTC"

  /**
  Returns the time, in milliseconds, since UNIX epoch (January 1, 1970 00:00:00 UTC).
  Be aware of using a value for year < 100, because it behaves inconsistent (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#interpretation_of_two-digit_years).
  Months are 0-indexed (0 = January, 11 = December).
  Values, which are out of range, will be carried over to the next bigger unit (s. example).

  ## Examples
  ```rescript
  Date.UTC.makeWithYMDHMS(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=0)
  // 1676911200000

  Date.UTC.makeWithYMDHMS(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=60)
  // 1676911260000

  Date.UTC.makeWithYMDHMS(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=-1)
  // 1676911199000
  ```
  */
  @val
  external makeWithYMDHMS: (
    ~year: int,
    ~month: int,
    ~date: int,
    ~hours: int,
    ~minutes: int,
    ~seconds: int,
  ) => msSinceEpoch = "Date.UTC"

  /**
  Returns the time, in milliseconds, since UNIX epoch (January 1, 1970 00:00:00 UTC).
  Be aware of using a value for year < 100, because it behaves inconsistent (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#interpretation_of_two-digit_years).
  Months are 0-indexed (0 = January, 11 = December).
  Values, which are out of range, will be carried over to the next bigger unit (s. example).

  ## Examples
  ```rescript
  Date.UTC.makeWithYMDHMSM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=0, ~milliseconds=0)->Console.log
  // 1676911200000

  Date.UTC.makeWithYMDHMSM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=0, ~milliseconds=1000)->Console.log
  // 1676911201000

  Date.UTC.makeWithYMDHMSM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=0, ~milliseconds=-1)->Console.log
  // 1676911199999
  ```
  */
  @val
  external makeWithYMDHMSM: (
    ~year: int,
    ~month: int,
    ~date: int,
    ~hours: int,
    ~minutes: int,
    ~seconds: int,
    ~milliseconds: int,
  ) => msSinceEpoch = "Date.UTC"
}

/**
`now()`

Returns the time, in milliseconds, between UNIX epoch (January 1, 1970 00:00:00 UTC) and the current date time.
*/
@val
external now: unit => msSinceEpoch = "Date.now"

let equal: (t, t) => bool

let compare: (t, t) => Stdlib_Ordering.t

/**
`getTime(date)`

Returns the time, in milliseconds, between UNIX epoch (January 1, 1970 00:00:00 UTC) and the current date time.
Invalid dates will return NaN.
Dates before epoch will return negative numbers.

## Examples
```rescript
Date.fromString("2023-02-20")->Date.getTime
// 1676851200000
```
*/
@send
external getTime: t => msSinceEpoch = "getTime"

/**
`getTimezoneOffset(date)`

Returns the time in minutes between the UTC time and the locale time.
The timezone of the given date doesn't matter.

## Examples
```rescript
Date.fromString("2023-01-01")->Date.getTimezoneOffset
// -60 with local time zone = Europe/Berlin

Date.fromString("2023-06-01")->Date.getTimezoneOffset
// -120 with local time zone = Europe/Berlin
```
*/
@send
external getTimezoneOffset: t => int = "getTimezoneOffset"

/**
`getFullYear(date)`

Returns the year of a given date (according to local time).

## Examples
```rescript
Date.fromString("2023-02-20")->Date.getFullYear
// 2023
```
*/
@send
external getFullYear: t => int = "getFullYear"

/**
`getMonth(date)`

Returns the month (0-indexed) of a given date (according to local time).

## Examples
```rescript
Date.fromString("2023-01-01")->Date.getMonth
// 0
```
*/
@send
external getMonth: t => int = "getMonth"

/**
`getDate(date)`

Returns the date (day of month) of a given date (according to local time).

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.getDate
// 20
```
*/
@send
external getDate: t => int = "getDate"

/**
`getHours(date)`

Returns the hours of a given date (according to local time).

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.getHours
// 16
```
*/
@send
external getHours: t => int = "getHours"

/**
`getMinutes(date)`

Returns the minutes of a given date (according to local time).

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.getMinutes
// 40
```
*/
@send
external getMinutes: t => int = "getMinutes"

/**
`getSeconds(date)`

Returns the seconds of a given date (according to local time).

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.getSeconds
// 0
```
*/
@send
external getSeconds: t => int = "getSeconds"

/**
`getMilliseconds(date)`

Returns the milliseconds of a given date (according to local time).

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.getMilliseconds
// 0
```
*/
@send
external getMilliseconds: t => int = "getMilliseconds"

/**
`getDay(date)`

Returns the day of week of a given date (according to local time).
0 = Sunday, 1 = Monday, ... 6 = Saturday

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.getDay
// 1
```
*/
@send
external getDay: t => int = "getDay"

/**
`setFullYear(date, year)`

Sets the year of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setFullYear(2024)
```
*/
@send
external setFullYear: (t, int) => unit = "setFullYear"

/**
`setFullYearM(date, ~year, ~month)`

Sets the year and month of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setFullYearM(~year=2024, ~month=0)
```
*/
@send
external setFullYearM: (t, ~year: int, ~month: int) => unit = "setFullYear"

/**
`setFullYearMD(date, ~year, ~month, ~date)`

Sets the year, month and date (day of month) of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setFullYearMD(~year=2024, ~month=0, ~date=1)
```
*/
@send
external setFullYearMD: (t, ~year: int, ~month: int, ~date: int) => unit = "setFullYear"

/**
`setMonth(date, month)`

Sets the month of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setMonth(0)
```
*/
@send
external setMonth: (t, int) => unit = "setMonth"

/**
`setDate(date, day)`

Sets the date (day of month) of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setDate(1)
```
*/
@send
external setDate: (t, int) => unit = "setDate"

/**
`setHours(date, hours)`

Sets the hours of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setHours(0)
```
*/
@send
external setHours: (t, int) => unit = "setHours"

/**
`setHoursM(date, ~hours, ~minutes)`

Sets the hours and minutes of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setHoursM(~hours=0, ~minutes=0)
```
*/
@send
external setHoursM: (t, ~hours: int, ~minutes: int) => unit = "setHours"

/**
`setHoursMS(date, ~hours, ~minutes, ~seconds)`

Sets the hours, minutes and seconds of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setHoursMS(~hours=0, ~minutes=0, ~seconds=0)
```
*/
@send
external setHoursMS: (t, ~hours: int, ~minutes: int, ~seconds: int) => unit = "setHours"

/**
`setHoursMSMs(date, ~hours, ~minutes, ~seconds, ~milliseconds)`

Sets the hours, minutes, seconds and milliseconds of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setHoursMSMs(~hours=0, ~minutes=0, ~seconds=0, ~milliseconds=0)
```
*/
@send
external setHoursMSMs: (t, ~hours: int, ~minutes: int, ~seconds: int, ~milliseconds: int) => unit =
  "setHours"

/**
`setMinutes(date, minutes)`

Sets the minutes of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setMinutes(0)
```
*/
@send
external setMinutes: (t, int) => unit = "setMinutes"

/**
`setMinutesS(date, ~minutes, ~seconds)`

Sets the minutes and seconds of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setMinutesS(~minutes=0, ~seconds=0)
```
*/
@send
external setMinutesS: (t, ~minutes: int, ~seconds: int) => unit = "setMinutes"

/**
`setMinutesSMs(date, ~minutes, ~seconds, ~milliseconds)`

Sets the minutes, seconds and milliseconds of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setMinutesSMs(~minutes=0, ~seconds=0, ~milliseconds=0)
```
*/
@send
external setMinutesSMs: (t, ~minutes: int, ~seconds: int, ~milliseconds: int) => unit = "setMinutes"

/**
`setSeconds(date, seconds)`

Sets the seconds of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setSeconds(0)
```
*/
@send
external setSeconds: (t, int) => unit = "setSeconds"

/**
`setSecondsMs(date, ~seconds, ~milliseconds)`

Sets the seconds and milliseconds of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setSecondsMs(~seconds=0, ~milliseconds=0)
```
*/
@send
external setSecondsMs: (t, ~seconds: int, ~milliseconds: int) => unit = "setSeconds"

/**
`setMilliseconds(date, milliseconds)`

Sets the milliseconds of a date (according to local time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setMilliseconds(0)
```
*/
@send
external setMilliseconds: (t, int) => unit = "setMilliseconds"

/**
`getUTCFullYear(date)`

Returns the year of a given date (according to UTC time).

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCFullYear // 2022
```
*/
@send
external getUTCFullYear: t => int = "getUTCFullYear"

/**
`getUTCMonth(date)`

Returns the month of a given date (according to UTC time).

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCMonth // 11
```
*/
@send
external getUTCMonth: t => int = "getUTCMonth"

/**
`getUTCDate(date)`

Returns the date (day of month) of a given date (according to UTC time).

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCDate // 31
```
*/
@send
external getUTCDate: t => int = "getUTCDate"

/**
`getUTCHours(date)`

Returns the hours of a given date (according to UTC time).

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCHours // 23
```
*/
@send
external getUTCHours: t => int = "getUTCHours"

/**
`getUTCMinutes(date)`

Returns the minutes of a given date (according to UTC time).

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCMinutes // 0
```
*/
@send
external getUTCMinutes: t => int = "getUTCMinutes"

/**
`getUTCSeconds(date)`

Returns the seconds of a given date (according to UTC time).

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCSeconds // 0
```
*/
@send
external getUTCSeconds: t => int = "getUTCSeconds"

/**
`getUTCMilliseconds(date)`

Returns the milliseconds of a given date (according to UTC time).

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCMilliseconds // 0
```
*/
@send
external getUTCMilliseconds: t => int = "getUTCMilliseconds"

/**
`getUTCDay(date)`

Returns the day (day of week) of a given date (according to UTC time).
0 = Sunday, 1 = Monday, ... 6 = Saturday

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCDay // 6
```
*/
@send
external getUTCDay: t => int = "getUTCDay"

/**
`setUTCFullYear(date, year)`

Sets the year of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCFullYear(2024)
```
*/
@send
external setUTCFullYear: (t, int) => unit = "setUTCFullYear"

/**
`setUTCFullYearM(date, ~year, ~month)`

Sets the year and month of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCFullYearM(~year=2024, ~month=0)
```
*/
@send
external setUTCFullYearM: (t, ~year: int, ~month: int) => unit = "setUTCFullYear"

/**
`setUTCFullYearMD(date, ~year, ~month, ~date)`

Sets the year, month and date (day of month) of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCFullYearMD(~year=2024, ~month=0, ~date=1)
```
*/
@send
external setUTCFullYearMD: (t, ~year: int, ~month: int, ~date: int) => unit = "setUTCFullYear"

/**
`setUTCMonth(date, month)`

Sets the month of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCMonth(0)
```
*/
@send
external setUTCMonth: (t, int) => unit = "setUTCMonth"

/**
`setDate(date, day)`

Sets the date (day of month) of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCDate(1)
```
*/
@send
external setUTCDate: (t, int) => unit = "setUTCDate"

/**
`setUTCHours(date, hours)`

Sets the hours of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCHours(0)
```
*/
@send
external setUTCHours: (t, int) => unit = "setUTCHours"

/**
`setHoursM(date, ~hours, ~minutes)`

Sets the hours and minutes of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCHoursM(~hours=0, ~minutes=0)
```
*/
@send
external setUTCHoursM: (t, ~hours: int, ~minutes: int) => unit = "setUTCHours"

/**
`setUTCHoursMS(date, ~hours, ~minutes, ~seconds)`

Sets the hours, minutes and seconds of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCHoursMS(~hours=0, ~minutes=0, ~seconds=0)
```
*/
@send
external setUTCHoursMS: (t, ~hours: int, ~minutes: int, ~seconds: int) => unit = "setUTCHours"

/**
`setUTCHoursMSMs(date, ~hours, ~minutes, ~seconds, ~milliseconds)`

Sets the hours, minutes, seconds and milliseconds of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCHoursMSMs(~hours=0, ~minutes=0, ~seconds=0, ~milliseconds=0)
```
*/
@send
external setUTCHoursMSMs: (
  t,
  ~hours: int,
  ~minutes: int,
  ~seconds: int,
  ~milliseconds: int,
) => unit = "setUTCHours"

/**
`setUTCMinutes(date, minutes)`

Sets the minutes of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCMinutes(0)
```
*/
@send
external setUTCMinutes: (t, int) => unit = "setUTCMinutes"

/**
`setUTCMinutesS(date, ~minutes, ~seconds)`

Sets the minutes and seconds of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCMinutesS(~minutes=0, ~seconds=0)
```
*/
@send
external setUTCMinutesS: (t, ~minutes: int, ~seconds: int) => unit = "setUTCMinutes"

/**
`setUTCMinutesSMs(date, ~minutes, ~seconds, ~milliseconds)`

Sets the minutes, seconds and milliseconds of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCMinutesSMs(~minutes=0, ~seconds=0, ~milliseconds=0)
```
*/
@send
external setUTCMinutesSMs: (t, ~minutes: int, ~seconds: int, ~milliseconds: int) => unit =
  "setUTCMinutes"

/**
`setUTCSeconds(date, seconds)`

Sets the seconds of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCSeconds(0)
```
*/
@send
external setUTCSeconds: (t, int) => unit = "setUTCSeconds"

/**
`setUTCSecondsMs(date, ~seconds, ~milliseconds)`

Sets the seconds and milliseconds of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCSecondsMs(~seconds=0, ~milliseconds=0)
```
*/
@send
external setUTCSecondsMs: (t, ~seconds: int, ~milliseconds: int) => unit = "setUTCSeconds"

/**
`setUTCMilliseconds(date, milliseconds)`

Sets the milliseconds of a date (according to UTC time).
Beware this will *mutate* the date.

## Examples
```rescript
Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCMilliseconds(0)
```
*/
@send
external setUTCMilliseconds: (t, int) => unit = "setUTCMilliseconds"

/**
`toDateString(date)`

Converts a JavaScript date to a standard date string. The date will be mapped to the current time zone.
If you want to convert it to a localized string, use `Date.toLocaleDateString` instead.

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.toDateString->Console.log
// Sun Jan 01 2023

Date.fromString("2023-01-01T00:00:00.00+08:00")->Date.toDateString->Console.log
// Sat Dec 31 2022
```
*/
@send
external toDateString: t => string = "toDateString"

/**
`toString(date)`

Converts a JavaScript date to a standard date time string. The date will be mapped to the current time zone.
If you want to convert it to a localized string, use `Date.toLocaleString` instead.

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.toString->Console.log
// Sun Jan 01 2023 00:00:00 GMT+0100 (Central European Standard Time)

Date.fromString("2023-06-01T00:00:00.00+01:00")->Date.toString->Console.log
// Thu Jun 01 2023 01:00:00 GMT+0200 (Central European Summer Time)
```
*/
@send
external toString: t => string = "toString"

/**
`toTimeString(date)`

Converts a JavaScript date to a standard time string. The date will be mapped to the current time zone.
If you want to convert it to a localized string, use `Date.toLocaleStimeString` instead.

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.toTimeString->Console.log
// 00:00:00 GMT+0100 (Central European Standard Time)

Date.fromString("2023-01-01T00:00:00.00+08:00")->Date.toTimeString->Console.log
// 17:00:00 GMT+0100 (Central European Standard Time)
```
*/
@send
external toTimeString: t => string = "toTimeString"

/**
`toLocaleDateString(date)`

Converts a JavaScript date to a localized date string. It will use the current locale.

## Examples
```rescript
Date.make()->Date.toLocaleDateString->Console.log
// 2/19/2023
```
*/
@send
external toLocaleDateString: t => string = "toLocaleDateString"

/**
`toLocaleDateStringWithLocale(date, locale)`

Converts a JavaScript date to a localized date string. It will use the specified locale.

## Examples
```rescript
Date.make()->Date.toLocaleDateStringWithLocale("en-US")->Console.log
// 2/19/2023
```
*/
@send
external toLocaleDateStringWithLocale: (t, string) => string = "toLocaleDateString"

/**
`toLocaleDateStringWithLocaleAndOptions(date, locale, options)`

Converts a JavaScript date to a localized date string. It will use the specified locale and formatting options.

## Examples
```rescript
Date.make()->Date.toLocaleDateStringWithLocaleAndOptions("en-US", { dateStyle: #long })->Console.log
// February 19, 2023

Date.make()->Date.toLocaleDateStringWithLocaleAndOptions("de", { hour: #"2-digit", minute: #"2-digit" })->Console.log
// 19.2.2023, 15:40

Date.make()->Date.toLocaleDateStringWithLocaleAndOptions("de", { year: #numeric })->Console.log
// 2023
```
*/
@send
external toLocaleDateStringWithLocaleAndOptions: (t, string, localeOptions) => string =
  "toLocaleDateString"

/**
`toLocaleString(date)`

Converts a JavaScript date to a localized date-time string. It will use the current locale.

## Examples
```rescript
Date.make()->Date.toLocaleString->Console.log
// 2/19/2023, 3:40:00 PM
```
*/
@send
external toLocaleString: t => string = "toLocaleString"

/**
`toLocaleStringWithLocale(date, locale)`

Converts a JavaScript date to a localized date-time string. It will use the specified locale.

## Examples
```rescript
Date.make()->Date.toLocaleStringWithLocale("en-US")->Console.log
// 2/19/2023, 3:40:00 PM
```
*/
@send
external toLocaleStringWithLocale: (t, string) => string = "toLocaleString"

/**
`toLocaleStringWithLocaleAndOptions(date, locale, options)`

Converts a JavaScript date to a localized date-time string. It will use the specified locale and formatting options.

## Examples
```rescript
Date.make()->Date.toLocaleStringWithLocaleAndOptions("en", { dateStyle: #short, timeStyle: #short })->Console.log
// 2/19/23, 3:40 PM

Date.make()->Date.toLocaleStringWithLocaleAndOptions("en", { era: #long, year: #numeric, month: #"2-digit", day: #"2-digit", hour: #numeric, timeZoneName: #short })->Console.log
// 02/19/2023 Anno Domini, 3 PM GMT+1
```
*/
@send
external toLocaleStringWithLocaleAndOptions: (t, string, localeOptions) => string = "toLocaleString"

/**
`toLocaleTimeString(date)`

Converts a JavaScript date to a localized time string. It will use the current locale.

## Examples
```rescript
Date.make()->Date.toLocaleTimeString->Console.log
// 3:40:00 PM
```
*/
@send
external toLocaleTimeString: t => string = "toLocaleTimeString"

/**
`toLocaleTimeStringWithLocale(date, locale)`

Converts a JavaScript date to a localized time string. It will use the specified locale.

## Examples
```rescript
Date.make()->Date.toLocaleTimeStringWithLocale("en-US")->Console.log
// 3:40:00 PM
```
*/
@send
external toLocaleTimeStringWithLocale: (t, string) => string = "toLocaleTimeString"

/**
`toLocaleTimeStringWithLocaleAndOptions(date, locale, options)`

Converts a JavaScript date to a localized time string. It will use the specified locale and formatting options.

## Examples
```rescript
Date.make()->Date.toLocaleTimeStringWithLocaleAndOptions("en-US", { timeStyle: #long })->Console.log
// 3:40:00 PM GMT+1

Date.make()->Date.toLocaleTimeStringWithLocaleAndOptions("de", { hour: #"2-digit", minute: #"2-digit" })->Console.log
// 15:40
```
*/
@send
external toLocaleTimeStringWithLocaleAndOptions: (t, string, localeOptions) => string =
  "toLocaleTimeString"

/**
`toISOString(date)`

Converts a JavaScript date to a ISO 8601 string (YYYY-MM-DDTHH:mm:ss.sssZ). The date will be mapped to the UTC time.

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+00:00")->Date.toISOString->Console.log
// 2023-01-01T00:00:00.000Z

Date.fromString("2023-01-01T00:00:00.00+08:00")->Date.toISOString->Console.log
// 2022-12-31T16:00:00.000Z
```
*/
@send
external toISOString: t => string = "toISOString"

/**
`toUTCString(date)`

Converts a JavaScript date to date time string. The date will be mapped to the UTC time.

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+00:00")->Date.toUTCString->Console.log
// Sun, 01 Jan 2023 00:00:00 GMT

Date.fromString("2023-01-01T00:00:00.00+08:00")->Date.toUTCString->Console.log
// Sat, 31 Dec 2022 16:00:00 GMT
```
*/
@send
external toUTCString: t => string = "toUTCString"

/**
`toJSON(date)`

Converts a JavaScript date to a string.
If the date is valid, the function will return the same result as `Date.toISOString`.
Invalid dates will return `None`.

## Examples
```rescript
Date.fromString("2023-01-01T00:00:00.00+00:00")->Date.toJSON
// Some("2023-01-01T00:00:00.000Z")

Date.fromString("")->Date.toJSON
// None
```
*/
@return(nullable)
@send
external toJSON: t => option<string> = "toJSON"

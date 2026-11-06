/***
Types shared by the `Temporal` bindings.

The Temporal value types are defined here so that the modules converting
between them, like `Temporal.PlainDate.toPlainDateTime` and
`Temporal.PlainDateTime.toPlainDate`, can refer to each other's types.
Each module re-exports its own type as `t`, so `Temporal.PlainDate.t` and
`Temporal.Common.plainDate` are the same type.

The other types here are used by several modules. Types used by a single
module are defined in that module.
*/

// `@editor.completeFrom` makes the editor complete a value of these types from
// its module, as it does for values typed with the module's own `t`.

/** A `Temporal.Duration`. Same type as `Temporal.Duration.t`. */
@notUndefined @editor.completeFrom(Stdlib.Temporal.Duration)
type duration

/** A `Temporal.Instant`. Same type as `Temporal.Instant.t`. */
@notUndefined @editor.completeFrom(Stdlib.Temporal.Instant)
type instant

/** A `Temporal.PlainDate`. Same type as `Temporal.PlainDate.t`. */
@notUndefined @editor.completeFrom(Stdlib.Temporal.PlainDate)
type plainDate

/** A `Temporal.PlainTime`. Same type as `Temporal.PlainTime.t`. */
@notUndefined @editor.completeFrom(Stdlib.Temporal.PlainTime)
type plainTime

/** A `Temporal.PlainDateTime`. Same type as `Temporal.PlainDateTime.t`. */
@notUndefined @editor.completeFrom(Stdlib.Temporal.PlainDateTime)
type plainDateTime

/** A `Temporal.PlainYearMonth`. Same type as `Temporal.PlainYearMonth.t`. */
@notUndefined @editor.completeFrom(Stdlib.Temporal.PlainYearMonth)
type plainYearMonth

/** A `Temporal.PlainMonthDay`. Same type as `Temporal.PlainMonthDay.t`. */
@notUndefined @editor.completeFrom(Stdlib.Temporal.PlainMonthDay)
type plainMonthDay

/** A `Temporal.ZonedDateTime`. Same type as `Temporal.ZonedDateTime.t`. */
@notUndefined @editor.completeFrom(Stdlib.Temporal.ZonedDateTime)
type zonedDateTime

/**
A calendar identifier supported by Temporal, like `#gregory` or `#iso8601`.
Unlike `Intl.Common.calendar`, it leaves out `#islamic` and
`#"islamic-rgsa"`, which Temporal rejects.
*/
type calendar = [
  | #buddhist
  | #chinese
  | #coptic
  | #dangi
  | #ethioaa
  | #ethiopic
  | #gregory
  | #hebrew
  | #indian
  | #"islamic-civil"
  | #"islamic-tbla"
  | #"islamic-umalqura"
  | #iso8601
  | #japanese
  | #persian
  | #roc
]

/**
How out-of-range fields are handled: `#constrain` clamps them to the nearest
valid value, `#reject` throws a `RangeError`. Defaults to `#constrain`.
*/
type overflow = [#constrain | #reject]

type overflowOptions = {overflow?: overflow}

/**
How a wall-clock time that is skipped or repeated in a time zone, for example
during a daylight saving time change, is resolved. Defaults to `#compatible`.
*/
type disambiguation = [#compatible | #earlier | #later | #reject]

type dateUnit = [#year | #month | #week | #day]
type timeUnit = [#hour | #minute | #second | #millisecond | #microsecond | #nanosecond]
type dateTimeUnit = [dateUnit | timeUnit]

type roundingMode = [
  | #ceil
  | #floor
  | #expand
  | #trunc
  | #halfCeil
  | #halfFloor
  | #halfExpand
  | #halfTrunc
  | #halfEven
]

/**
Options for `until` and `since`. `'unit` is the set of units the receiving
type supports.
*/
type differenceOptions<'unit> = {
  largestUnit?: 'unit,
  smallestUnit?: 'unit,
  roundingIncrement?: int,
  roundingMode?: roundingMode,
}

/**
Options for `round`. `'unit` is the set of units the receiving type supports.
*/
type roundingOptions<'unit> = {
  smallestUnit: 'unit,
  roundingIncrement?: int,
  roundingMode?: roundingMode,
}

/** How many fractional second digits `toString` prints. Defaults to `#auto`. */
type fractionalSecondDigits = [#auto | #0 | #1 | #2 | #3 | #4 | #5 | #6 | #7 | #8 | #9]

/** Whether `toString` prints the calendar annotation. Defaults to `#auto`. */
type calendarName = [#auto | #always | #never | #critical]

type dateToStringOptions = {calendarName?: calendarName}

type timeToStringOptions = {
  fractionalSecondDigits?: fractionalSecondDigits,
  smallestUnit?: [#minute | #second | #millisecond | #microsecond | #nanosecond],
  roundingMode?: roundingMode,
}

type dateTimeToStringOptions = {
  ...timeToStringOptions,
  calendarName?: calendarName,
}

/**
The fields of a duration. At least one field must be given, or a `TypeError`
is thrown, and all non-zero fields must have the same sign. Omitted fields
default to `0.`, except in `Duration.with`, which keeps their current values.
*/
type durationFields = {
  years?: float,
  months?: float,
  weeks?: float,
  days?: float,
  hours?: float,
  minutes?: float,
  seconds?: float,
  milliseconds?: float,
  microseconds?: float,
  nanoseconds?: float,
}

/**
The fields of a wall-clock time. At least one field must be given, or a
`TypeError` is thrown. Omitted fields default to `0`, except in `with`, which
keeps their current values.
*/
type timeFields = {
  hour?: int,
  minute?: int,
  second?: int,
  millisecond?: int,
  microsecond?: int,
  nanosecond?: int,
}

/**
The fields of a calendar date. A date needs `year`, `month` (or `monthCode`)
and `day`. In a calendar with eras, like `#gregory`, `era` and `eraYear` can
replace `year`. `calendar` defaults to `#iso8601`, which has no eras.
*/
type dateFields = {
  year?: int,
  era?: string,
  eraYear?: int,
  month?: int,
  monthCode?: string,
  day: int,
  calendar?: calendar,
}

type dateTimeFields = {
  ...dateFields,
  ...timeFields,
}

/**
The date fields `with` can replace. The calendar can't be replaced. As with
every record passed to `with`, at least one field must be given, or a
`TypeError` is thrown.
*/
type dateUpdates = {
  year?: int,
  era?: string,
  eraYear?: int,
  month?: int,
  monthCode?: string,
  day?: int,
}

type dateTimeUpdates = {
  ...dateUpdates,
  ...timeFields,
}

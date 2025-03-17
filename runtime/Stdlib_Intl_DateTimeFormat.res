type t

type dateStyle = [#full | #long | #medium | #short]
type timeStyle = [#full | #long | #medium | #short]
type dayPeriod = [#narrow | #short | #long]
type weekday = [#narrow | #short | #long]
type era = [#narrow | #short | #long]
type year = [#numeric | #"2-digit"]
type month = [#numeric | #"2-digit" | #narrow | #short | #long]
type day = [#numeric | #"2-digit"]
type hour = [#numeric | #"2-digit"]
type minute = [#numeric | #"2-digit"]
type second = [#numeric | #"2-digit"]

/**
Firefox also supports IANA time zone names here
Node v19+ supports "shortOffset", "shortGeneric", "longOffset", and "longGeneric".
*/
type timeZoneName = [
  | #short
  | #long
  | #shortOffset
  | #shortGeneric
  | #longOffset
  | #longGeneric
]

type hourCycle = [#h11 | #h12 | #h23 | #h24]
type formatMatcher = [#basic | #"best fit"]
type fractionalSecondDigits = [#0 | #1 | #2 | #3]

type options = {
  dateStyle?: dateStyle, // can be used with timeStyle, but not other options
  timeStyle?: timeStyle, // can be used with dateStyle, but not other options
  calendar?: Stdlib_Intl_Common.calendar,
  dayPeriod?: dayPeriod, // only has an effect if a 12-hour clock is used
  numberingSystem?: Stdlib_Intl_Common.numberingSystem,
  localeMatcher?: Stdlib_Intl_Common.localeMatcher,
  timeZone?: string,
  hour12?: bool,
  hourCycle?: hourCycle,
  formatMatcher?: formatMatcher,
  // date-time components
  weekday?: weekday,
  era?: era,
  year?: year,
  month?: month,
  day?: day,
  hour?: hour,
  minute?: minute,
  second?: second,
  fractionalSecondDigits?: fractionalSecondDigits,
  timeZoneName?: timeZoneName,
}

type resolvedOptions = {
  dateStyle?: dateStyle,
  timeStyle?: timeStyle,
  weekday?: weekday,
  era?: era,
  year?: year,
  month?: month,
  day?: day,
  hour?: hour,
  minute?: minute,
  second?: second,
  fractionalSecondDigits?: fractionalSecondDigits,
  timeZoneName?: timeZoneName,
  calendar: Stdlib_Intl_Common.calendar,
  hour12: bool,
  hourCycle: hourCycle,
  locale: string,
  numberingSystem: Stdlib_Intl_Common.numberingSystem,
  timeZone: string,
}

type supportedLocalesOptions = {localeMatcher: Stdlib_Intl_Common.localeMatcher}

type dateTimeComponent = [
  | #day
  | #dayPeriod
  | #era
  | #fractionalSecond
  | #hour
  | #literal
  | #minute
  | #month
  | #relatedYear
  | #second
  | #timeZone
  | #weekday
  | #year
  | #yearName
]

type dateTimePart = {
  \"type": dateTimeComponent,
  value: string,
}

type dateTimeRangeSource = [#startRange | #shared | #endRange]
type dateTimeRangePart = {
  \"type": dateTimeComponent,
  value: string,
  source: dateTimeRangeSource,
}

@new external make: (~locales: array<string>=?, ~options: options=?) => t = "Intl.DateTimeFormat"

@val
external supportedLocalesOf: (array<string>, ~options: supportedLocalesOptions=?) => t =
  "Intl.DateTimeFormat.supportedLocalesOf"

@send external resolvedOptions: t => resolvedOptions = "resolvedOptions"

@send external format: (t, Stdlib_Date.t) => string = "format"
@send
external formatToParts: (t, Stdlib_Date.t) => array<dateTimePart> = "formatToParts"

@send
external formatRange: (t, ~startDate: Stdlib_Date.t, ~endDate: Stdlib_Date.t) => string =
  "formatRange"

@send
external formatRangeToParts: (
  t,
  ~startDate: Stdlib_Date.t,
  ~endDate: Stdlib_Date.t,
) => array<dateTimeRangePart> = "formatRangeToParts"

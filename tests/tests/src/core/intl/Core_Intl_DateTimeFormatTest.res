Console.log("---")
Console.log("Intl.DateTimeFormat")

Intl.DateTimeFormat.supportedLocalesOf(["en-US", "en-GB"])->ignore
Intl.DateTimeFormat.supportedLocalesOf(
  ["en-US", "en-GB"],
  ~options={localeMatcher: #lookup},
)->ignore

let formatter = Intl.DateTimeFormat.make(~options={dateStyle: #full, timeStyle: #full})

Console.log(formatter->Intl.DateTimeFormat.format(Date.fromTime(Date.now())))

Console.log(
  formatter->Intl.DateTimeFormat.formatRange(
    ~startDate=Date.makeWithYMD(~year=2023, ~day=1, ~month=1),
    ~endDate=Date.makeWithYMD(~year=2023, ~day=31, ~month=12),
  ),
)

let options: Intl.DateTimeFormat.options = {
  hour12: false,
  hourCycle: #h24,
  timeZone: "UTC",
  weekday: #narrow,
  era: #narrow,
  year: #"2-digit",
  month: #"2-digit",
  day: #"2-digit",
  hour: #"2-digit",
  minute: #"2-digit",
  second: #"2-digit",
  fractionalSecondDigits: #3,
  timeZoneName: #longGeneric,
}
let formatter = Intl.DateTimeFormat.make(~options)

Console.log(formatter->Intl.DateTimeFormat.format(Date.fromTime(Date.now())))

let formatter = Intl.DateTimeFormat.make(~options={...options, timeZoneName: #long})
Console.log(formatter->Intl.DateTimeFormat.format(Date.fromTime(Date.now())))

let formatter = Intl.DateTimeFormat.make(~options={...options, timeZoneName: #longOffset})
Console.log(formatter->Intl.DateTimeFormat.format(Date.fromTime(Date.now())))

let formatter = Intl.DateTimeFormat.make(~options={...options, timeZoneName: #short})
Console.log(formatter->Intl.DateTimeFormat.format(Date.fromTime(Date.now())))

let formatter = Intl.DateTimeFormat.make(~options={...options, timeZoneName: #shortGeneric})
Console.log(formatter->Intl.DateTimeFormat.format(Date.fromTime(Date.now())))

let formatter = Intl.DateTimeFormat.make(~options={...options, timeZoneName: #shortOffset})
Console.log(formatter->Intl.DateTimeFormat.format(Date.fromTime(Date.now())))

let resolvedOptions = Intl.DateTimeFormat.make()->Intl.DateTimeFormat.resolvedOptions
let {Intl.DateTimeFormat.timeZone: timeZone, _} = resolvedOptions

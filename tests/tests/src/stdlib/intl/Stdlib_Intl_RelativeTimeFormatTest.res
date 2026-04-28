Console.log("---")
Console.log("Intl.RelativeTimeFormat")

Intl.RelativeTimeFormat.supportedLocalesOf(["en-US", "en-GB"])->ignore
Intl.RelativeTimeFormat.supportedLocalesOf(
  ["en-US", "en-GB"],
  ~options={localeMatcher: #lookup},
)->ignore

let _formatter = Intl.RelativeTimeFormat.make()
let _formatter = Intl.RelativeTimeFormat.make(~locales=["en-US", "en-GB"])
let _formatter = Intl.RelativeTimeFormat.make(
  ~options={
    numeric: #always,
    style: #narrow,
  },
)
let formatter = Intl.RelativeTimeFormat.make(
  ~locales=["en-US"],
  ~options={
    numeric: #always,
    style: #narrow,
  },
)

formatter->Intl.RelativeTimeFormat.resolvedOptions->Console.log

formatter->Intl.RelativeTimeFormat.format(3, #hour)->Console.log
formatter->Intl.RelativeTimeFormat.formatToParts(3, #hour)->Console.log

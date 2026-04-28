Console.log("---")
Console.log("Intl.ListFormat")

let _formatter = Intl.ListFormat.make()
let _formatter = Intl.ListFormat.make(~locales=["en-US", "en-GB"])
let _formatter = Intl.ListFormat.make(
  ~locales=["en-US", "en-GB"],
  ~options={style: #long, \"type": #conjunction},
)
Intl.ListFormat.supportedLocalesOf(["en-US", "en-GB"])->ignore
Intl.ListFormat.supportedLocalesOf(["en-US", "en-GB"], ~options={localeMatcher: #lookup})->ignore

let formatter = Intl.ListFormat.make(~options={style: #long, \"type": #conjunction})

formatter->Intl.ListFormat.resolvedOptions->Console.log
formatter->Intl.ListFormat.format(["one", "two", "three"])->Console.log
formatter->Intl.ListFormat.formatToParts(["one", "two", "three"])->Console.log

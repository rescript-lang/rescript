Console.log("---")
Console.log("Intl.Locale")

let _locale = Intl.Locale.make("en-US")
let locale = Intl.Locale.make(
  "en-US",
  ~options={
    calendar: #hebrew,
    collation: #compat,
    hourCycle: #h24,
    caseFirst: #upper,
    numeric: true,
  },
)

locale->Intl.Locale.baseName->Console.log
locale->Intl.Locale.calendar->Console.log
locale->Intl.Locale.caseFirst->Console.log
locale->Intl.Locale.collation->Console.log
locale->Intl.Locale.hourCycle->Console.log
locale->Intl.Locale.language->Console.log
locale->Intl.Locale.numberingSystem->Console.log
locale->Intl.Locale.numeric->Console.log
locale->Intl.Locale.region->Console.log
locale->Intl.Locale.script->Console.log
locale->Intl.Locale.maximize->Console.log
locale->Intl.Locale.minimize->Console.log

Console.log("---")
Console.log("Intl.Collator")

let _collator = Intl.Collator.make()
let _collator = Intl.Collator.make(~locales=["en-US"])
let _collator = Intl.Collator.make(~locales=["en-US", "en-GB"])
let collator = Intl.Collator.make(
  ~locales=["en-US"],
  ~options={caseFirst: #upper, sensitivity: #base, ignorePunctuation: true, numeric: true},
)
Intl.Collator.supportedLocalesOf(["en-US", "en-GB"])->ignore
Intl.Collator.supportedLocalesOf(["en-US", "en-GB"], ~options={localeMatcher: #lookup})->ignore

collator->Intl.Collator.resolvedOptions->Console.log
collator->Intl.Collator.compare("hi", "hï")->Console.log
Intl.Collator.supportedLocalesOf(["hi"])->Console.log

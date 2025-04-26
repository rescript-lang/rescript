include Core_Intl_CollatorTest
include Core_Intl_DateTimeFormatTest
include Core_Intl_ListFormatTest
include Core_Intl_LocaleTest
include Core_Intl_NumberFormatTest
include Core_Intl_PluralRulesTest
include Core_Intl_RelativeTimeFormatTest
include Core_Intl_SegmenterTest

Console.log("---")
Console.log("Intl")

Intl.getCanonicalLocalesExn("EN-US")->Console.log
Intl.getCanonicalLocalesManyExn(["EN-US", "Fr"])->Console.log

try {
  Intl.getCanonicalLocalesExn("bloop")->Console.log
} catch {
| JsExn(e) => Console.error(e)
}

try {
  Intl.supportedValuesOfExn("calendar")->Console.log
  Intl.supportedValuesOfExn("collation")->Console.log
  Intl.supportedValuesOfExn("currency")->Console.log
  Intl.supportedValuesOfExn("numberingSystem")->Console.log
  Intl.supportedValuesOfExn("timeZone")->Console.log
  Intl.supportedValuesOfExn("unit")->Console.log
} catch {
| JsExn(e) => Console.error(e)
}

try {
  Intl.supportedValuesOfExn("someInvalidKey")->ignore

  Console.error("Shouldn't have been hit")
} catch {
| JsExn(e) =>
  switch JsExn.message(e)->Option.map(String.toLowerCase) {
  | Some("invalid key : someinvalidkey") => Console.log("Caught expected error")
  | message => {
      Console.warn(`Unexpected error message: "${message->Option.getUnsafe}"`)
      JsExn.throw(e)
    }
  }
| e =>
  switch JsExn.fromException(e) {
  | Some(e) => JsExn.throw(e)
  | None => Console.error("Unexpected error")
  }
}

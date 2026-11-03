@throws(JsExn)
let optionGetOrThrow = o => o->Option.getOrThrow

@throws(JsExn)
let resultGetOrThrow = r => r->Result.getOrThrow

@throws(Invalid_argument)
let nullGetOrThrow = n => n->Null.getOrThrow

@throws(JsExn)
let bigIntFromStringExn = s => s->BigInt.fromStringOrThrow

@throws(JsExn)
let jsonParseExn = s => s->JSON.parseOrThrow

@throws(JsExn)
let durationFromStringOrThrow = s => Temporal.Duration.fromStringOrThrow(s)

@throws(JsExn)
let instantFromStringOrThrow = s => Temporal.Instant.fromStringOrThrow(s)

@throws(JsExn)
let plainDateFromStringOrThrow = s => Temporal.PlainDate.fromStringOrThrow(s)

@throws(JsExn)
let plainDateTimeFromStringOrThrow = s => Temporal.PlainDateTime.fromStringOrThrow(s)

@throws(JsExn)
let plainMonthDayFromStringOrThrow = s => Temporal.PlainMonthDay.fromStringOrThrow(s)

@throws(JsExn)
let plainTimeFromStringOrThrow = s => Temporal.PlainTime.fromStringOrThrow(s)

@throws(JsExn)
let plainYearMonthFromStringOrThrow = s => Temporal.PlainYearMonth.fromStringOrThrow(s)

@throws(JsExn)
let zonedDateTimeFromStringOrThrow = s => Temporal.ZonedDateTime.fromStringOrThrow(s)

@throws(JsExn)
let stdlibPlainDateFromStringOrThrow = s => Stdlib.Temporal.PlainDate.fromStringOrThrow(s)

@throws(JsExn)
let openedPlainDateFromStringOrThrow = s => {
  open Temporal
  PlainDate.fromStringOrThrow(s)
}

@throws(JsExn)
let directPlainDateFromStringOrThrow = s => Stdlib_Temporal_PlainDate.fromStringOrThrow(s)

@throws(JsExn)
let aliasPlainDateFromStringOrThrow = s => Stdlib_Temporal.PlainDate.fromStringOrThrow(s)

@throws(JsExn)
let canonicalLocales = s => Intl.getCanonicalLocalesExn(s)

@throws(JsExn)
let canonicalLocalesMany = s => Intl.getCanonicalLocalesManyExn(s)

@throws(JsExn)
let supportedValuesOf = s => Intl.supportedValuesOfExn(s)

@throws(JsExn)
let stdlibCanonicalLocales = s => Stdlib.Intl.getCanonicalLocalesExn(s)

let plainDateFromStringUnannotated = s => Temporal.PlainDate.fromStringOrThrow(s)

@throws(JsExn)
let callPanic = () => panic("x")

@throws(Assert_failure)
let callAssertEqual = (a, b) => assertEqual(a, b)

@throws(JsExn)
let rangeError = () => JsError.RangeError.throwWithMessage("x")

@throws(Not_found)
let listHead = l => List.headOrThrow(l)

@throws(Invalid_argument)
let boolFromString = s => Bool.fromStringOrThrow(s)

@throws(Invalid_argument)
let nullableGetOrThrow = n => Nullable.getOrThrow(n)

@throws(JsExn)
let stringifyAny = x => JSON.stringifyAny(x)

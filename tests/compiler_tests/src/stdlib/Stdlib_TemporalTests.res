// scripts/test.js runs these tests on their own, because the installation
// tests also run Stdlib_TestSuite on Node.js 22, which lacks Temporal.

let eq = (a, b) => a == b

let catch = f =>
  try {
    f()->ignore
    JsError.throwWithMessage("no exception raised")
  } catch {
  | JsExn(err) => err
  }

let throwsReferenceErrorWithoutTemporal: (unit => 'a) => bool = %raw(`f => {
  const temporal = Object.getOwnPropertyDescriptor(globalThis, "Temporal")
  delete globalThis.Temporal
  try {
    f()
    return false
  } catch (error) {
    return error instanceof ReferenceError
  } finally {
    Object.defineProperty(globalThis, "Temporal", temporal)
  }
}`)

// `fromString` returns `None` only for a `RangeError`, so a missing `Temporal`
// global isn't reported as an invalid string.
Test.run(
  __POS_OF__("fromString rethrows errors other than RangeError"),
  throwsReferenceErrorWithoutTemporal(() => Temporal.PlainDate.fromString("2024-02-29")),
  eq,
  true,
)

// A module named like the global must not shadow the `Temporal` global that
// the bindings call.
module ShadowingModule = {
  module Temporal = {
    let name = "local"
  }

  let today = () => Stdlib.Temporal.Now.plainDateISO(~timeZone="UTC")
}

Test.run(
  __POS_OF__("local Temporal module doesn't shadow the global"),
  ShadowingModule.today()->Temporal.PlainDate.calendarId,
  eq,
  #iso8601,
)

// Labeled constructor arguments are passed positionally, omitted ones as `undefined`.
Test.run(
  __POS_OF__("PlainTime.make without fields"),
  Temporal.PlainTime.make()->Temporal.PlainTime.toString,
  eq,
  "00:00:00",
)

Test.run(
  __POS_OF__("PlainTime.make with a later field only"),
  Temporal.PlainTime.make(~minute=30)->Temporal.PlainTime.toString,
  eq,
  "00:30:00",
)

Test.run(
  __POS_OF__("PlainDateTime.make with a later time field only"),
  Temporal.PlainDateTime.make(
    ~year=2024,
    ~month=2,
    ~day=29,
    ~second=5,
  )->Temporal.PlainDateTime.toString,
  eq,
  "2024-02-29T00:00:05",
)

Test.run(
  __POS_OF__("PlainDateTime.make with a calendar"),
  Temporal.PlainDateTime.make(
    ~year=2024,
    ~month=2,
    ~day=29,
    ~calendar=#gregory,
  )->Temporal.PlainDateTime.calendarId,
  eq,
  #gregory,
)

Test.run(
  __POS_OF__("PlainDate.make with a calendar"),
  Temporal.PlainDate.make(
    ~year=2024,
    ~month=2,
    ~day=29,
    ~calendar=#gregory,
  )->Temporal.PlainDate.toString,
  eq,
  "2024-02-29[u-ca=gregory]",
)

Test.run(
  __POS_OF__("PlainYearMonth.make with a reference day"),
  Temporal.PlainYearMonth.make(
    ~year=2024,
    ~month=2,
    ~referenceDay=15,
  )->Temporal.PlainYearMonth.toString(~options={calendarName: #always}),
  eq,
  "2024-02-15[u-ca=iso8601]",
)

Test.run(
  __POS_OF__("PlainMonthDay.make with a reference year"),
  Temporal.PlainMonthDay.make(
    ~month=2,
    ~day=29,
    ~referenceYear=2024,
  )->Temporal.PlainMonthDay.toString(~options={calendarName: #always}),
  eq,
  "2024-02-29[u-ca=iso8601]",
)

Test.run(
  __POS_OF__("ZonedDateTime.fromEpochNanoseconds with a calendar"),
  Temporal.ZonedDateTime.fromEpochNanoseconds(
    0n,
    ~timeZone="UTC",
    ~calendar=#gregory,
  )->Temporal.ZonedDateTime.toString,
  eq,
  "1970-01-01T00:00:00+00:00[UTC][u-ca=gregory]",
)

Test.run(
  __POS_OF__("fromStringOrThrow throws on invalid input"),
  catch(() => Temporal.PlainDate.fromStringOrThrow("2024-13-01"))->JsExn.name,
  eq,
  Some("RangeError"),
)

Test.run(
  __POS_OF__("fromFields rejects out-of-range fields"),
  catch(() =>
    Temporal.PlainDate.fromFields({year: 2024, month: 2, day: 30}, ~options={overflow: #reject})
  )->JsExn.name,
  eq,
  Some("RangeError"),
)

Test.run(
  __POS_OF__("addFields rejects a missing day"),
  catch(() =>
    Temporal.PlainDate.fromStringOrThrow("2024-01-31")->Temporal.PlainDate.addFields(
      {months: 1.},
      ~options={overflow: #reject},
    )
  )->JsExn.name,
  eq,
  Some("RangeError"),
)

Test.run(
  __POS_OF__("Duration.addFields rejects calendar units"),
  catch(() =>
    Temporal.Duration.fromFields({hours: 1.})->Temporal.Duration.addFields({months: 1.})
  )->JsExn.name,
  eq,
  Some("RangeError"),
)

let date = Temporal.PlainDate.fromStringOrThrow("2024-02-29")

Test.run(__POS_OF__("era is None in the ISO calendar"), date->Temporal.PlainDate.era, eq, None)

Test.run(
  __POS_OF__("eraYear is Some in the Gregorian calendar"),
  date->Temporal.PlainDate.withCalendar(#gregory)->Temporal.PlainDate.eraYear,
  eq,
  Some(2024),
)

Test.run(
  __POS_OF__("compare sorts chronologically"),
  ["2024-03-01", "2024-01-01", "2024-02-01"]
  ->Array.map(Temporal.PlainDate.fromStringOrThrow)
  ->Array.toSorted(Temporal.PlainDate.compare)
  ->Array.map(date => date->Temporal.PlainDate.toString),
  eq,
  ["2024-01-01", "2024-02-01", "2024-03-01"],
)

Test.run(
  __POS_OF__("toJSON is used by JSON.stringify"),
  JSON.stringifyAny({"date": date}),
  eq,
  Some(`{"date":"2024-02-29"}`),
)

Test.run(
  __POS_OF__("toLocaleString passes Intl.DateTimeFormat options"),
  date->Temporal.PlainDate.toLocaleString(~locales=["en-US"], ~options={year: #numeric}),
  eq,
  "2024",
)

Test.run(
  __POS_OF__("until passes difference options"),
  date
  ->Temporal.PlainDate.until(
    Temporal.PlainDate.fromStringOrThrow("2025-03-31"),
    ~options={largestUnit: #year, smallestUnit: #month, roundingMode: #floor},
  )
  ->Temporal.Duration.toString,
  eq,
  "P1Y1M",
)

Test.run(
  __POS_OF__("Duration.total relative to a PlainDate"),
  Temporal.Duration.fromFields({months: 1.})->Temporal.Duration.total({
    unit: #day,
    relativeTo: Temporal.Duration.relativeToPlainDate(date->Temporal.PlainDate.with({day: 1})),
  }),
  eq,
  29.,
)

Test.run(
  __POS_OF__("Duration.round relative to a ZonedDateTime"),
  Temporal.Duration.fromFields({hours: 25.})
  ->Temporal.Duration.round({
    largestUnit: #day,
    relativeTo: Temporal.Duration.relativeToZonedDateTime(
      Temporal.ZonedDateTime.fromStringOrThrow("2024-03-10T00:00:00-05:00[America/New_York]"),
    ),
  })
  ->Temporal.Duration.toString,
  eq,
  "P1DT2H",
)

let utc = Temporal.ZonedDateTime.fromStringOrThrow("2024-01-01T00:00:00+00:00[UTC]")

Test.run(
  __POS_OF__("getTimeZoneTransition without a transition"),
  utc->Temporal.ZonedDateTime.getTimeZoneTransition(#previous)->Option.isNone,
  eq,
  true,
)

Test.run(
  __POS_OF__("getTimeZoneTransition with a transition"),
  utc
  ->Temporal.ZonedDateTime.withTimeZone("America/New_York")
  ->Temporal.ZonedDateTime.getTimeZoneTransition(#next)
  ->Option.map(transition => transition->Temporal.ZonedDateTime.toString),
  eq,
  Some("2024-03-10T03:00:00-04:00[America/New_York]"),
)

Test.run(
  __POS_OF__("Instant.epochNanoseconds"),
  Temporal.Instant.fromEpochMilliseconds(1.)->Temporal.Instant.epochNanoseconds,
  eq,
  1_000_000n,
)

Test.run(
  __POS_OF__("Date.toTemporalInstant"),
  Date.fromTime(1_000.)->Date.toTemporalInstant->Temporal.Instant.epochMilliseconds,
  eq,
  1_000.,
)

Test.run(
  __POS_OF__("Now.zonedDateTimeISO in a time zone"),
  Temporal.Now.zonedDateTimeISO(~timeZone="Asia/Seoul")->Temporal.ZonedDateTime.offset,
  eq,
  "+09:00",
)

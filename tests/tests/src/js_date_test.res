open Mocha
open Test_utils

module N = Js.Date

let date = () => N.fromString("1976-03-08T12:34:56.789+01:23")

describe(__MODULE__, () => {
  test("valueOf", () => eq(__LOC__, 195131516789., N.valueOf(date())))
  test("make", () => eq(__LOC__, true, N.getTime(N.make()) > 1487223505382.))
  test("parseAsFloat", () =>
    eq(__LOC__, N.parseAsFloat("1976-03-08T12:34:56.789+01:23"), 195131516789.)
  )
  test("parseAsFloat_invalid", () => eq(__LOC__, true, Js_float.isNaN(N.parseAsFloat("gibberish"))))
  test("fromFloat", () =>
    eq(__LOC__, "1976-03-08T11:11:56.789Z", N.toISOString(N.fromFloat(195131516789.)))
  )
  test("fromString_valid", () =>
    eq(__LOC__, 195131516789., N.getTime(N.fromString("1976-03-08T12:34:56.789+01:23")))
  )
  test("fromString_invalid", () =>
    eq(__LOC__, true, Js_float.isNaN(N.getTime(N.fromString("gibberish"))))
  )
  test("makeWithYM", () => {
    let d = N.makeWithYM(~year=1984., ~month=4., ())
    eq(__LOC__, (1984., 4.), (N.getFullYear(d), N.getMonth(d)))
  })
  test("makeWithYMD", () => {
    let d = N.makeWithYMD(~year=1984., ~month=4., ~date=6., ())
    eq(__LOC__, (1984., 4., 6.), (N.getFullYear(d), N.getMonth(d), N.getDate(d)))
  })
  test("makeWithYMDH", () => {
    let d = N.makeWithYMDH(~year=1984., ~month=4., ~date=6., ~hours=3., ())
    eq(__LOC__, (1984., 4., 6., 3.), (N.getFullYear(d), N.getMonth(d), N.getDate(d), N.getHours(d)))
  })
  test("makeWithYMDHM", () => {
    let d = N.makeWithYMDHM(~year=1984., ~month=4., ~date=6., ~hours=3., ~minutes=59., ())
    eq(
      __LOC__,
      (1984., 4., 6., 3., 59.),
      (N.getFullYear(d), N.getMonth(d), N.getDate(d), N.getHours(d), N.getMinutes(d)),
    )
  })
  test("makeWithYMDHMS", () => {
    let d = N.makeWithYMDHMS(
      ~year=1984.,
      ~month=4.,
      ~date=6.,
      ~hours=3.,
      ~minutes=59.,
      ~seconds=27.,
      (),
    )
    eq(
      __LOC__,
      (1984., 4., 6., 3., 59., 27.),
      (
        N.getFullYear(d),
        N.getMonth(d),
        N.getDate(d),
        N.getHours(d),
        N.getMinutes(d),
        N.getSeconds(d),
      ),
    )
  })
  test("utcWithYM", () => {
    let d = N.utcWithYM(~year=1984., ~month=4., ())
    let d = N.fromFloat(d)
    eq(__LOC__, (1984., 4.), (N.getUTCFullYear(d), N.getUTCMonth(d)))
  })
  test("utcWithYMD", () => {
    let d = N.utcWithYMD(~year=1984., ~month=4., ~date=6., ())
    let d = N.fromFloat(d)
    eq(__LOC__, (1984., 4., 6.), (N.getUTCFullYear(d), N.getUTCMonth(d), N.getUTCDate(d)))
  })
  test("utcWithYMDH", () => {
    let d = N.utcWithYMDH(~year=1984., ~month=4., ~date=6., ~hours=3., ())
    let d = N.fromFloat(d)
    eq(
      __LOC__,
      (1984., 4., 6., 3.),
      (N.getUTCFullYear(d), N.getUTCMonth(d), N.getUTCDate(d), N.getUTCHours(d)),
    )
  })
  test("utcWithYMDHM", () => {
    let d = N.utcWithYMDHM(~year=1984., ~month=4., ~date=6., ~hours=3., ~minutes=59., ())
    let d = N.fromFloat(d)
    eq(
      __LOC__,
      (1984., 4., 6., 3., 59.),
      (
        N.getUTCFullYear(d),
        N.getUTCMonth(d),
        N.getUTCDate(d),
        N.getUTCHours(d),
        N.getUTCMinutes(d),
      ),
    )
  })
  test("utcWithYMDHMS", () => {
    let d = N.utcWithYMDHMS(
      ~year=1984.,
      ~month=4.,
      ~date=6.,
      ~hours=3.,
      ~minutes=59.,
      ~seconds=27.,
      (),
    )
    let d = N.fromFloat(d)
    eq(
      __LOC__,
      (1984., 4., 6., 3., 59., 27.),
      (
        N.getUTCFullYear(d),
        N.getUTCMonth(d),
        N.getUTCDate(d),
        N.getUTCHours(d),
        N.getUTCMinutes(d),
        N.getUTCSeconds(d),
      ),
    )
  })
  test("getFullYear", () => eq(__LOC__, 1976., N.getFullYear(date())))
  test("getMilliseconds", () => eq(__LOC__, 789., N.getMilliseconds(date())))
  test("getSeconds", () => eq(__LOC__, 56., N.getSeconds(date())))
  test("getTime", () => eq(__LOC__, 195131516789., N.getTime(date())))
  test("getUTCDate", () => eq(__LOC__, 8., N.getUTCDate(date())))
  test("getUTCDay", () => eq(__LOC__, 1., N.getUTCDay(date())))
  test("getUTCFUllYear", () => eq(__LOC__, 1976., N.getUTCFullYear(date())))
  test("getUTCHours", () => eq(__LOC__, 11., N.getUTCHours(date())))
  test("getUTCMilliseconds", () => eq(__LOC__, 789., N.getUTCMilliseconds(date())))
  test("getUTCMinutes", () => eq(__LOC__, 11., N.getUTCMinutes(date())))
  test("getUTCMonth", () => eq(__LOC__, 2., N.getUTCMonth(date())))
  test("getUTCSeconds", () => eq(__LOC__, 56., N.getUTCSeconds(date())))
  test("getYear", () => eq(__LOC__, 1976., N.getFullYear(date())))
  test("setDate", () => {
    let d = date()
    let _ = N.setDate(d, 12.)
    eq(__LOC__, 12., N.getDate(d))
  })
  test("setFullYear", () => {
    let d = date()
    let _ = N.setFullYear(d, 1986.)
    eq(__LOC__, 1986., N.getFullYear(d))
  })
  test("setFullYearM", () => {
    let d = date()
    let _ = N.setFullYearM(d, ~year=1986., ~month=7., ())
    eq(__LOC__, (1986., 7.), (N.getFullYear(d), N.getMonth(d)))
  })
  test("setFullYearMD", () => {
    let d = date()
    let _ = N.setFullYearMD(d, ~year=1986., ~month=7., ~date=23., ())
    eq(__LOC__, (1986., 7., 23.), (N.getFullYear(d), N.getMonth(d), N.getDate(d)))
  })
  test("setHours", () => {
    let d = date()
    let _ = N.setHours(d, 22.)
    eq(__LOC__, 22., N.getHours(d))
  })
  test("setHoursM", () => {
    let d = date()
    let _ = N.setHoursM(d, ~hours=22., ~minutes=48., ())
    eq(__LOC__, (22., 48.), (N.getHours(d), N.getMinutes(d)))
  })
  test("setHoursMS", () => {
    let d = date()
    let _ = N.setHoursMS(d, ~hours=22., ~minutes=48., ~seconds=54., ())
    eq(__LOC__, (22., 48., 54.), (N.getHours(d), N.getMinutes(d), N.getSeconds(d)))
  })
  test("setMilliseconds", () => {
    let d = date()
    let _ = N.setMilliseconds(d, 543.)
    eq(__LOC__, 543., N.getMilliseconds(d))
  })
  test("setMinutes", () => {
    let d = date()
    let _ = N.setMinutes(d, 18.)
    eq(__LOC__, 18., N.getMinutes(d))
  })
  test("setMinutesS", () => {
    let d = date()
    let _ = N.setMinutesS(d, ~minutes=18., ~seconds=42., ())
    eq(__LOC__, (18., 42.), (N.getMinutes(d), N.getSeconds(d)))
  })
  test("setMinutesSMs", () => {
    let d = date()
    let _ = N.setMinutesSMs(d, ~minutes=18., ~seconds=42., ~milliseconds=311., ())
    eq(__LOC__, (18., 42., 311.), (N.getMinutes(d), N.getSeconds(d), N.getMilliseconds(d)))
  })
  test("setMonth", () => {
    let d = date()
    let _ = N.setMonth(d, 10.)
    eq(__LOC__, 10., N.getMonth(d))
  })
  test("setMonthD", () => {
    let d = date()
    let _ = N.setMonthD(d, ~month=10., ~date=14., ())
    eq(__LOC__, (10., 14.), (N.getMonth(d), N.getDate(d)))
  })
  test("setSeconds", () => {
    let d = date()
    let _ = N.setSeconds(d, 36.)
    eq(__LOC__, 36., N.getSeconds(d))
  })
  test("setSecondsMs", () => {
    let d = date()
    let _ = N.setSecondsMs(d, ~seconds=36., ~milliseconds=420., ())
    eq(__LOC__, (36., 420.), (N.getSeconds(d), N.getMilliseconds(d)))
  })
  test("setUTCDate", () => {
    let d = date()
    let _ = N.setUTCDate(d, 12.)
    eq(__LOC__, 12., N.getUTCDate(d))
  })
  test("setUTCFullYear", () => {
    let d = date()
    let _ = N.setUTCFullYear(d, 1986.)
    eq(__LOC__, 1986., N.getUTCFullYear(d))
  })
  test("setUTCFullYearM", () => {
    let d = date()
    let _ = N.setUTCFullYearM(d, ~year=1986., ~month=7., ())
    eq(__LOC__, (1986., 7.), (N.getUTCFullYear(d), N.getUTCMonth(d)))
  })
  test("setUTCFullYearMD", () => {
    let d = date()
    let _ = N.setUTCFullYearMD(d, ~year=1986., ~month=7., ~date=23., ())
    eq(__LOC__, (1986., 7., 23.), (N.getUTCFullYear(d), N.getUTCMonth(d), N.getUTCDate(d)))
  })
  test("setUTCHours", () => {
    let d = date()
    let _ = N.setUTCHours(d, 22.)
    eq(__LOC__, 22., N.getUTCHours(d))
  })
  test("setUTCHoursM", () => {
    let d = date()
    let _ = N.setUTCHoursM(d, ~hours=22., ~minutes=48., ())
    eq(__LOC__, (22., 48.), (N.getUTCHours(d), N.getUTCMinutes(d)))
  })
  test("setUTCHoursMS", () => {
    let d = date()
    let _ = N.setUTCHoursMS(d, ~hours=22., ~minutes=48., ~seconds=54., ())
    eq(__LOC__, (22., 48., 54.), (N.getUTCHours(d), N.getUTCMinutes(d), N.getUTCSeconds(d)))
  })
  test("setUTCMilliseconds", () => {
    let d = date()
    let _ = N.setUTCMilliseconds(d, 543.)
    eq(__LOC__, 543., N.getUTCMilliseconds(d))
  })
  test("setUTCMinutes", () => {
    let d = date()
    let _ = N.setUTCMinutes(d, 18.)
    eq(__LOC__, 18., N.getUTCMinutes(d))
  })
  test("setUTCMinutesS", () => {
    let d = date()
    let _ = N.setUTCMinutesS(d, ~minutes=18., ~seconds=42., ())
    eq(__LOC__, (18., 42.), (N.getUTCMinutes(d), N.getUTCSeconds(d)))
  })
  test("setUTCMinutesSMs", () => {
    let d = date()
    let _ = N.setUTCMinutesSMs(d, ~minutes=18., ~seconds=42., ~milliseconds=311., ())
    eq(__LOC__, (18., 42., 311.), (N.getUTCMinutes(d), N.getUTCSeconds(d), N.getUTCMilliseconds(d)))
  })
  test("setUTCMonth", () => {
    let d = date()
    let _ = N.setUTCMonth(d, 10.)
    eq(__LOC__, 10., N.getUTCMonth(d))
  })
  test("setUTCMonthD", () => {
    let d = date()
    let _ = N.setUTCMonthD(d, ~month=10., ~date=14., ())
    eq(__LOC__, (10., 14.), (N.getUTCMonth(d), N.getUTCDate(d)))
  })
  test("setUTCSeconds", () => {
    let d = date()
    let _ = N.setUTCSeconds(d, 36.)
    eq(__LOC__, 36., N.getUTCSeconds(d))
  })
  test("setUTCSecondsMs", () => {
    let d = date()
    let _ = N.setUTCSecondsMs(d, ~seconds=36., ~milliseconds=420., ())
    eq(__LOC__, (36., 420.), (N.getUTCSeconds(d), N.getUTCMilliseconds(d)))
  })
  test("toDateString", () => eq(__LOC__, "Mon Mar 08 1976", N.toDateString(date())))
  test("toGMTString", () => eq(__LOC__, "Mon, 08 Mar 1976 11:11:56 GMT", N.toUTCString(date())))
  test("toISOString", () => eq(__LOC__, "1976-03-08T11:11:56.789Z", N.toISOString(date())))
  test("toJSON", () => eq(__LOC__, "1976-03-08T11:11:56.789Z", N.toJSON(date())))
  test("toJSONUnsafe", () => eq(__LOC__, "1976-03-08T11:11:56.789Z", N.toJSONUnsafe(date())))
  test("toUTCString", () => eq(__LOC__, "Mon, 08 Mar 1976 11:11:56 GMT", N.toUTCString(date())))
  test("eq", () => {
    let a = Js.Date.fromString("2013-03-01T01:10:00")
    let b = Js.Date.fromString("2013-03-01T01:10:00")
    let c = Js.Date.fromString("2013-03-01T01:10:01")
    ok(__LOC__, a == b && (b != c && c > b))
  })
})

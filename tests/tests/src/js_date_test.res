module N = Js.Date

let date = () => N.fromString("1976-03-08T12:34:56.789+01:23")

let suites = {
  open Mt
  list{
    ("valueOf", _ => Eq(195131516789., N.valueOf(date()))),
    ("make", _ => Ok(N.getTime(N.make()) > 1487223505382.)),
    ("parseAsFloat", _ => Eq(N.parseAsFloat("1976-03-08T12:34:56.789+01:23"), 195131516789.)),
    ("parseAsFloat_invalid", _ => Ok(Js_float.isNaN(N.parseAsFloat("gibberish")))),
    ("fromFloat", _ => Eq("1976-03-08T11:11:56.789Z", N.toISOString(N.fromFloat(195131516789.)))),
    (
      "fromString_valid",
      _ => Eq(195131516789., N.getTime(N.fromString("1976-03-08T12:34:56.789+01:23"))),
    ),
    ("fromString_invalid", _ => Ok(Js_float.isNaN(N.getTime(N.fromString("gibberish"))))),
    (
      "makeWithYM",
      _ => {
        let d = N.makeWithYM(~year=1984., ~month=4., ())

        Eq((1984., 4.), (N.getFullYear(d), N.getMonth(d)))
      },
    ),
    (
      "makeWithYMD",
      _ => {
        let d = N.makeWithYMD(~year=1984., ~month=4., ~date=6., ())

        Eq((1984., 4., 6.), (N.getFullYear(d), N.getMonth(d), N.getDate(d)))
      },
    ),
    (
      "makeWithYMDH",
      _ => {
        let d = N.makeWithYMDH(~year=1984., ~month=4., ~date=6., ~hours=3., ())

        Eq((1984., 4., 6., 3.), (N.getFullYear(d), N.getMonth(d), N.getDate(d), N.getHours(d)))
      },
    ),
    (
      "makeWithYMDHM",
      _ => {
        let d = N.makeWithYMDHM(~year=1984., ~month=4., ~date=6., ~hours=3., ~minutes=59., ())

        Eq(
          (1984., 4., 6., 3., 59.),
          (N.getFullYear(d), N.getMonth(d), N.getDate(d), N.getHours(d), N.getMinutes(d)),
        )
      },
    ),
    (
      "makeWithYMDHMS",
      _ => {
        let d = N.makeWithYMDHMS(
          ~year=1984.,
          ~month=4.,
          ~date=6.,
          ~hours=3.,
          ~minutes=59.,
          ~seconds=27.,
          (),
        )

        Eq(
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
      },
    ),
    (
      "utcWithYM",
      _ => {
        let d = N.utcWithYM(~year=1984., ~month=4., ())
        let d = N.fromFloat(d)

        Eq((1984., 4.), (N.getUTCFullYear(d), N.getUTCMonth(d)))
      },
    ),
    (
      "utcWithYMD",
      _ => {
        let d = N.utcWithYMD(~year=1984., ~month=4., ~date=6., ())
        let d = N.fromFloat(d)

        Eq((1984., 4., 6.), (N.getUTCFullYear(d), N.getUTCMonth(d), N.getUTCDate(d)))
      },
    ),
    (
      "utcWithYMDH",
      _ => {
        let d = N.utcWithYMDH(~year=1984., ~month=4., ~date=6., ~hours=3., ())
        let d = N.fromFloat(d)

        Eq(
          (1984., 4., 6., 3.),
          (N.getUTCFullYear(d), N.getUTCMonth(d), N.getUTCDate(d), N.getUTCHours(d)),
        )
      },
    ),
    (
      "utcWithYMDHM",
      _ => {
        let d = N.utcWithYMDHM(~year=1984., ~month=4., ~date=6., ~hours=3., ~minutes=59., ())
        let d = N.fromFloat(d)

        Eq(
          (1984., 4., 6., 3., 59.),
          (
            N.getUTCFullYear(d),
            N.getUTCMonth(d),
            N.getUTCDate(d),
            N.getUTCHours(d),
            N.getUTCMinutes(d),
          ),
        )
      },
    ),
    (
      "utcWithYMDHMS",
      _ => {
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

        Eq(
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
      },
    ),
    /* locale dependent
    "getDate", (fun _ ->
      Eq(8., N.getDate (date ())));
    "getDay", (fun _ ->
      Eq(1., N.getDay (date ())));
 */
    ("getFullYear", _ => Eq(1976., N.getFullYear(date()))),
    /* locale dependent
    "getHours", (fun _ ->
      Eq(12., N.getHours (date ())));
 */
    ("getMilliseconds", _ => Eq(789., N.getMilliseconds(date()))),
    /* locale dependent
    "getMinutes", (fun _ ->
      Eq(11., N.getMinutes (date ())));
    "getMonth", (fun _ ->
      Eq(2., N.getMonth (date ())));
 */
    ("getSeconds", _ => Eq(56., N.getSeconds(date()))),
    ("getTime", _ => Eq(195131516789., N.getTime(date()))),
    /* locale depdendent
    "getTimezoneOffset", (fun _ ->
      Eq(-60., N.getTimezoneOffset (date ())));
 */
    ("getUTCDate", _ => Eq(8., N.getUTCDate(date()))),
    ("getUTCDay", _ => Eq(1., N.getUTCDay(date()))),
    ("getUTCFUllYear", _ => Eq(1976., N.getUTCFullYear(date()))),
    ("getUTCHours", _ => Eq(11., N.getUTCHours(date()))),
    ("getUTCMilliseconds", _ => Eq(789., N.getUTCMilliseconds(date()))),
    ("getUTCMinutes", _ => Eq(11., N.getUTCMinutes(date()))),
    ("getUTCMonth", _ => Eq(2., N.getUTCMonth(date()))),
    ("getUTCSeconds", _ => Eq(56., N.getUTCSeconds(date()))),
    ("getYear", _ => Eq(1976., N.getFullYear(date()))),
    (
      "setDate",
      _ => {
        let d = date()
        let _ = N.setDate(d, 12.)

        Eq(12., N.getDate(d))
      },
    ),
    (
      "setFullYear",
      _ => {
        let d = date()
        let _ = N.setFullYear(d, 1986.)

        Eq(1986., N.getFullYear(d))
      },
    ),
    (
      "setFullYearM",
      _ => {
        let d = date()
        let _ = N.setFullYearM(d, ~year=1986., ~month=7., ())
        Eq((1986., 7.), (N.getFullYear(d), N.getMonth(d)))
      },
    ),
    (
      "setFullYearMD",
      _ => {
        let d = date()
        let _ = N.setFullYearMD(d, ~year=1986., ~month=7., ~date=23., ())
        Eq((1986., 7., 23.), (N.getFullYear(d), N.getMonth(d), N.getDate(d)))
      },
    ),
    (
      "setHours",
      _ => {
        let d = date()
        let _ = N.setHours(d, 22.)

        Eq(22., N.getHours(d))
      },
    ),
    (
      "setHoursM",
      _ => {
        let d = date()
        let _ = N.setHoursM(d, ~hours=22., ~minutes=48., ())
        Eq((22., 48.), (N.getHours(d), N.getMinutes(d)))
      },
    ),
    (
      "setHoursMS",
      _ => {
        let d = date()
        let _ = N.setHoursMS(d, ~hours=22., ~minutes=48., ~seconds=54., ())
        Eq((22., 48., 54.), (N.getHours(d), N.getMinutes(d), N.getSeconds(d)))
      },
    ),
    (
      "setMilliseconds",
      _ => {
        let d = date()
        let _ = N.setMilliseconds(d, 543.)

        Eq(543., N.getMilliseconds(d))
      },
    ),
    (
      "setMinutes",
      _ => {
        let d = date()
        let _ = N.setMinutes(d, 18.)

        Eq(18., N.getMinutes(d))
      },
    ),
    (
      "setMinutesS",
      _ => {
        let d = date()
        let _ = N.setMinutesS(d, ~minutes=18., ~seconds=42., ())
        Eq((18., 42.), (N.getMinutes(d), N.getSeconds(d)))
      },
    ),
    (
      "setMinutesSMs",
      _ => {
        let d = date()
        let _ = N.setMinutesSMs(d, ~minutes=18., ~seconds=42., ~milliseconds=311., ())
        Eq((18., 42., 311.), (N.getMinutes(d), N.getSeconds(d), N.getMilliseconds(d)))
      },
    ),
    (
      "setMonth",
      _ => {
        let d = date()
        let _ = N.setMonth(d, 10.)

        Eq(10., N.getMonth(d))
      },
    ),
    (
      "setMonthD",
      _ => {
        let d = date()
        let _ = N.setMonthD(d, ~month=10., ~date=14., ())
        Eq((10., 14.), (N.getMonth(d), N.getDate(d)))
      },
    ),
    (
      "setSeconds",
      _ => {
        let d = date()
        let _ = N.setSeconds(d, 36.)

        Eq(36., N.getSeconds(d))
      },
    ),
    (
      "setSecondsMs",
      _ => {
        let d = date()
        let _ = N.setSecondsMs(d, ~seconds=36., ~milliseconds=420., ())
        Eq((36., 420.), (N.getSeconds(d), N.getMilliseconds(d)))
      },
    ),
    (
      "setUTCDate",
      _ => {
        let d = date()
        let _ = N.setUTCDate(d, 12.)

        Eq(12., N.getUTCDate(d))
      },
    ),
    (
      "setUTCFullYear",
      _ => {
        let d = date()
        let _ = N.setUTCFullYear(d, 1986.)

        Eq(1986., N.getUTCFullYear(d))
      },
    ),
    (
      "setUTCFullYearM",
      _ => {
        let d = date()
        let _ = N.setUTCFullYearM(d, ~year=1986., ~month=7., ())
        Eq((1986., 7.), (N.getUTCFullYear(d), N.getUTCMonth(d)))
      },
    ),
    (
      "setUTCFullYearMD",
      _ => {
        let d = date()
        let _ = N.setUTCFullYearMD(d, ~year=1986., ~month=7., ~date=23., ())
        Eq((1986., 7., 23.), (N.getUTCFullYear(d), N.getUTCMonth(d), N.getUTCDate(d)))
      },
    ),
    (
      "setUTCHours",
      _ => {
        let d = date()
        let _ = N.setUTCHours(d, 22.)

        Eq(22., N.getUTCHours(d))
      },
    ),
    (
      "setUTCHoursM",
      _ => {
        let d = date()
        let _ = N.setUTCHoursM(d, ~hours=22., ~minutes=48., ())
        Eq((22., 48.), (N.getUTCHours(d), N.getUTCMinutes(d)))
      },
    ),
    (
      "setUTCHoursMS",
      _ => {
        let d = date()
        let _ = N.setUTCHoursMS(d, ~hours=22., ~minutes=48., ~seconds=54., ())
        Eq((22., 48., 54.), (N.getUTCHours(d), N.getUTCMinutes(d), N.getUTCSeconds(d)))
      },
    ),
    (
      "setUTCMilliseconds",
      _ => {
        let d = date()
        let _ = N.setUTCMilliseconds(d, 543.)

        Eq(543., N.getUTCMilliseconds(d))
      },
    ),
    (
      "setUTCMinutes",
      _ => {
        let d = date()
        let _ = N.setUTCMinutes(d, 18.)

        Eq(18., N.getUTCMinutes(d))
      },
    ),
    (
      "setUTCMinutesS",
      _ => {
        let d = date()
        let _ = N.setUTCMinutesS(d, ~minutes=18., ~seconds=42., ())
        Eq((18., 42.), (N.getUTCMinutes(d), N.getUTCSeconds(d)))
      },
    ),
    (
      "setUTCMinutesSMs",
      _ => {
        let d = date()
        let _ = N.setUTCMinutesSMs(d, ~minutes=18., ~seconds=42., ~milliseconds=311., ())
        Eq((18., 42., 311.), (N.getUTCMinutes(d), N.getUTCSeconds(d), N.getUTCMilliseconds(d)))
      },
    ),
    (
      "setUTCMonth",
      _ => {
        let d = date()
        let _ = N.setUTCMonth(d, 10.)

        Eq(10., N.getUTCMonth(d))
      },
    ),
    (
      "setUTCMonthD",
      _ => {
        let d = date()
        let _ = N.setUTCMonthD(d, ~month=10., ~date=14., ())
        Eq((10., 14.), (N.getUTCMonth(d), N.getUTCDate(d)))
      },
    ),
    (
      "setUTCSeconds",
      _ => {
        let d = date()
        let _ = N.setUTCSeconds(d, 36.)

        Eq(36., N.getUTCSeconds(d))
      },
    ),
    (
      "setUTCSecondsMs",
      _ => {
        let d = date()
        let _ = N.setUTCSecondsMs(d, ~seconds=36., ~milliseconds=420., ())
        Eq((36., 420.), (N.getUTCSeconds(d), N.getUTCMilliseconds(d)))
      },
    ),
    ("toDateString", _ => Eq("Mon Mar 08 1976", N.toDateString(date()))),
    ("toGMTString", _ => Eq("Mon, 08 Mar 1976 11:11:56 GMT", N.toUTCString(date()))),
    ("toISOString", _ => Eq("1976-03-08T11:11:56.789Z", N.toISOString(date()))),
    ("toJSON", _ => Eq("1976-03-08T11:11:56.789Z", N.toJSON(date()))),
    ("toJSONUnsafe", _ => Eq("1976-03-08T11:11:56.789Z", N.toJSONUnsafe(date()))),
    /* locale dependent
    "toLocaleDateString", (fun _ ->
      Eq("3/8/1976", N.toLocaleDateString (date ())));
    "toLocaleString", (fun _ ->
      Eq("3/8/1976, 12:11:56 PM", N.toLocaleString (date ())));
    "toLocaleTimeString", (fun _ ->
      Eq("12:11:56 PM", N.toLocaleTimeString (date ())));
    "toString", (fun _ ->
      Eq("Mon Mar 08 1976 12:11:56 GMT+0100 (CET)", N.toString (date ())));
    "toTimeString", (fun _ ->
      Eq("12:11:56 GMT+0100 (CET)", N.toTimeString (date ())));
 */
    ("toUTCString", _ => Eq("Mon, 08 Mar 1976 11:11:56 GMT", N.toUTCString(date()))),
    (
      "eq",
      _ => {
        let a = Js.Date.fromString("2013-03-01T01:10:00")
        let b = Js.Date.fromString("2013-03-01T01:10:00")
        let c = Js.Date.fromString("2013-03-01T01:10:01")
        Ok(a == b && (b != c && c > b))
      },
    ),
  }
}

Mt.from_pair_suites(__MODULE__, suites)

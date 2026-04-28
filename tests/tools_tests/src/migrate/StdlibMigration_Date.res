let d1 = Js.Date.make()
let d2 = Js.Date.fromString("1973-11-29T21:30:54.321Z")
let d3 = Js.Date.fromFloat(123456789.0)

let msNow = Js.Date.now()

let v1 = d2->Js.Date.valueOf
let v2 = Js.Date.valueOf(d2)

let y = d2->Js.Date.getFullYear
let mo = d2->Js.Date.getMonth
let dayOfMonth = d2->Js.Date.getDate
let dayOfWeek = d2->Js.Date.getDay
let h = d2->Js.Date.getHours
let mi = d2->Js.Date.getMinutes
let s = d2->Js.Date.getSeconds
let ms = d2->Js.Date.getMilliseconds
let tz = d2->Js.Date.getTimezoneOffset

let uy = d2->Js.Date.getUTCFullYear
let um = d2->Js.Date.getUTCMonth
let ud = d2->Js.Date.getUTCDate
let uday = d2->Js.Date.getUTCDay
let uh = d2->Js.Date.getUTCHours
let umi = d2->Js.Date.getUTCMinutes
let us = d2->Js.Date.getUTCSeconds
let ums = d2->Js.Date.getUTCMilliseconds

let s1 = d2->Js.Date.toISOString
let s2 = d2->Js.Date.toUTCString
let s3 = d2->Js.Date.toString
let s4 = d2->Js.Date.toTimeString
let s5 = d2->Js.Date.toDateString
let s6 = d2->Js.Date.toLocaleString
let s7 = d2->Js.Date.toLocaleDateString
let s8 = d2->Js.Date.toLocaleTimeString

/* Additional deprecated APIs to exercise migration */

/* getters and legacy variants */
let t = d2->Js.Date.getTime
let y2 = d2->Js.Date.getYear

/* constructors with components */
let mym = Js.Date.makeWithYM(~year=2020.0, ~month=10.0, ())
let mymd = Js.Date.makeWithYMD(~year=1973.0, ~month=10.0, ~date=29.0, ())
let mymdh = Js.Date.makeWithYMDH(~year=1973.0, ~month=10.0, ~date=29.0, ~hours=21.0, ())
let mymdhm = Js.Date.makeWithYMDHM(
  ~year=1973.0,
  ~month=10.0,
  ~date=29.0,
  ~hours=21.0,
  ~minutes=30.0,
  (),
)
let mymdhms = Js.Date.makeWithYMDHMS(
  ~year=1973.0,
  ~month=10.0,
  ~date=29.0,
  ~hours=21.0,
  ~minutes=30.0,
  ~seconds=54.0,
  (),
)

/* Date.UTC variants */
let uym = Js.Date.utcWithYM(~year=2020.0, ~month=10.0, ())
let uymd = Js.Date.utcWithYMD(~year=1973.0, ~month=10.0, ~date=29.0, ())
let uymdh = Js.Date.utcWithYMDH(~year=1973.0, ~month=10.0, ~date=29.0, ~hours=21.0, ())
let uymdhm = Js.Date.utcWithYMDHM(
  ~year=1973.0,
  ~month=10.0,
  ~date=29.0,
  ~hours=21.0,
  ~minutes=30.0,
  (),
)
let uymdhms = Js.Date.utcWithYMDHMS(
  ~year=1973.0,
  ~month=10.0,
  ~date=29.0,
  ~hours=21.0,
  ~minutes=30.0,
  ~seconds=54.0,
  (),
)

/* parse APIs */
let p = Js.Date.parse("1973-11-29T21:30:54.321Z")
let pf = Js.Date.parseAsFloat("1973-11-29T21:30:54.321Z")

/* setters (local time) */
let setD = d2->Js.Date.setDate(15.0)
let setFY = d2->Js.Date.setFullYear(1974.0)

let setFYM = d2->Js.Date.setFullYearM(~year=1974.0, ~month=0.0, ())
let setFYMD = d2->Js.Date.setFullYearMD(~year=1974.0, ~month=0.0, ~date=7.0, ())
let setH = d2->Js.Date.setHours(22.0)
let setHM = d2->Js.Date.setHoursM(~hours=22.0, ~minutes=46.0, ())
let setHMS = d2->Js.Date.setHoursMS(~hours=22.0, ~minutes=46.0, ~seconds=37.0, ())
let setHMSMs =
  d2->Js.Date.setHoursMSMs(~hours=22.0, ~minutes=46.0, ~seconds=37.0, ~milliseconds=494.0, ())
let setMs = d2->Js.Date.setMilliseconds(494.0)
let setMin = d2->Js.Date.setMinutes(34.0)
let setMinS = d2->Js.Date.setMinutesS(~minutes=34.0, ~seconds=56.0, ())
let setMinSMs = d2->Js.Date.setMinutesSMs(~minutes=34.0, ~seconds=56.0, ~milliseconds=789.0, ())
let setMon = d2->Js.Date.setMonth(11.0)
let setMonD = d2->Js.Date.setMonthD(~month=11.0, ~date=8.0, ())
let setSec = d2->Js.Date.setSeconds(56.0)
let setSecMs = d2->Js.Date.setSecondsMs(~seconds=56.0, ~milliseconds=789.0, ())

/* setters (UTC) */
let setUD = d2->Js.Date.setUTCDate(15.0)
let setUFY = d2->Js.Date.setUTCFullYear(1974.0)
let setUFYM = d2->Js.Date.setUTCFullYearM(~year=1974.0, ~month=0.0, ())
let setUFYMD = d2->Js.Date.setUTCFullYearMD(~year=1974.0, ~month=0.0, ~date=7.0, ())
let setUH = d2->Js.Date.setUTCHours(22.0)
let setUHM = d2->Js.Date.setUTCHoursM(~hours=22.0, ~minutes=46.0, ())
let setUHMS = d2->Js.Date.setUTCHoursMS(~hours=22.0, ~minutes=46.0, ~seconds=37.0, ())
let setUHMSMs =
  d2->Js.Date.setUTCHoursMSMs(~hours=22.0, ~minutes=46.0, ~seconds=37.0, ~milliseconds=494.0, ())
let setUMs = d2->Js.Date.setUTCMilliseconds(494.0)
let setUMin = d2->Js.Date.setUTCMinutes(34.0)
let setUMinS = d2->Js.Date.setUTCMinutesS(~minutes=34.0, ~seconds=56.0, ())
let setUMinSMs = d2->Js.Date.setUTCMinutesSMs(~minutes=34.0, ~seconds=56.0, ~milliseconds=789.0, ())
let setUMon = d2->Js.Date.setUTCMonth(11.0)
let setUMonD = d2->Js.Date.setUTCMonthD(~month=11.0, ~date=8.0, ())
let setUSec = d2->Js.Date.setUTCSeconds(56.0)
let setUSecMs = d2->Js.Date.setUTCSecondsMs(~seconds=56.0, ~milliseconds=789.0, ())
let setUT = d2->Js.Date.setUTCTime(198765432101.0)
let setYr = d2->Js.Date.setYear(1999.0)

/* other string conversions */
let s9 = d2->Js.Date.toGMTString
let j1 = d2->Js.Date.toJSON
let j2 = d2->Js.Date.toJSONUnsafe

// Type alias migration
external someDate: Js.Date.t = "someDate"

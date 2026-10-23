module Map: {
  type t<'key, 'value> constraint 'key = string

  let keys: t<'key, 'value> => array<'key>
  let get: (t<'key, 'value>, 'key) => 'value
  let get_opt: (t<'key, 'value>, 'key) => option<'value>
  let map: (('value) => 'b, t<'key, 'value>) => t<'key, 'b>
  let entries: t<'key, 'value> => array<('key, 'value)>
  let fromArray: array<('key, 'value)> => t<'key, 'value>
  let set: (t<'key, 'value>, 'key, 'value) => unit
  let empty: unit => t<'key, 'value>
} = {
  type t<'key, 'value> = dict<'value> constraint 'key = string
  let keys = Dict.keysToArray
  let get = Dict.getUnsafe
  let get_opt = Dict.get
  let map = Dict.mapValues
  let entries = Dict.toArray
  let fromArray = Dict.fromArray
  let set = Dict.set
  let empty = Dict.make
}

@val external require: string => 'a = "require"

type countryId = string
type location = {
  country: string,
  provinceOrState: option<string>,
  name: string,
}
type day = string
type record = {
  confirmed: int,
  deaths: int,
}
type dataPoints = Map.t<day, record>

let locations: Map.t<countryId, location> = require("../data/locations.json")
let days: array<day> = require("../data/days.json")
let data: Map.t<countryId, dataPoints> = require("../data/data.json")
let countryIds = Map.keys(locations)

let startDate = Date.fromString(days[0])
let endDate = Date.fromString(days[Array.length(days) - 1])

let dayToIndex = Array.mapWithIndex(days, (day, index) => (day, index))->Map.fromArray

type xValue =
  | Date(Date.t)
  | Day(int)

type value =
  | First(record)
  | Pair({prevRecord: record, record: record})

let dataWithGrowth =
  Map.entries(data)
  ->Array.map(((countryId, dataPoints)) => {
    let data = Lazy.from_fun(() => {
      let countryDataWithGrowth = Map.empty()
      let _ = Array.reduce(days, None, (prevRecord, day) => {
        let record = Map.get(dataPoints, day)
        Map.set(
          countryDataWithGrowth,
          day,
          switch prevRecord {
          | None => First(record)
          | Some(x) => Pair({prevRecord: x, record: record})
          },
        )
        Some(record)
      })
      countryDataWithGrowth
    })
    (countryId, data)
  })
  ->Belt.Map.String.fromArray

type item = {
  x: xValue,
  index: int,
  values: countryId => option<value>,
}

type t = array<item>

let calendar: t = Array.mapWithIndex(days, (day, index) => {
  let values = Belt.HashMap.String.make(~hintSize=Array.length(countryIds))
  Array.forEach(countryIds, countryId =>
      Belt.HashMap.String.set(
        values,
        Map.get(locations, countryId).name,
        Lazy.from_fun(() => Map.get(Belt.Map.String.getExn(dataWithGrowth, countryId)->Lazy.force, day)),
      ))
  {
    x: Date(Date.fromString(day)),
    index: index,
    values: countryId =>
      Belt.HashMap.String.get(values, countryId)->Option.map((x) => Lazy.force(x)),
  }
})

let isInitialRange = (selectedStartDate, selectedEndDate) =>
  Date.getTime(selectedEndDate) == Date.getTime(endDate) &&
    Date.getDate(selectedStartDate) == Date.getTime(startDate)

let calendar = (selectedStartDate, selectedEndDate) =>
  if isInitialRange(selectedStartDate, selectedEndDate) {
    calendar
  } else {
    Array.filter(calendar, ({x}) =>
      switch x {
      | Date(date) => date >= selectedStartDate && date <= selectedEndDate
      | _ => false
      })
  }

type dataType =
  | Confirmed
  | Deaths

let getRecord = x =>
  switch x {
  | First(value) => value
  | Pair({record}) => record
  }

let getValueFromRecord = (dataType, record) =>
  switch dataType {
  | Deaths => record.deaths
  | Confirmed => record.confirmed
  }

let getValue = (dataType, dataItem) => getValueFromRecord(dataType, getRecord(dataItem))

let alignToDay0 = (dataType, threshold) => {
  let data = Belt.Map.String.mapU(dataWithGrowth, (dataPoints) =>
    Lazy.from_fun(() => {
      let dataPoints = Lazy.force(dataPoints)
      Map.entries(dataPoints)
      ->Array.map(((date, value)) => (Map.get(dayToIndex, date), value))
      ->Array.toSorted((a, b) => Ordering.fromInt(((a, b) => compare(a->fst, b->fst))(a, b)))
      ->Array.map(((_, value)) => value)
      ->Array.filter(value => getValue(dataType, value) >= threshold)
      ->Array.mapWithIndex((value, index) => (index, value))
      ->Belt.Map.Int.fromArray
    })
  )

  Array.init(Array.length(days), day => {
    x: Day(day),
    index: day,
    values: countryId =>
      Belt.Map.String.get(data, countryId)->Option.andThen((countryData) =>
        Belt.Map.Int.get(Lazy.force(countryData), day)
      ),
  })
}

let getGrowth = (dataType, x) =>
  switch x {
  | First(_) => 0.
  | Pair({prevRecord, record}) =>
    let numberOfCasesF = Int.toFloat(getValueFromRecord(dataType, record))
    let prevNumberOfCases = getValueFromRecord(dataType, prevRecord)
    let prevNumberOfCasesF = Int.toFloat(prevNumberOfCases)
    prevNumberOfCases == 0 ? 0. : numberOfCasesF /. prevNumberOfCasesF -. 1.
  }

let getTotalMortailityRate = x =>
  switch x {
  | First({confirmed, deaths}) if confirmed > 0 =>
    Int.toFloat(deaths) /. Int.toFloat(confirmed)
  | Pair({record: {confirmed, deaths}}) if confirmed > 0 =>
    Int.toFloat(deaths) /. Int.toFloat(confirmed)
  | _ => 0.
  }

let getDailyNewCases = x =>
  switch x {
  | First(ret) => ret
  | Pair({prevRecord, record}) =>
    let confirmed = record.confirmed - prevRecord.confirmed
    let deaths = record.deaths - prevRecord.deaths
    {confirmed: confirmed, deaths: deaths}
  }

let getDailyMortailityRate = x => {
  let {confirmed, deaths} = getDailyNewCases(x)
  if confirmed > 0 {
    Int.toFloat(deaths) /. Int.toFloat(confirmed)
  } else {
    0.
  }
}

/*
 * let allLocations =
 *   Map.entries(locations)
 *   ->Array.map(((locationId, value)) =>
 *        {ReactSelect.label: value.name, value: locationId}
 *);
 */

/* Workaround Datepicker bug/feature.
   This file is loaded before filters :/.
   https://github.com/inspect-js/has-symbols/issues/6
 */
Window.window.global = Window.window

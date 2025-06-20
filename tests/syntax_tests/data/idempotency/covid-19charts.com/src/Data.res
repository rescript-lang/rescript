module Map: {
  type t<'key, 'value> constraint 'key = string

  let keys: t<'key, 'value> => array<'key>
  let get: (t<'key, 'value>, 'key) => 'value
  let get_opt: (t<'key, 'value>, 'key) => option<'value>
  let map: ((. 'value) => 'b, t<'key, 'value>) => t<'key, 'b>
  let entries: t<'key, 'value> => array<('key, 'value)>
  let fromArray: array<('key, 'value)> => t<'key, 'value>
  let set: (t<'key, 'value>, 'key, 'value) => unit
  let empty: unit => t<'key, 'value>
} = {
  type t<'key, 'value> = dict<'value> constraint 'key = string
  let keys = Js.Dict.keys
  let get = Js.Dict.unsafeGet
  let get_opt = Js.Dict.get
  let map = Js.Dict.map
  let entries = Js.Dict.entries
  let fromArray = Js.Dict.fromArray
  let set = Js.Dict.set
  let empty = Js.Dict.empty
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

let startDate = Js.Date.fromString(days[0])
let endDate = Js.Date.fromString(days[Js.Array.length(days) - 1])

let dayToIndex = Js.Array.mapi((day, index) => (day, index), days)->Map.fromArray

type xValue =
  | Date(Js.Date.t)
  | Day(int)

type value =
  | First(record)
  | Pair({prevRecord: record, record: record})

let dataWithGrowth =
  Map.entries(data)
  ->Js.Array.map(((countryId, dataPoints)) => {
    let data = Lazy.from_fun(() => {
      let countryDataWithGrowth = Map.empty()
      let _ = Js.Array.reduce((prevRecord, day) => {
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
      }, None, days)
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

let calendar: t = Js.Array.mapi((day, index) => {
  let values = Belt.HashMap.String.make(~hintSize=Js.Array.length(countryIds))
  Js.Array.forEach(
    countryId =>
      Belt.HashMap.String.set(
        values,
        Map.get(locations, countryId).name,
        Lazy.from_fun(() => Map.get(Belt.Map.String.getExn(dataWithGrowth, countryId)->Lazy.force, day)),
      ),
    countryIds,
  )
  {
    x: Date(Js.Date.fromString(day)),
    index: index,
    values: countryId =>
      Belt.HashMap.String.get(values, countryId)->Js.Option.map((. x) => Lazy.force(x)),
  }
}, days)

let isInitialRange = (selectedStartDate, selectedEndDate) =>
  Js.Date.getTime(selectedEndDate) == Js.Date.getTime(endDate) &&
    Js.Date.getDate(selectedStartDate) == Js.Date.getTime(startDate)

let calendar = (selectedStartDate, selectedEndDate) =>
  if isInitialRange(selectedStartDate, selectedEndDate) {
    calendar
  } else {
    Js.Array.filter(({x}) =>
      switch x {
      | Date(date) => date >= selectedStartDate && date <= selectedEndDate
      | _ => false
      }
    , calendar)
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
  let data = Belt.Map.String.mapU(dataWithGrowth, (. dataPoints) =>
    Lazy.from_fun(() => {
      let dataPoints = Lazy.force(dataPoints)
      Map.entries(dataPoints)
      ->Js.Array.map(((date, value)) => (Map.get(dayToIndex, date), value))
      ->Js.Array.sortInPlaceWith((a, b) => compare(a->fst, b->fst))
      ->Js.Array.map(((_, value)) => value)
      ->Js.Array.filter(value => getValue(dataType, value) >= threshold)
      ->Js.Array.mapi((value, index) => (index, value))
      ->Belt.Map.Int.fromArray
    })
  )

  Array.init(Js.Array.length(days), day => {
    x: Day(day),
    index: day,
    values: countryId =>
      Belt.Map.String.get(data, countryId)->Js.Option.andThen((. countryData) =>
        Belt.Map.Int.get(Lazy.force(countryData), day)
      ),
  })
}

let getGrowth = (dataType, x) =>
  switch x {
  | First(_) => 0.
  | Pair({prevRecord, record}) =>
    let numberOfCasesF = Js.Int.toFloat(getValueFromRecord(dataType, record))
    let prevNumberOfCases = getValueFromRecord(dataType, prevRecord)
    let prevNumberOfCasesF = Js.Int.toFloat(prevNumberOfCases)
    prevNumberOfCases == 0 ? 0. : numberOfCasesF /. prevNumberOfCasesF -. 1.
  }

let getTotalMortailityRate = x =>
  switch x {
  | First({confirmed, deaths}) if confirmed > 0 =>
    Js.Int.toFloat(deaths) /. Js.Int.toFloat(confirmed)
  | Pair({record: {confirmed, deaths}}) if confirmed > 0 =>
    Js.Int.toFloat(deaths) /. Js.Int.toFloat(confirmed)
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
    Js.Int.toFloat(deaths) /. Js.Int.toFloat(confirmed)
  } else {
    0.
  }
}

/*
 * let allLocations =
 *   Map.entries(locations)
 *   ->Js.Array.map(((locationId, value)) =>
 *        {ReactSelect.label: value.name, value: locationId}
 *      );
 */

/* Workaround Datepicker bug/feature.
   This file is loaded before filters :/.
   https://github.com/inspect-js/has-symbols/issues/6
 */
Window.window.global = Window.window

let pathToConfirmedDataCsv = Sys.argv[1]
let pathToDeathsDataCsv = Sys.argv[2]

let csv = Csv.load(pathToConfirmedDataCsv)

let createId = (country, provinceOrState) =>
  if String.length(provinceOrState) === 0 {
    country
  } else {
    country ++ (" (" ++ (provinceOrState ++ ")"))
  }

module StringSet = Set.Make(String)

let locationRows = Csv.sub(~r=1, ~c=0, ~cols=2, ~rows=Csv.lines(csv) - 1, csv)->Csv.to_array
let duplicateCountries = Array.fold_left(((countries, duplicateCountries), x) =>
  switch x {
  | [provinceOrState, country] =>
    if StringSet.mem(country, countries) {
      (countries, StringSet.add(country, duplicateCountries))
    } else {
      (StringSet.add(country, countries), duplicateCountries)
    }
  | _ => invalid_arg("Unknown format")
  }
, (StringSet.empty, StringSet.empty), locationRows)->snd

let locations = \"@"(
  duplicateCountries
  ->StringSet.elements
  ->List.map(country => {
    let id = country ++ " (All regions)"
    (id, #Assoc(list{("country", #String(country)), ("name", #String(id))}))
  }),
  locationRows
  ->Array.map(x =>
    switch x {
    | [provinceOrState, country] if String.length(provinceOrState) === 0 =>
      let id = createId(country, provinceOrState)
      (id, #Assoc(list{("country", #String(country)), ("name", #String(id))}))
    | [provinceOrState, country] =>
      let id = createId(country, provinceOrState)
      (
        id,
        #Assoc(list{
          ("country", #String(country)),
          ("provinceOrState", #String(provinceOrState)),
          ("name", #String(id)),
        }),
      )
    | _ => invalid_arg("Unknown format")
    }
  )
  ->Array.to_list,
)

Yojson.Basic.to_file(Sys.argv[3], #Assoc(locations))

let days =
  (
    Csv.sub(~r=0, ~c=4, ~cols=Csv.columns(csv) - 4, ~rows=1, csv)->Csv.to_array
  )[0]->Array.map(x => #String(x))

Yojson.Basic.to_file(Sys.argv[4], #List(days->Array.to_list))

module LocationMap = Map.Make(String)
module DayMap = LocationMap

let casesListToMap = list => list->List.fold_left((countryMap, row) =>
    switch Csv.Row.to_assoc(row) {
    | list{("Province/State", provinceOrState), ("Country/Region", country), _lat, _lon, ...rest} =>
      let currentCountryMap = LocationMap.find_opt(country, countryMap)->(
        x =>
          switch x {
          | Some(x) => x
          | None => LocationMap.empty
          }
      )
      LocationMap.add(
        country,
        LocationMap.add(
          provinceOrState,
          List.fold_left(
            (days, (key, value)) => DayMap.add(key, value->int_of_string, days),
            DayMap.empty,
            rest,
          ),
          currentCountryMap,
        ),
        countryMap,
      )
    | _ => invalid_arg("")
    }
  , LocationMap.empty)

type record = {
  deaths: int,
  confirmed: int,
}

let getWithDefault = (default, x) =>
  switch x {
  | Some(x) => x
  | None => default
  }

let data = {
  let confirmed = Csv.Rows.load(~has_header=true, pathToConfirmedDataCsv)->casesListToMap
  let deaths = Csv.Rows.load(~has_header=true, pathToDeathsDataCsv)->casesListToMap

  let merged = LocationMap.merge((countryId, confirmed, deaths) => {
    let confirmed = getWithDefault(LocationMap.empty, confirmed)
    let deaths = getWithDefault(LocationMap.empty, deaths)
    Some(LocationMap.merge((regionId, confirmed, deaths) => {
        let confirmed = getWithDefault(DayMap.empty, confirmed)
        let deaths = getWithDefault(DayMap.empty, deaths)
        Some(DayMap.merge((day, confirmed, deaths) => Some({
            deaths: getWithDefault(0, deaths),
            confirmed: getWithDefault(0, confirmed),
          }), confirmed, deaths))
      }, confirmed, deaths))
  }, confirmed, deaths)

  let allRegionsPerCountry = {
    let nullRecord = {confirmed: 0, deaths: 0}

    List.fold_left((countries, countryId) => {
      let allRegions = LocationMap.find(countryId, merged)
      let days = LocationMap.fold(_regionId =>
        DayMap.merge((day, all, region) => {
          let all = getWithDefault(nullRecord, all)
          let region = getWithDefault(nullRecord, region)

          Some({
            confirmed: all.confirmed + region.confirmed,
            deaths: all.deaths + region.deaths,
          })
        })
      , allRegions, DayMap.empty)

      let id = countryId ++ " (All regions)"
      LocationMap.add(id, days, countries)
    }, LocationMap.empty, duplicateCountries->StringSet.elements)
  }

  LocationMap.fold(
    (countryId, regions, locations): LocationMap.t<DayMap.t<record>> =>
      LocationMap.fold(
        (regionId, days, allRegions) =>
          LocationMap.add(createId(countryId, regionId), days, allRegions),
        regions,
        locations,
      ),
    merged,
    allRegionsPerCountry,
  )
  ->LocationMap.bindings
  ->List.map(((locationId, dayMap)) => (
    locationId,
    #Assoc(
      dayMap
      ->DayMap.bindings
      ->List.map(((day, {confirmed, deaths})) => (
        day,
        #Assoc(list{("confirmed", #Int(confirmed)), ("deaths", #Int(deaths))}),
      )),
    ),
  ))
}

Yojson.Basic.to_file(Sys.argv[5], #Assoc(data))

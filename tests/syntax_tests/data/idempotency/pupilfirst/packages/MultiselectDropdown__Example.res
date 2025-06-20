let str = React.string

module DetailedExample = {
  module Selectable = {
    type rec t =
      | City(name)
      | State(name)
      | Country(name)
      | Search(string)
    and name = string

    let label = t => {
      let labelString = switch t {
      | City(_) => "City"
      | State(_) => "State"
      | Country(_) => "Country"
      | Search(_) => "Search"
      }
      Some(labelString)
    }

    let value = t =>
      switch t {
      | City(name) => name
      | State(name) => name
      | Country(name) => name
      | Search(input) => input
      }

    let searchString = t =>
      switch t {
      | City(name) => "city " ++ name
      | State(name) => "state" ++ name
      | Country(name) => "country" ++ name
      | Search(input) => input
      }

    let color = t =>
      switch t {
      | City(_) => "orange"
      | State(_) => "green"
      | Country(_) => "blue"
      | Search(_) => "purple"
      }

    let makeCity = name => City(name)
    let makeState = name => State(name)
    let makeCountry = name => Country(name)
    let makeSearch = input => Search(input)
  }

  module Multiselect = MultiselectDropdown.Make(Selectable)

  type state = {
    searchInput: string,
    selected: array<Selectable.t>,
  }

  let unselected = searchInput => {
    let citySuggestions =
      [
        "Chicago",
        "San Francisco",
        "Los Angeles",
        "Busan",
        "Jerusalem",
        "Bangalore",
        "Cochin",
        "Chennai",
      ]->Array.map(t => Selectable.makeCity(t))

    let stateSuggestions =
      [
        "Washington",
        "California",
        "Mississippi",
        "Kuala Lumpur",
        "Kerala",
        "Karnataka",
        "Tamil Nadu",
      ]->Array.map(l => Selectable.makeState(l))

    let countrySuggestions =
      ["India", "USA", "Canada", "China", "Japan", "Egypt", "Korea"]->Array.map(l =>
        Selectable.makeCountry(l)
      )

    let searchSuggestion =
      searchInput->Js.String.trim == "" ? [] : [Selectable.makeSearch(searchInput)]

    searchSuggestion
    ->Array.append(citySuggestions)
    ->Array.append(stateSuggestions)
    ->Array.append(countrySuggestions)
  }

  let select = (setState, selectable) =>
    setState(s => {
      searchInput: "",
      selected: [selectable]->Array.append(s.selected),
    })

  let deselect = (selected, setState, selectable) => {
    let newSelected = selected->Js.Array.filter(s => s != selectable)
    setState(_ => {searchInput: "", selected: newSelected})
  }

  let updateSearchInput = (setState, searchInput) => setState(s => {...s, searchInput: searchInput})

  @react.component
  let make = () => {
    let (state, setState) = React.useState(() => {searchInput: "", selected: []})
    <div className="mt-4">
      <h2 className="text-xl font-semibold"> {"Detailed Example"->str} </h2>
      <div className="mt-4">
        <label
          className="block text-xs font-semibold"
          htmlFor="MultiselectDropdown__search-input-detailed-example">
          {"Filter by:"->str}
        </label>
      </div>
      <Multiselect
        unselected={unselected(state.searchInput)}
        selected=state.selected
        onSelect={select(setState)}
        onDeselect={deselect(state.selected, setState)}
        value=state.searchInput
        onChange={updateSearchInput(setState)}
        placeholder="Type city, state or country"
      />
    </div>
  }
}

module MinimalExample = {
  module Selectable = {
    type rec t =
      | City(pincode, name)
      | Country(countryCode, name)
    and pincode = string
    and countryCode = string
    and name = string

    let label = _t => None

    let value = t =>
      switch t {
      | City(_pincode, name) => name
      | Country(_countryCode, name) => name
      }

    let searchString = t => t->value
    let color = _t => "gray"

    let makeCountry = (~name, ~countryCode) => Country(countryCode, name)
    let makeCity = (~name, ~pincode) => City(pincode, name)
  }

  // create a Multiselect
  module Multiselect = MultiselectDropdown.Make(Selectable)

  type state = {
    selected: array<Selectable.t>,
    searchString: string,
  }

  let unselected = [
    Selectable.makeCity(~name="Delhi", ~pincode=""),
    Selectable.makeCountry(~name="India", ~countryCode="91"),
    Selectable.makeCity(~name="Washington D.C", ~pincode=""),
    Selectable.makeCountry(~name="USA", ~countryCode="1"),
  ]

  let deselect = (selected, setState, selectable) => {
    let newSelected = selected->Js.Array.filter(s => s != selectable)
    setState(_ => {searchString: "", selected: newSelected})
  }

  @react.component
  let make = () => {
    let (state, setState) = React.useState(() => {searchString: "", selected: []})
    <div className="mt-4">
      <h2 className="text-xl font-semibold"> {"Minimal Example"->str} </h2>
      <div className="mt-4">
        <label className="block text-xs font-semibold" htmlFor="MultiselectDropdown__search-input">
          {"Filter by:"->str}
        </label>
      </div>
      <Multiselect
        unselected
        selected=state.selected
        onSelect={selectable =>
          setState(s => {
            searchString: "",
            selected: [selectable]->Array.append(s.selected),
          })}
        onDeselect={deselect(state.selected, setState)}
        value=state.searchString
        onChange={searchString => setState(s => {...s, searchString: searchString})}
      />
    </div>
  }
}

ReactDOMRe.renderToElementWithId(<DetailedExample />, "DetailedExample")
ReactDOMRe.renderToElementWithId(<MinimalExample />, "MinimalExample")

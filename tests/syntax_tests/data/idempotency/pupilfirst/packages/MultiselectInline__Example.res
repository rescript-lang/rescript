let str = React.string

module Example = {
  module Selectable = {
    type rec t = Sport(name)
    and name = string

    let value = t =>
      switch t {
      | Sport(name) => name
      }

    let searchString = value

    let makeSport = name => Sport(name)
  }

  module MultiSelect = MultiselectInline.Make(Selectable)

  type state = {
    searchInput: string,
    selected: array<Selectable.t>,
  }

  let unselected = selected => {
    let searchCollection =
      [
        "Badminton",
        "Tennis",
        "Baseball",
        "Swimming",
        "Volleyball",
        "Football",
        "Formula 1",
        "Squash",
        "Boxing",
      ]->Array.map(sportName => Selectable.makeSport(sportName))
    searchCollection->Js.Array.filter(sport => !(selected->Array.mem(sport)))
  }

  let setSportSearch = (setState, value) => setState(state => {...state, searchInput: value})

  let select = (setState, state, sport) => {
    let selected = state.selected->Js.Array.concat([sport])
    setState(_state => {searchInput: "", selected: selected})
  }

  let deSelect = (setState, state, sport) => {
    let selected =
      state.selected->Js.Array.filter(selected =>
        Selectable.value(sport) != Selectable.value(selected)
      )
    setState(_state => {searchInput: "", selected: selected})
  }

  @react.component
  let make = () => {
    let (state, setState) = React.useState(() => {searchInput: "", selected: []})
    <div className="mt-4">
      <h2 className="text-xl font-semibold"> {"Example"->str} </h2>
      <div className="mt-4">
        <label
          className="block text-xs font-semibold" htmlFor="MultiselectInline__search-input-example">
          {"Select your sports:"->str}
        </label>
      </div>
      <MultiSelect
        placeholder="Search sport"
        emptySelectionMessage="No sport selected"
        allItemsSelectedMessage="You have selected all sports!"
        selected=state.selected
        unselected={unselected(state.selected)}
        onChange={setSportSearch(setState)}
        value=state.searchInput
        onSelect={select(setState, state)}
        onDeselect={deSelect(setState, state)}
      />
    </div>
  }
}
ReactDOMRe.renderToElementWithId(<Example />, "Example")

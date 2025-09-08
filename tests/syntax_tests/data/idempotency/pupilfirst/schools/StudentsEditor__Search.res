open StudentsEditor__Types

let str = React.string

module Selectable = {
  type t =
    | Level(Level.t)
    | Tag(string)
    | NameOrEmail(string)

  let label = t => {
    let labelString = switch t {
    | Level(level) => "Level " ++ (level->Level.number->string_of_int)
    | Tag(_) => "Tag"
    | NameOrEmail(_) => "Name or Email"
    }
    Some(labelString)
  }

  let value = t =>
    switch t {
    | Level(level) => level->Level.name
    | Tag(tag) => tag
    | NameOrEmail(input) => input
    }

  let searchString = t =>
    switch t {
    | Level(level) => level->Level.title
    | Tag(tag) => "tag " ++ tag
    | NameOrEmail(input) => input
    }

  let color = t =>
    switch t {
    | Level(_level) => "orange"
    | Tag(_tag) => "gray"
    | NameOrEmail(_input) => "purple"
    }

  let makeLevel = level => Level(level)
  let makeTag = tag => Tag(tag)
  let makeNameOrEmail = input => NameOrEmail(input)
}

module MultiselectDropdown = MultiselectDropdown.Make(Selectable)

let updateFilter = (setSearchInput, updateFilterCB, filter) => {
  updateFilterCB(filter)
  setSearchInput(_ => "")
}

let selected = (filter, levels) => {
  let level = switch filter->Filter.levelId {
  | Some(id) => [Selectable.makeLevel(id->Level.unsafeFind(levels, "Search"))]
  | None => []
  }
  let searchString = switch filter->Filter.searchString {
  | Some(s) => s->Js.String.trim == "" ? [] : [Selectable.makeNameOrEmail(s)]
  | None => []
  }

  let tags = filter->Filter.tags->Array.map(t => Selectable.makeTag(t))

  searchString->Array.append(tags)->Array.append(level)
}

let unselected = (tags, levels, filter, searchInput) => {
  let tagSuggestions =
    tags
    ->Js.Array.filter(t => !(filter->Filter.tags->Array.mem(t)))
    ->Array.map(t => Selectable.makeTag(t))
  let levelSuggestions = switch filter->Filter.levelId {
  | Some(levelId) => levels->Js.Array.filter(l => l->Level.id != levelId)
  | None => levels
  }->Array.map(l => Selectable.makeLevel(l))
  let searchSuggestion =
    searchInput->Js.String.trim == "" ? [] : [Selectable.makeNameOrEmail(searchInput)]

  searchSuggestion->Array.append(tagSuggestions)->Array.append(levelSuggestions)
}

let select = (filter, updateFilterCB, setSearchInput, selectable) =>
  switch (selectable: Selectable.t) {
  | Level(level) => filter->Filter.changeLevelId(Some(level->Level.id))
  | Tag(tag) => filter->Filter.addTag(tag)
  | NameOrEmail(input) => filter->Filter.changeSearchString(Some(input))
  }->updateFilter(setSearchInput, updateFilterCB)

let deselect = (filter, updateFilterCB, selectable) => {
  let newFilter = switch (selectable: Selectable.t) {
  | Level(_level) => filter->Filter.removeLevelId
  | Tag(tag) => filter->Filter.removeTag(tag)
  | NameOrEmail(_) => filter->Filter.removeSearchString
  }

  updateFilterCB(newFilter)
}
let updateSearchInput = (setSearchInput, searchInput) => setSearchInput(_ => searchInput)

@react.component
let make = (~filter, ~updateFilterCB, ~tags, ~levels) => {
  let (searchInput, setSearchInput) = React.useState(() => "")
  let id = "search"
  <div className="inline-block w-full">
    <label className="block text-tiny font-semibold uppercase" htmlFor=id>
      {"Filter by:"->str}
    </label>
    <MultiselectDropdown
      unselected={unselected(tags, levels, filter, searchInput)}
      selected={selected(filter, levels)}
      onSelect={select(filter, updateFilterCB, setSearchInput)}
      onDeselect={deselect(filter, updateFilterCB)}
      value=searchInput
      onChange={updateSearchInput(setSearchInput)}
      id
      placeholder="Type name, tag or level"
    />
  </div>
}

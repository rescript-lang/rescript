%raw(`require("./MultiselectDropdown.css")`)

let str = React.string

module DomUtils = {
  exception RootElementMissing(string)

  open Webapi.Dom

  let focus = id =>
    (switch document->Document.getElementById(id) {
    | Some(el) => el
    | None => throw(RootElementMissing(id))
    }->Element.asHtmlElement)->Belt.Option.map(HtmlElement.focus)->ignore
}

module type Selectable = {
  type t
  let label: t => option<string>
  let value: t => string
  let searchString: t => string
  let color: t => string
}

module Make = (Selectable: Selectable) => {
  let search = (searchString, selections) =>
    (selections->Js.Array.filter(selection =>
      selection
      ->Selectable.searchString
      ->String.lowercase_ascii
      ->Js.String.includes(searchString->String.lowercase_ascii)
    ))
      ->Belt.SortArray.stableSortBy((x, y) =>
        String.compare(x->Selectable.value, y->Selectable.value)
      )

  let selectionTitle = selection => {
    let value = selection->Selectable.value
    switch selection->Selectable.label {
    | Some(label) => "Pick " ++ (label ++ (": " ++ value))
    | None => "Pick " ++ value
    }
  }

  let tagPillClasses = (color, showHover) => {
    let bgColor200 = "bg-" ++ (color ++ "-200 ")
    let bgColor300 = "bg-" ++ (color ++ "-300 ")
    let textColor800 = "text-" ++ (color ++ "-800 ")
    let textColor900 = "text-" ++ (color ++ "-900 ")

    "rounded text-xs overflow-hidden " ++
    (bgColor200 ++
    (textColor800 ++ (
      showHover ? "px-2 py-px hover:" ++ (bgColor300 ++ ("hover:" ++ textColor900)) : "inline-flex"
    )))
  }

  let applyFilter = (selection, onSelect, id, event) => {
    event->ReactEvent.Mouse.preventDefault

    onSelect(selection)
    DomUtils.focus(id)
  }

  let searchResult = (searchInput, unselected, labelSuffix, id, onSelect) => {
    // Remove all excess space characters from the user input.
    let normalizedString =
      searchInput
      ->Js.String.trim
      ->Js.String.replaceByRe(Js.Re.fromStringWithFlags("\\s+", ~flags="g"), " ")

    switch normalizedString {
    | "" => []
    | searchString =>
      let matchingSelections = unselected->search(searchString)

      matchingSelections->Array.mapi((index, selection) =>
        <button
          key={index->string_of_int}
          title={selectionTitle(selection)}
          className="flex text-xs py-1 items-center w-full hover:bg-gray-200 focus:outline-none focus:bg-gray-200"
          onClick={applyFilter(selection, onSelect, id)}>
          {switch selection->Selectable.label {
          | Some(label) =>
            <span className="mr-2 w-1/6 text-right"> {label ++ labelSuffix->str} </span>
          | None => React.null
          }}
          <span className={tagPillClasses(selection->Selectable.color, true)}>
            {selection->Selectable.value->str}
          </span>
        </button>
      )
    }
  }

  let removeSelection = (onDeselect, selection, event) => {
    event->ReactEvent.Mouse.preventDefault

    onDeselect(selection)
  }

  let showSelected = (onDeselect, labelSuffix, selected) =>
    selected->Array.mapi((index, selection) => {
      let value = selection->Selectable.value
      <div key={index->string_of_int} className="inline-flex py-1 mr-2">
        <div className={tagPillClasses(selection->Selectable.color, false)}>
          <span className="pl-2 py-px">
            {switch selection->Selectable.label {
            | Some(label) => label ++ (labelSuffix ++ value)
            | None => value
            }->str}
          </span>
          <button
            title={"Remove selection: " ++ value}
            className="ml-1 text-red-700 px-2 py-px focus:outline-none hover:bg-red-400 hover:text-white flex items-center"
            onClick={removeSelection(onDeselect, selection)}>
            <PfIcon className="if i-times-light" />
          </button>
        </div>
      </div>
    })

  @react.component
  let make = (
    ~id=?,
    ~placeholder="Search",
    ~onChange,
    ~value,
    ~unselected,
    ~selected,
    ~onSelect,
    ~onDeselect,
    ~labelSuffix=": ",
    ~emptyMessage="No results found",
  ) => {
    let (inputId, _setId) = React.useState(() =>
      switch id {
      | Some(id) => id
      | None =>
        "re-multiselect-" ++
        ((Js.Date.now()->Js.Float.toString) ++
        ("-" ++ (Js.Math.random_int(100000, 999999)->string_of_int)))
      }
    )

    let results = searchResult(value, unselected, labelSuffix, inputId, onSelect)
    <div className="w-full relative">
      <div>
        <div
          className="flex flex-wrap items-center text-sm bg-white border border-gray-400 rounded w-full py-1 px-3 mt-1 focus:outline-none focus:bg-white focus:border-primary-300">
          {selected->showSelected(onDeselect, labelSuffix)->React.array}
          <input
            autoComplete="off"
            value
            onChange={e => onChange(ReactEvent.Form.target(e)["value"])}
            className="flex-grow appearance-none bg-transparent border-none text-gray-700 mr-3 py-1 leading-snug focus:outline-none"
            id=inputId
            type_="text"
            placeholder
          />
        </div>
      </div>
      <div />
      {if value->String.trim != "" {
        <div
          className="multiselect-dropdown__search-dropdown w-full absolute border border-gray-400 bg-white mt-1 rounded-lg shadow-lg px-4 py-2 z-50">
          {switch results {
          | [] => <div> {emptyMessage->str} </div>
          | results => results->React.array
          }}
        </div>
      } else {
        React.null
      }}
    </div>
  }
}

let str = React.string

type key = string
type value = string
type selected = bool
type item = (key, value, selected)

let convertOldItems = items =>
  items->List.map(((key, value, selected)) => (key->string_of_int, value, selected))

let convertOldCallback = (cb, key, value, selected) => cb(key->int_of_string, value, selected)

@react.component
let make = (
  ~items: list<item>,
  ~selectCB: (key, value, selected) => unit,
  ~noSelectionHeading="None Selected",
  ~noSelectionDescription="Select from the following list.",
  ~emptyListDescription="There are no items to select.",
) => {
  let (searchString, setSearchString) = React.useState(() => "")
  let selectedList = items->List.filter(((_, _, selected)) => selected == true)
  let nonSelectedList = items->List.filter(((_, _, selected)) => selected == false)
  let filteredList = switch nonSelectedList {
  | list{} => list{}
  | someList =>
    someList->List.filter(((_, value, _)) =>
      Js.String.includes(String.lowercase_ascii(searchString), String.lowercase_ascii(value))
    )
  }

  <div className="p-6 border rounded bg-gray-100">
    {selectedList->List.length > 0
      ? selectedList
        ->List.rev
        ->List.map(((key, value, _)) =>
          <div
            key
            className="select-list__item-selected flex items-center justify-between bg-white font-semibold text-xs border rounded mb-2">
            <div className="p-3 flex-1"> {value->str} </div>
            <button
              className="flex p-3 text-gray-800 hover:bg-gray-200 hover:text-gray-900 focus:outline-none"
              title="Remove"
              onClick={_event => {
                ReactEvent.Mouse.preventDefault(_event)
                setSearchString(_ => "")
                selectCB(key, value, false)
              }}>
              <i className="fas fa-trash-alt text-base" />
            </button>
          </div>
        )
        ->Array.of_list
        ->React.array
      : <div
          className="flex flex-col items-center justify-center bg-gray-100 text-gray-800 rounded px-3 pt-3 ">
          <i className="fas fa-inbox text-3xl" />
          <h5 className="mt-1 font-semibold"> {noSelectionHeading->str} </h5>
          <span className="text-xs">
            {(items->ListUtils.isEmpty ? emptyListDescription : noSelectionDescription)->str}
          </span>
        </div>}
    {nonSelectedList->List.length > 0
      ? <div className="flex relative pt-4">
          <div className="select-list__group text-sm bg-white rounded shadow pb-2 w-full">
            {nonSelectedList->List.length > 3
              ? <div className="px-3 pt-3 pb-2">
                  <input
                    className="appearance-none bg-transparent border-b w-full text-gray-700 pb-3 px-2 pl-0 leading-normal focus:outline-none"
                    type_="text"
                    placeholder="Type to Search"
                    onChange={event => setSearchString(ReactEvent.Form.target(event)["value"])}
                  />
                </div>
              : React.null}
            <div className={nonSelectedList->List.length > 3 ? "h-28 overflow-y-scroll" : ""}>
              {filteredList
              ->List.map(((key, value, _)) =>
                <div
                  key
                  onClick={_event => {
                    ReactEvent.Mouse.preventDefault(_event)
                    setSearchString(_ => "")
                    selectCB(key, value, true)
                  }}
                  title={"Select " ++ value}
                  className="px-3 py-2 font-semibold hover:bg-primary-100 hover:text-primary-500 cursor-pointer">
                  {value->str}
                </div>
              )
              ->Array.of_list
              ->React.array}
            </div>
          </div>
        </div>
      : React.null}
  </div>
}

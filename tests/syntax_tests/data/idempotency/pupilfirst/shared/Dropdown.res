open Webapi.Dom

let onWindowClick = (showDropdown, setShowDropdown, _event) =>
  if showDropdown {
    setShowDropdown(_ => false)
  } else {
    ()
  }

let toggleDropdown = (setShowDropdown, event) => {
  event->ReactEvent.Mouse.stopPropagation
  setShowDropdown(showDropdown => !showDropdown)
}

let containerClasses = className => "dropdown inline-block relative text-sm " ++ className

@react.component
let make = (~selected, ~contents, ~right=false, ~className="w-full md:w-auto") => {
  let (showDropdown, setShowDropdown) = React.useState(() => false)

  React.useEffect1(() => {
    let curriedFunction = onWindowClick(showDropdown, setShowDropdown)

    let removeEventListener = () => Window.removeEventListener("click", curriedFunction, window)

    if showDropdown {
      Window.addEventListener("click", curriedFunction, window)
      Some(removeEventListener)
    } else {
      removeEventListener()
      None
    }
  }, [showDropdown])

  <div className={containerClasses(className)} onClick={toggleDropdown(setShowDropdown)}>
    selected
    {showDropdown
      ? <ul
          className={"dropdown__list bg-white overflow-y-auto shadow-lg rounded mt-1 border border-gray-400 absolute overflow-hidden min-w-full md:w-auto z-30 " ++ (
            right ? "right-0" : "left-0"
          )}>
          {contents
          ->Array.mapi((index, content) =>
            <li
              key={"dropdown-" ++ (index->string_of_int)}
              className="cursor-pointer block text-sm font-semibold text-gray-900 border-b border-gray-200 bg-white hover:text-primary-500 hover:bg-gray-200 md:whitespace-no-wrap">
              content
            </li>
          )
          ->React.array}
        </ul>
      : React.null}
  </div>
}

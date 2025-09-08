let str = React.string

let handleClick = (setShowDropdown, versions, event) => {
  event->ReactEvent.Mouse.preventDefault
  if versions->Array.length > 1 {
    setShowDropdown(showDropdown => !showDropdown)
  }
}

let handleVersionSelect = (setShowDropdown, selectVersionCB, selectedVersion, event) => {
  event->ReactEvent.Mouse.preventDefault
  setShowDropdown(showDropdown => !showDropdown)
  selectVersionCB(selectedVersion)
}

let handleViewMode = (switchViewModeCB, previewMode, event) => {
  event->ReactEvent.Mouse.preventDefault
  switchViewModeCB(previewMode)
}

let handleRestoreVersion = (handleRestoreVersionCB, versionOn, event) => {
  event->ReactEvent.Mouse.preventDefault
  handleRestoreVersionCB(versionOn)
}

let previewModeButtonEnableClass = "bg-primary-100 shadow-inner text-primary-500"

@react.component
let make = (
  ~versions,
  ~selectedVersion,
  ~selectVersionCB,
  ~previewMode,
  ~switchViewModeCB,
  ~handleRestoreVersionCB,
) => {
  let (showDropdown, setShowDropdown) = React.useState(() => false)
  <div className="flex justify-between items-end">
    <div className="w-2/6">
      {selectedVersion == versions[0]
        ? <div className="flex rounded-lg border border-gray-400">
            <button
              onClick={handleViewMode(switchViewModeCB, true)}
              className={"w-1/2 py-2 px-3 font-semibold rounded-l-lg text-sm focus:outline-none " ++ (
                previewMode
                  ? previewModeButtonEnableClass
                  : "bg-white shadow-md hover:shadow hover:text-primary-500 hover:bg-gray-100"
              )}>
              {"Preview"->str}
            </button>
            <button
              onClick={handleViewMode(switchViewModeCB, false)}
              className={"w-1/2 py-2 px-3 font-semibold rounded-r-lg text-sm focus:outline-none " ++ (
                previewMode
                  ? "bg-white shadow-md hover:shadow hover:text-primary-500 hover:bg-gray-100"
                  : previewModeButtonEnableClass
              )}>
              {"Edit"->str}
            </button>
          </div>
        : React.null}
    </div>
    <div className="w-4/6 flex justify-end items-end">
      {selectedVersion == versions[0]
        ? React.null
        : <button
            onClick={handleRestoreVersion(
              handleRestoreVersionCB,
              selectedVersion->Js.Json.string,
            )}
            className="btn btn-warning border border-orange-500 mr-4">
            {"Restore this version"->str}
          </button>}
      <div className="relative">
        <div className="inline-block">
          <label className="text-xs block text-gray-600 mb-1">
            {(versions->Array.length > 1 ? "Versions" : "Version")->str}
          </label>
          <button
            onClick={handleClick(setShowDropdown, versions)}
            className={"target-editor__version-dropdown-button text-sm appearance-none bg-white border inline-flex items-center justify-between focus:outline-none font-semibold relative rounded " ++ (
              versions->Array.length > 1
                ? "px-3 border-gray-400 hover:bg-gray-100 hover:shadow-lg"
                : "border-transparent cursor-auto"
            )}>
            <span className="flex items-center py-2">
              <span className="truncate text-left">
                {selectedVersion->DateTime.stingToFormatedTime(DateTime.OnlyDate)->str}
              </span>
            </span>
            {versions->Array.length > 1
              ? <span className="text-right pl-3 py-2 border-l border-gray-400">
                  <i className="fas fa-chevron-down text-sm" />
                </span>
              : React.null}
          </button>
        </div>
        {showDropdown
          ? <ul
              id="version-selection-list"
              className="target-editor__version-dropdown-list text-sm bg-white font-semibold border border-gray-400 mt-1 shadow-lg rounded-lg border absolute overflow-auto h-auto w-full z-20">
              {versions
              ->Array.to_list
              ->List.filter(version => version != selectedVersion)
              ->List.map(version =>
                <li
                  id=version
                  key=version
                  onClick={handleVersionSelect(setShowDropdown, selectVersionCB, version)}
                  className="target-editor__version-dropdown-list-item flex justify-between whitespace-no-wrap px-3 py-2 cursor-pointer hover:bg-gray-100 hover:text-primary-500">
                  {version->DateTime.stingToFormatedTime(DateTime.OnlyDate)->str}
                </li>
              )
              ->Array.of_list
              ->React.array}
            </ul>
          : React.null}
      </div>
    </div>
  </div>
}

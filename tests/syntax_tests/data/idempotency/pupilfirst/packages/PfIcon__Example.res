%raw(`require("./PfIcon__Example.css")`)

let str = React.string

let copyAndSort = (f, t) => {
  let cp = t->Array.copy
  cp->Array.sort(f)
  cp
}

module Example = {
  let icons = [
    "plus-circle-solid",
    "plus-circle-regular",
    "plus-circle-light",
    "lamp-solid",
    "check-light",
    "times-light",
    "badge-check-solid",
    "badge-check-regular",
    "badge-check-light",
    "writing-pad-solid",
    "eye-solid",
    "users-solid",
    "users-regular",
    "users-light",
    "ellipsis-h-solid",
    "ellipsis-h-regular",
    "ellipsis-h-light",
    "check-square-alt-solid",
    "check-square-alt-regular",
    "check-square-alt-light",
    "check-square-solid",
    "check-square-regular",
    "check-square-light",
    "comment-alt-solid",
    "comment-alt-regular",
    "comment-alt-light",
    "tachometer-solid",
    "tachometer-regular",
    "tachometer-light",
    "user-check-solid",
    "user-check-regular",
    "user-check-light",
    "users-check-solid",
    "users-check-regular",
    "users-check-light",
    "sort-alpha-down-solid",
    "sort-alpha-down-regular",
    "sort-alpha-down-light",
    "clock-solid",
    "clock-regular",
    "clock-light",
    "scroll-solid",
    "scroll-regular",
    "scroll-light",
    "book-open-solid",
    "book-open-regular",
    "book-open-light",
    "long-text-solid",
    "long-text-regular",
    "long-text-light",
    "short-text-solid",
    "short-text-regular",
    "short-text-light",
    "check-circle-alt-solid",
    "check-circle-alt-regular",
    "check-circle-alt-light",
    "file-solid",
    "file-regular",
    "file-light",
    "link-solid",
    "link-regular",
    "link-light",
    "download-solid",
    "download-regular",
    "download-light",
    "external-link-solid",
    "external-link-regular",
    "external-link-light",
    "upload-solid",
    "upload-regular",
    "upload-light",
    "question-circle-solid",
    "question-circle-regular",
    "question-circle-light",
    "question-square-solid",
    "question-square-regular",
    "question-square-light",
  ]

  let search = searchString => {
    let normalizedString =
      searchString
      ->Js.String.trim
      ->Js.String.replaceByRe(Js.Re.fromStringWithFlags("\\s+", ~flags="g"), " ")

    switch normalizedString {
    | "" => icons
    | searchString =>
      icons
      ->Js.Array.filter(icon => icon->String.lowercase_ascii->Js.String.includes(searchString))
      ->copyAndSort(String.compare)
    }
  }

  let onChange = (setSearchString, event) => {
    let searchString = ReactEvent.Form.target(event)["value"]
    setSearchString(_ => searchString)
  }

  @react.component
  let make = () => {
    let (searchString, setSearchString) = React.useState(() => "")
    <div className="max-w-5xl mx-auto">
      <h1 className="text-center text-2xl font-bold pt-4"> {"pf-icon"->str} </h1>
      <div>
        <div className="mt-4">
          <input
            autoComplete="off"
            value=searchString
            onChange={onChange(setSearchString)}
            type_="text"
            placeholder="Search"
            className="mx-2 text-sm bg-white border border-gray-400 rounded py-2 px-3 mt-1 focus:outline-none focus:bg-white focus:border-primary-300 appearance-none text-gray-700 focus:outline-none md:w-2/5"
          />
        </div>
        <div className="mx-2 mt-4 flex md:flex-row flex-col flex-wrap bg-white border rounded p-2">
          {switch search(searchString) {
          | [] => <div className="p-4 text-sm text-center w-full"> {"Icon not found"->str} </div>
          | resultIcons =>
            resultIcons
            ->Array.map(icon => {
              let iconClasses = "if i-" ++ icon
              <div key=icon className="flex items-center mt-4 md:w-1/2 w-full px-2 my-2">
                <PfIcon className={iconClasses ++ " if-fw text-2xl"} />
                <div className="ml-4 overflow-x-auto">
                  <div className="font-semibold text-xl"> {icon->str} </div>
                  <div className="overflow-x-auto">
                    <code
                      className="inline-block text-gray-900 text-xs bg-red-100 p-1 mt-px whitespace-no-wrap">
                      {"<PfIcon className=\"" ++ (iconClasses ++ " if-fw\" />")->str}
                    </code>
                  </div>
                </div>
              </div>
            })
            ->React.array
          }}
        </div>
      </div>
    </div>
  }
}
ReactDOMRe.renderToElementWithId(<Example />, "root")

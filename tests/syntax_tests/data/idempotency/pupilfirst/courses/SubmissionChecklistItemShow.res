@@config({jsx: 3})

module ChecklistItem = SubmissionChecklistItem

let str = React.string

let kindIconClasses = result =>
  switch (result: ChecklistItem.result) {
  | ShortText(_text) => "if i-short-text-regular md:text-base text-gray-800 if-fw"
  | LongText(_markdown) => "if i-long-text-regular md:text-base text-gray-800 if-fw"
  | Link(_link) => "if i-link-regular md:text-base text-gray-800 if-fw"
  | MultiChoice(_text) => "if i-check-circle-alt-regular md:text-base text-gray-800 if-fw"
  | Files(_files) => "if i-file-regular md:text-base text-gray-800 if-fw"
  }

let showFiles = files =>
  <div className="flex flex-wrap">
    {files
    ->Array.map(file =>
      <a
        key={"file-" ++ (file->ChecklistItem.fileUrl)}
        href={file->ChecklistItem.fileUrl}
        target="_blank"
        className="mt-1 mr-3 flex border overflow-hidden rounded hover:shadow-md border-pink-400 bg-white text-pink-700 hover:border-pink-600 hover:text-pink-700">
        <span
          className="course-show-attachments__attachment-title rounded text-xs font-semibold inline-block whitespace-normal truncate w-32 md:w-42 h-full px-3 py-2 leading-loose">
          {file->ChecklistItem.fileName->str}
        </span>
        <span className="flex w-10 justify-center items-center p-2 bg-pink-700 text-white">
          <PfIcon className="if i-download-regular" />
        </span>
      </a>
    )
    ->React.array}
  </div>

let showlink = link =>
  <a
    href=link
    target="_blank"
    className="max-w-fc mt-1 mr-3 flex border overflow-hidden rounded hover:shadow-md border-indigo-400 bg-white text-indigo-700 hover:border-blue-600 hover:text-indigo-800">
    <span
      className="course-show-attachments__attachment-title rounded text-xs font-semibold inline-block whitespace-normal truncate w-32 md:w-42 h-full px-3 py-2 leading-loose">
      {link->str}
    </span>
    <span className="flex w-10 justify-center items-center p-2 bg-indigo-700 text-white">
      <PfIcon className="if i-external-link-regular" />
    </span>
  </a>

let statusIcon = (updateChecklistCB, status) =>
  switch (updateChecklistCB, (status: ChecklistItem.status)) {
  | (None, Passed) =>
    <PfIcon className="if i-check-square-solid text-green-500 text-lg mr-3 -ml-6 mt-1 bg-white" />
  | (None, Failed) =>
    <PfIcon className="if i-times-square-solid text-red-500 text-lg mr-3 -ml-6 mt-1 bg-white" />
  | (_, _) => React.null
  }

let showStatus = status =>
  switch (status: ChecklistItem.status) {
  | Passed =>
    <div className="bg-green-200 rounded px-1 py-px text-green-800 text-tiny">
      {"Correct"->str}
    </div>
  | Failed =>
    <div className="bg-red-200 rounded px-1 py-px text-red-800 text-tiny">
      {"Incorrect"->str}
    </div>
  | NoAnswer => React.null
  }

let statusButtonSelectedClasses = (status, currentStatus) =>
  "inline-flex items-center cursor-pointer leading-tight font-semibold inline-block text-xs relative hover:bg-gray-100 hover:text-gray-700 " ++
  switch ((currentStatus: ChecklistItem.status), (status: ChecklistItem.status)) {
  | (
      Passed,
      Passed,
    ) => "bg-green-100 hover:bg-green-100 text-green-800 hover:text-green-800 border-green-500 z-10"
  | (
      Failed,
      Failed,
    ) => "bg-red-100 hover:bg-red-100 text-red-700 hover:text-red-700 border-red-500 z-10"
  | (_, _) => "bg-white"
  }

let statusButtonIcon = bool =>
  bool
    ? "if i-times-square-solid text-base if-fw"
    : "if i-square-regular text-base if-fw text-gray-500"

let statusButtonOnClick = (bool, callback, checklist, index, _event) =>
  bool
    ? callback(checklist->ChecklistItem.makeNoAnswer(index))
    : callback(checklist->ChecklistItem.makeFailed(index))

let statusButton = (index, status, callback, checklist) =>
  <div className="mt-2">
    <button
      onClick={statusButtonOnClick(status == ChecklistItem.Failed, callback, checklist, index)}
      className={"border border-gray-500 rounded " ++
      statusButtonSelectedClasses(ChecklistItem.Failed, status)}>
      <span className="w-8 p-2 border-r border-gray-500 flex items-center justify-center">
        <PfIcon className={statusButtonIcon(status == ChecklistItem.Failed)} />
      </span>
      <span className="p-2"> {"Mark as incorrect"->str} </span>
    </button>
  </div>

let computeShowResult = (pending, checklistItem) =>
  switch (pending, checklistItem->ChecklistItem.status) {
  | (true, NoAnswer | Passed | Failed) => true
  | (false, Failed) => true
  | (false, NoAnswer | Passed) => false
  }

let cardClasses = pending => pending ? "mt-3" : "rounded shadow mt-4 "

let cardHeaderClasses = pending =>
  "text-sm font-semibold flex items-center justify-between " ++ (
    pending ? "" : "p-4 bg-white rounded cursor-pointer"
  )

let cardBodyClasses = pending =>
  "pl-5 md:pl-7 p-3 pb-4 " ++ (pending ? "" : "border-t bg-gray-200 rounded-b")

@react.component
let make = (~index, ~checklistItem, ~updateChecklistCB, ~checklist, ~pending) => {
  let (showResult, setShowResult) = React.useState(() => computeShowResult(pending, checklistItem))

  React.useEffect1(() => {
    let newShowResult = computeShowResult(pending, checklistItem)
    newShowResult == showResult ? () : setShowResult(_ => newShowResult)
    None
  }, [updateChecklistCB])
  let status = checklistItem->ChecklistItem.status

  <div
    className={cardClasses(pending)}
    ariaLabel={checklistItem->ChecklistItem.title}
    onClick={_ => setShowResult(_ => true)}>
    <div className={cardHeaderClasses(pending)}>
      <div className="inline-flex items-center">
        {statusIcon(updateChecklistCB, status)}
        <PfIcon className={kindIconClasses(checklistItem->ChecklistItem.result)} />
        <p className="pl-2 tracking-wide"> {checklistItem->ChecklistItem.title->str} </p>
      </div>
      <div className="inline-block">
        {showResult ? showStatus(status) : <button> <i className="fas fa-chevron-down" /> </button>}
      </div>
    </div>
    {showResult
      ? <div className={cardBodyClasses(pending)}>
          <div>
            {switch checklistItem->ChecklistItem.result {
            | ShortText(text) => <div> {text->str} </div>
            | LongText(markdown) => <MarkdownBlock profile=Markdown.Permissive markdown />
            | Link(link) => showlink(link)
            | MultiChoice(text) => <div> {text->str} </div>
            | Files(files) => showFiles(files)
            }}
          </div>
          {switch updateChecklistCB {
          | Some(callback) => statusButton(index, status, callback, checklist)
          | None => React.null
          }}
        </div>
      : React.null}
  </div>
}

@@config({jsx: 3})

open CurriculumEditor__Types

let str = React.string

let updateTitle = (checklistItem, updateChecklistItemCB, event) => {
  let title = ReactEvent.Form.target(event)["value"]
  let newChecklistItem = checklistItem->ChecklistItem.updateTitle(title)
  updateChecklistItemCB(newChecklistItem)
}

let updateKind = (checklistItem, updateChecklistItemCB, kind) => {
  let newChecklistItem = checklistItem->ChecklistItem.updateKind(kind)
  updateChecklistItemCB(newChecklistItem)
}

let updateOptional = (checklistItem, updateChecklistItemCB, event) => {
  let optional = ReactEvent.Form.target(event)["checked"]
  let newChecklistItem = checklistItem->ChecklistItem.updateOptional(optional)
  updateChecklistItemCB(newChecklistItem)
}

let buttonColorClasses = color =>
  "border-" ++
  (color ++
  ("-500 " ++ ("bg-" ++ (color ++ ("-100 " ++ ("text-" ++ (color ++ "-800")))))))
let iconColorClasses = color => "bg-" ++ (color ++ "-500 ")
let selectedButtonIcon = kind =>
  switch kind {
  | ChecklistItem.LongText => "i-long-text-regular"
  | ShortText => "i-short-text-regular"
  | Files => "i-file-regular"
  | Link => "i-link-regular"
  | MultiChoice(_choices) => "i-check-circle-alt-regular"
  }
let checklistDropdown = (checklistItem, allowFileKind, updateChecklistItemCB) => {
  let selectedKind = checklistItem->ChecklistItem.kind
  let selectedButtonColor = switch selectedKind {
  | LongText => "blue"
  | ShortText => "orange"
  | Files => "pink"
  | Link => "indigo"
  | MultiChoice(_choices) => "teal"
  }
  let selected =
    <button
      className={"border focus:outline-none appearance-none flex items-center rounded " ++
      buttonColorClasses(selectedButtonColor)}>
      <div className="flex">
        <span
          className={"flex items-center justify-center rounded text-white p-1 m-1 " ++
          iconColorClasses(selectedButtonColor)}>
          <PfIcon className={"if if-fw " ++ selectedButtonIcon(selectedKind)} />
        </span>
        <span className="inline-flex items-center px-1 py-1 font-semibold text-xs">
          {selectedKind->ChecklistItem.actionStringForKind->str}
        </span>
      </div>
      <span className="px-2 py-1 flex items-center">
        <i className="fas fa-caret-down text-xs" />
      </span>
    </button>

  let defaultKindTypes = [ChecklistItem.LongText, ShortText, Link, MultiChoice(["Yes", "No"])]

  let allowedKindTypes = allowFileKind
    ? defaultKindTypes->Array.append([ChecklistItem.Files])
    : defaultKindTypes

  let contents =
    allowedKindTypes
    ->Js.Array.filter(kind => kind != selectedKind)
    ->Array.mapi((index, kind) =>
      <button
        key={index->string_of_int}
        className="w-full px-2 py-1 focus:outline-none appearance-none text-left"
        onClick={_ => updateKind(checklistItem, updateChecklistItemCB, kind)}>
        <PfIcon className={"mr-2 if if-fw " ++ selectedButtonIcon(kind)} />
        {kind->ChecklistItem.actionStringForKind->str}
      </button>
    )
  <Dropdown selected contents />
}

let removeMultichoiceOption = (choiceIndex, checklistItem, updateChecklistItemCB) => {
  let newChecklistItem = checklistItem->ChecklistItem.removeMultichoiceOption(choiceIndex)
  updateChecklistItemCB(newChecklistItem)
}
let addMultichoiceOption = (checklistItem, updateChecklistItemCB) => {
  let newChecklistItem = checklistItem->ChecklistItem.addMultichoiceOption
  updateChecklistItemCB(newChecklistItem)
}

let updateChoiceText = (choiceIndex, checklistItem, updateChecklistItemCB, event) => {
  let choice = ReactEvent.Form.target(event)["value"]
  let newChecklistItem = checklistItem->ChecklistItem.updateMultichoiceOption(choiceIndex, choice)
  updateChecklistItemCB(newChecklistItem)
}

let multiChoiceEditor = (choices, checklistItem, removeMultichoiceOption, updateChecklistItemCB) =>
  <div className="ml-3 mt-3">
    <div className="text-xs font-semibold mb-2"> {"Choices:"->str} </div>
    {
      let showRemoveIcon = Array.length(choices) > 2
      choices
      ->Array.mapi((index, choice) =>
        <div key={index->string_of_int}>
          <div className="flex items-center text-sm rounded mt-2">
            <span className="text-gray-400"> <i className="far fa-circle text-base" /> </span>
            <div
              className="flex flex-1 py-2 px-3 ml-3 justify-between items-center focus:outline-none bg-white focus:bg-white focus:border-primary-300 border border-gray-400 rounded">
              <input
                name={"multichoice-input-" ++ (index + 1->string_of_int)}
                className="flex-1 appearance-none bg-transparent border-none leading-snug focus:outline-none"
                onChange={updateChoiceText(index, checklistItem, updateChecklistItemCB)}
                type_="text"
                value=choice
              />
              <button
                title={"Remove Choice " ++ (index + 1->string_of_int)}
                onClick={_ => removeMultichoiceOption(index, checklistItem, updateChecklistItemCB)}>
                {showRemoveIcon ? <PfIcon className="if i-times-regular if-fw" /> : React.null}
              </button>
            </div>
          </div>
          <div className="ml-6">
            <School__InputGroupError
              message="Not a valid choice" active={choice->String.trim == ""}
            />
          </div>
        </div>
      )
      ->React.array
    }
    <button
      onClick={_ => addMultichoiceOption(checklistItem, updateChecklistItemCB)}
      className="flex mt-2 ml-7 p-2 text-sm appearance-none bg-white border rounded items-center justify-between outline-none border-gray-400 hover:border-gray-100 hover:shadow-lg focus:outline-none">
      <PfIcon className="fas fa-plus-circle if-fw" />
      <span className="font-semibold ml-2"> {"Add a choice"->str} </span>
    </button>
  </div>

let controlIcon = (~icon, ~title, ~handler) =>
  handler == None
    ? React.null
    : <button
        title
        disabled={handler == None}
        className="px-2 py-1 focus:outline-none text-sm text-gray-700 hover:bg-gray-300 hover:text-gray-900 overflow-hidden"
        onClick=?handler>
        <i className={"fas fa-fw " ++ icon} />
      </button>

let filesNotice =
  <div className="mt-2 text-sm">
    <strong> {"Note:"->str} </strong>
    <span className="ml-1">
      {"Students can submit up to 3 files, limited to 5 MB each."->str}
    </span>
  </div>

let isRequiredStepTitleDuplicated = (checklist, item) => {
  let trimmedTitle = item->ChecklistItem.title->String.trim

  if trimmedTitle == "" {
    false
  } else {
    checklist
    ->Js.Array.filter(item => !ChecklistItem.optional(item))
    ->Js.Array.filter(checklistItem =>
      checklistItem->ChecklistItem.title->String.trim == trimmedTitle
    )
    ->Array.length > 1
  }
}

@react.component
let make = (
  ~checklist,
  ~checklistItem,
  ~index,
  ~updateChecklistItemCB,
  ~removeChecklistItemCB,
  ~moveChecklistItemUpCB=?,
  ~moveChecklistItemDownCB=?,
  ~copyChecklistItemCB,
  ~allowFileKind,
) =>
  <div
    key={index->string_of_int}
    ariaLabel={"Editor for checklist item " ++ (index + 1->string_of_int)}
    className="flex items-start py-2 relative">
    <div className="w-full bg-gray-100 border rounded-lg p-5 mr-1">
      <div className="flex justify-between items-center">
        <div> {checklistDropdown(checklistItem, allowFileKind, updateChecklistItemCB)} </div>
        <div className="items-center">
          <input
            className="leading-tight"
            type_="checkbox"
            onChange={updateOptional(checklistItem, updateChecklistItemCB)}
            id={index->string_of_int}
            checked={checklistItem->ChecklistItem.optional}
          />
          <label className="text-xs text-gray-600 ml-2" htmlFor={index->string_of_int}>
            {"Optional"->str}
          </label>
        </div>
      </div>
      <div
        className="flex items-center text-sm bg-white border border-gray-400 rounded py-2 px-3 mt-2 focus:outline-none focus:bg-white focus:border-primary-300">
        <input
          name={"checklist-item-" ++ ((index + 1->string_of_int) ++ "-title")}
          className="flex-grow appearance-none bg-transparent border-none leading-relaxed focus:outline-none"
          placeholder="Describe this step"
          onChange={updateTitle(checklistItem, updateChecklistItemCB)}
          type_="text"
          value={checklistItem->ChecklistItem.title}
        />
      </div>
      <div>
        <School__InputGroupError
          message="Step cannot be empty"
          active={checklistItem->ChecklistItem.title->String.trim == ""}
        />
        <School__InputGroupError
          message="Not a unique step; required steps must be unique"
          active={isRequiredStepTitleDuplicated(checklist, checklistItem)}
        />
      </div>
      {switch checklistItem->ChecklistItem.kind {
      | MultiChoice(choices) =>
        multiChoiceEditor(choices, checklistItem, removeMultichoiceOption, updateChecklistItemCB)
      | Files => filesNotice
      | ShortText
      | LongText
      | Link => React.null
      }}
    </div>
    <div
      ariaLabel={"Controls for checklist item " ++ (index + 1->string_of_int)}
      className="-mr-10 flex-shrink-0 border bg-gray-100 border rounded-lg flex flex-col text-xs sticky top-0">
      {controlIcon(
        ~icon="fa-arrow-up",
        ~title="Move Up",
        ~handler=moveChecklistItemUpCB->OptionUtils.map((cb, _) => cb()),
      )}
      {controlIcon(
        ~icon="fa-arrow-down",
        ~title="Move Down",
        ~handler=moveChecklistItemDownCB->OptionUtils.map((cb, _) => cb()),
      )}
      {controlIcon(~icon="fa-copy", ~title="Copy", ~handler=Some(_ => copyChecklistItemCB()))}
      {controlIcon(
        ~icon="fa-trash-alt",
        ~title="Delete",
        ~handler=Some(_ => removeChecklistItemCB()),
      )}
    </div>
  </div>

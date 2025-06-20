%raw(`require("./CurriculumEditor__TargetShow.css")`)

open CurriculumEditor__Types

let str = ReasonReact.string

let targetClasses = (target, targets) =>
  "target-group__target flex justify-between items-center pl-2 pr-5 " ++
  switch (targets->List.length == 1, target->Target.visibility) {
  | (true, Archived) => "target-group__target--archived py-4 pl-5"
  | (false, Archived) => "target-group__target--archived py-4"
  | (true, _) => "py-6 pl-5"
  | (false, _) => "py-6"
  }

let updateSortIndex = (targets, target, up, updateTargetSortIndexCB) => {
  let newTargets = targets->ListUtils.swap(up, target)
  let targetIds = newTargets->List.map(t => t->Target.id)->Array.of_list
  targetIds->CurriculumEditor__SortResourcesMutation.sort(
    CurriculumEditor__SortResourcesMutation.Target,
  )
  updateTargetSortIndexCB(newTargets)
}

let sortIndexHiddenClass = bool => bool ? " invisible" : ""

let editorLink = (linkPrefix, linkSuffix, target, iconClass) => {
  let link = linkPrefix ++ linkSuffix

  <Link
    title={"Edit " ++ (linkSuffix ++ (" of target " ++ (target->Target.title)))}
    href=link
    className="curriculum-editor__target-show-quick-link text-gray-400 border-l border-transparent py-6 px-3 hover:bg-gray-200">
    <i className={"fas fa-fw " ++ iconClass} />
  </Link>
}

@react.component
let make = (~target, ~targets, ~updateTargetSortIndexCB, ~index, ~course) => {
  let linkPrefix =
    "/school/courses/" ++ ((course->Course.id) ++ ("/targets/" ++ ((target->Target.id) ++ "/")))

  <div
    className="flex target-group__target-container border-t bg-white overflow-hidden relative hover:bg-gray-100 hover:text-primary-500">
    {targets->List.length == 1
      ? React.null
      : <div
          className="target-group__target-reorder relative flex flex-col z-10 h-full border-r border-transparent text-gray-700 justify-between items-center">
          <div
            title="Move Up"
            id={"target-move-up-" ++ (target->Target.id)}
            className={"target-group__target-reorder-up flex items-center justify-center cursor-pointer w-9 h-9 p-1 text-gray-400 hover:bg-gray-200" ++
            sortIndexHiddenClass(index == 0)}
            onClick={_ => updateSortIndex(targets, target, true, updateTargetSortIndexCB)}>
            <i className="fas fa-arrow-up text-sm" />
          </div>
          <div
            title="Move Down"
            id={"target-move-down-" ++ (target->Target.id)}
            className={"target-group__target-reorder-down flex items-center justify-center cursor-pointer w-9 h-9 p-1 border-t border-transparent text-gray-400 hover:bg-gray-200" ++
            sortIndexHiddenClass(index + 1 == (targets->List.length))}
            onClick={_ => updateSortIndex(targets, target, false, updateTargetSortIndexCB)}>
            <i className="fas fa-arrow-down text-sm" />
          </div>
        </div>}
    <Link
      id={"target-show-" ++ (target->Target.id)}
      title={"Edit content of target " ++ (target->Target.title)}
      className={targetClasses(target, targets)}
      href={linkPrefix ++ "content"}>
      <p className="font-semibold text-sm"> {target->Target.title->str} </p>
      <div className="items-center">
        {switch target->Target.visibility {
        | Draft =>
          <span
            className="target-group__target-draft-pill leading-tight text-xs py-1 px-2 font-semibold rounded-lg border bg-blue-100 text-blue-700 border-blue-400 mr-2 whitespace-no-wrap">
            <i className="fas fa-file-signature text-sm" />
            <span className="ml-1"> {"Draft"->str} </span>
          </span>
        | _ => React.null
        }}
      </div>
    </Link>
    {editorLink(linkPrefix, "details", target, "fa-list-alt")}
    {editorLink(linkPrefix, "versions", target, "fa-code-branch")}
  </div>
}

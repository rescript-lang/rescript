open CurriculumEditor__Types

let str = ReasonReact.string

type state = {
  targetTitle: string,
  savingNewTarget: bool,
  validTargetTitle: bool,
}

module CreateTargetMutation = %graphql(`
   mutation CreateTargetMutation($title: String!, $targetGroupId: String!) {
     createTarget(title: $title, targetGroupId: $targetGroupId ) {
       target {
         id
         contentBlockId
         sampleContent
       }
     }
   }
   `)

type action =
  | UpdateTargetTitle(string)
  | UpdateTargetSaving

let reducer = (state, action) =>
  switch action {
  | UpdateTargetTitle(targetTitle) => {
      ...state,
      targetTitle: targetTitle,
      validTargetTitle: targetTitle->String.length > 1,
    }
  | UpdateTargetSaving => {...state, savingNewTarget: !state.savingNewTarget}
  }

let archivedClasses = archived =>
  "target-group__header flex flex-col items-center justify-center relative cursor-pointer px-20 pb-7 text-center rounded-lg rounded-b-none overflow-hidden w-full " ++ (
    archived ? "target-group__header--archived" : ""
  )

let updateSortIndex = (targetGroups, targetGroup, up, updateTargetGroupSortIndexCB) => {
  let newTargetGroups = targetGroups->ListUtils.swap(up, targetGroup)

  let targetGroupIds = newTargetGroups->List.map(t => t->TargetGroup.id)->Array.of_list
  targetGroupIds->CurriculumEditor__SortResourcesMutation.sort(
    CurriculumEditor__SortResourcesMutation.TargetGroup,
  )
  updateTargetGroupSortIndexCB(newTargetGroups)
}

let sortIndexHiddenClass = bool => bool ? " invisible" : ""

@react.component
let make = (
  ~targetGroup,
  ~targetGroups,
  ~targets,
  ~showTargetGroupEditorCB,
  ~updateTargetCB,
  ~showArchived,
  ~updateTargetSortIndexCB,
  ~updateTargetGroupSortIndexCB,
  ~index,
  ~course,
) => {
  let (state, send) = React.useReducer(
    reducer,
    {targetTitle: "", savingNewTarget: false, validTargetTitle: false},
  )
  let milestone = targetGroup->TargetGroup.milestone
  let targetGroupArchived = targetGroup->TargetGroup.archived
  let targetsInTG =
    targets
    ->List.filter(target => target->Target.targetGroupId == (targetGroup->TargetGroup.id))
    ->Target.sort

  let targetsToDisplay = showArchived
    ? targetsInTG
    : targetsInTG->List.filter(target => !(target->Target.visibility === Archived))
  let handleResponseCB = target => {
    let targetId = target["id"]
    let targetGroupId = targetGroup->TargetGroup.id
    /* let sortIndex = json->Json.Decode.(field("sortIndex", int)); */
    let newTarget = Target.template(targetId, targetGroupId, state.targetTitle)
    send(UpdateTargetSaving)
    send(UpdateTargetTitle(""))
    updateTargetCB(newTarget)
  }
  let handleCreateTarget = (title, targetGroupId) => {
    send(UpdateTargetSaving)

    CreateTargetMutation.make(~title, ~targetGroupId, ())
    ->GraphqlQuery.sendQuery
    ->Js.Promise.then_(response => {
      switch response["createTarget"]["target"] {
      | Some(target) => handleResponseCB(target)
      | None => ()
      }
      Js.Promise.resolve()
    })
    ->ignore
  }

  <div
    className="target-group__box relative mt-12 rounded-lg border border-b-0 border-gray-300 shadow-md">
    <div
      className="w-full target-group__header-container rounded-lg rounded-b-none relative bg-white hover:bg-gray-100 hover:text-primary-500">
      <div
        id="target_group"
        className={archivedClasses(targetGroup->TargetGroup.archived)}
        onClick={_event => showTargetGroupEditorCB(Some(targetGroup))}>
        {milestone
          ? <div
              className="inline-block px-3 py-2 bg-orange-400 font-bold text-xs rounded-b-lg leading-tight text-white uppercase">
              {"Milestone Targets"->str}
            </div>
          : ReasonReact.null}
        <div className="target-group__title pt-6">
          <h4> {targetGroup->TargetGroup.name->str} </h4>
        </div>
        <div className="target-group__description">
          <p className="pt-px text-sm">
            {switch targetGroup->TargetGroup.description {
            | Some(description) => description
            | None => ""
            }->str}
          </p>
        </div>
      </div>
      {targetGroups->List.length == 1
        ? React.null
        : <div
            className="target-group__group-reorder flex flex-col shadow rounded-l-lg absolute h-full border border-r-0 overflow-hidden text-gray-700 justify-between items-center bg-white">
            <div
              title="Move Up"
              id={"target-group-move-up-" ++ (targetGroup->TargetGroup.id)}
              className={"target-group__group-reorder-up flex items-center justify-center cursor-pointer w-9 h-9 p-1 text-gray-400 hover:bg-gray-200" ++
              sortIndexHiddenClass(index == 0)}
              onClick={_ =>
                updateSortIndex(targetGroups, targetGroup, true, updateTargetGroupSortIndexCB)}>
              <i className="fas fa-arrow-up text-sm" />
            </div>
            <div
              title="Move Down"
              id={"target-group-move-down-" ++ (targetGroup->TargetGroup.id)}
              className={"target-group__group-reorder-down flex items-center justify-center cursor-pointer w-9 h-9 p-1 text-gray-400 hover:bg-gray-200" ++
              sortIndexHiddenClass(index + 1 == (targetGroups->List.length))}
              onClick={_ =>
                updateSortIndex(targetGroups, targetGroup, false, updateTargetGroupSortIndexCB)}>
              <i className="fas fa-arrow-down text-sm" />
            </div>
          </div>}
    </div>
    {targetsToDisplay
    ->List.mapi((index, target) =>
      <CurriculumEditor__TargetShow
        key={target->Target.id}
        target
        targets=targetsToDisplay
        updateTargetSortIndexCB
        index
        course
      />
    )
    ->Array.of_list
    ->ReasonReact.array}
    {targetGroupArchived
      ? ReasonReact.null
      : <div
          className="target-group__target-create relative bg-gray-100 flex items-center border border-dashed border-gray-400 text-gray-700 hover:text-gray-900 active:text-gray-900 focus:text-gray-900 hover:shadow-lg hover:border-gray-500 rounded-lg rounded-t-none overflow-hidden">
          <label
            htmlFor={"create-target-input" ++ (targetGroup->TargetGroup.id)}
            className="absolute flex items-center h-full cursor-pointer pl-4">
            <i className="fas fa-plus-circle text-2xl" />
          </label>
          <input
            id={"create-target-input" ++ (targetGroup->TargetGroup.id)}
            title="Create target"
            value=state.targetTitle
            onChange={event => send(UpdateTargetTitle(ReactEvent.Form.target(event)["value"]))}
            placeholder="Create a target"
            className="target-create__input text-xs text-left bg-gray-100 pr-5 pl-12 py-6 rounded-b appearance-none block w-full text-sm text-gray-900 font-semibold leading-tight hover:bg-gray-100 focus:outline-none focus:bg-white focus:border-gray-500"
          />
          {state.validTargetTitle
            ? <button
                onClick={_e => handleCreateTarget(state.targetTitle, targetGroup->TargetGroup.id)}
                disabled=state.savingNewTarget
                className="flex items-center whitespace-no-wrap text-sm font-semibold py-2 px-4 mr-4 rounded btn-primary appearance-none focus:outline-none text-center">
                {"Create"->str}
              </button>
            : ReasonReact.null}
        </div>}
  </div>
}

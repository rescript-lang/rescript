let str = ReasonReact.string

open CurriculumEditor__Types

type editorAction =
  | Hidden
  | ShowTargetGroupEditor(option<TargetGroup.t>)
  | ShowLevelEditor(option<Level.t>)

type state = {
  selectedLevel: Level.t,
  editorAction: editorAction,
  levels: list<Level.t>,
  targetGroups: list<TargetGroup.t>,
  targets: list<Target.t>,
  showArchived: bool,
}

type action =
  | SelectLevel(Level.t)
  | UpdateEditorAction(editorAction)
  | UpdateLevels(Level.t)
  | UpdateTargetGroup(TargetGroup.t)
  | UpdateTargetGroups(list<TargetGroup.t>)
  | UpdateTarget(Target.t)
  | UpdateTargets(list<Target.t>)
  | ToggleShowArchived

let reducer = (state, action) =>
  switch action {
  | SelectLevel(selectedLevel) => {...state, selectedLevel: selectedLevel}
  | UpdateEditorAction(editorAction) => {...state, editorAction: editorAction}
  | UpdateLevels(level) =>
    let newLevels = level->Level.updateList(state.levels)
    {...state, levels: newLevels, editorAction: Hidden, selectedLevel: level}
  | UpdateTargetGroup(targetGroup) =>
    let newtargetGroups = targetGroup->TargetGroup.updateList(state.targetGroups)
    {...state, targetGroups: newtargetGroups}
  | UpdateTargetGroups(targetGroups) => {...state, targetGroups: targetGroups}
  | UpdateTarget(target) =>
    let newtargets = target->Target.updateList(state.targets)
    {...state, targets: newtargets}
  | ToggleShowArchived => {...state, showArchived: !state.showArchived}
  | UpdateTargets(targets) => {...state, targets: targets}
  }

let showArchivedButton = (targetGroupsInLevel, targets) => {
  let tgIds = targetGroupsInLevel->List.map(tg => tg->TargetGroup.id)

  let numberOfArchivedTargetGroupsInLevel =
    targetGroupsInLevel->List.filter(tg => tg->TargetGroup.archived)->List.length
  let numberOfArchivedTargetsInLevel =
    targets
    ->List.filter(target => tgIds->List.mem(target->Target.targetGroupId))
    ->List.filter(target => target->Target.visibility === Archived)
    ->List.length

  numberOfArchivedTargetGroupsInLevel > 0 || numberOfArchivedTargetsInLevel > 0
}

let updateTargetSortIndex = (state, send, sortedTargets) => {
  let oldTargets = state.targets->List.filter(t => !(sortedTargets->List.mem(t)))
  send(UpdateTargets(oldTargets->List.append(sortedTargets->Target.updateSortIndex)))
}

let updateTargetGroupSortIndex = (state, send, sortedTargetGroups) => {
  let oldTargetGroups = state.targetGroups->List.filter(t => !(sortedTargetGroups->List.mem(t)))
  send(
    UpdateTargetGroups(
      oldTargetGroups->List.append(sortedTargetGroups->TargetGroup.updateSortIndex),
    ),
  )
}

let levelOfTarget = (targetId, targets, levels, targetGroups) => {
  let target =
    targets->ListUtils.unsafeFind(
      target => Target.id(target) == targetId,
      "Unable to find target with ID:" ++ (targetId ++ " in CurriculumEditor"),
    )
  let targetGroup =
    targetGroups->ListUtils.unsafeFind(
      tg => TargetGroup.id(tg) == Target.targetGroupId(target),
      "Unable to find target group with ID:" ++
      (Target.targetGroupId(target) ++
      " in CurriculumEditor"),
    )
  levels->ListUtils.unsafeFind(
    level => Level.id(level) == TargetGroup.levelId(targetGroup),
    "Unable to find level with ID:" ++ (TargetGroup.levelId(targetGroup) ++ " in CurriculumEditor"),
  )
}

let computeIntialState = ((levels, targetGroups, targets, path)) => {
  let maxLevel =
    levels->List.sort((l1, l2) => (l2->Level.number) - (l1->Level.number))->List.hd

  let selectedLevel = switch path {
  | list{"school", "courses", _courseId, "curriculum"} => maxLevel
  | list{"school", "courses", _courseId, "targets", targetId, ..._} =>
    levelOfTarget(targetId, targets, levels, targetGroups)
  | _ => maxLevel
  }

  {
    selectedLevel: selectedLevel,
    editorAction: Hidden,
    targetGroups: targetGroups,
    levels: levels,
    targets: targets,
    showArchived: false,
  }
}

@react.component
let make = (~course, ~evaluationCriteria, ~levels, ~targetGroups, ~targets, ~authenticityToken) => {
  let path = ReasonReactRouter.useUrl().path
  let (state, send) = React.useReducerWithMapState(
    reducer,
    (levels, targetGroups, targets, path),
    computeIntialState,
  )

  let hideEditorActionCB = () => send(UpdateEditorAction(Hidden))
  let currentLevel = state.selectedLevel
  let currentLevelId = Level.id(currentLevel)
  let updateLevelsCB = level => send(UpdateLevels(level))
  let targetGroupsInLevel =
    state.targetGroups
    ->List.filter(targetGroup => targetGroup->TargetGroup.levelId == currentLevelId)
    ->TargetGroup.sort
  let targetGroupsToDisplay = state.showArchived
    ? targetGroupsInLevel
    : targetGroupsInLevel->List.filter(tg => !(tg->TargetGroup.archived))
  let showTargetGroupEditorCB = targetGroup =>
    send(UpdateEditorAction(ShowTargetGroupEditor(targetGroup)))

  let updateTargetCB = target => {
    let targetGroup =
      state.targetGroups->ListUtils.unsafeFind(
        tg => TargetGroup.id(tg) == Target.targetGroupId(target),
        "Unabltge to find target group with ID: " ++ Target.targetGroupId(target),
      )

    let updatedTargetGroup = switch target->Target.visibility {
    | Archived => targetGroup
    | Draft
    | Live =>
      targetGroup->TargetGroup.unarchive
    }

    send(UpdateTarget(target))
    send(UpdateTargetGroup(updatedTargetGroup))
  }

  let updateTargetGroupsCB = targetGroup => {
    targetGroup->TargetGroup.archived
      ? {
          let targetIdsInTargerGroup =
            state.targets->Target.targetIdsInTargetGroup(targetGroup->TargetGroup.id)
          let newTargets =
            state.targets->List.map(target =>
              targetIdsInTargerGroup->List.mem(target->Target.id)
                ? Target.archive(target)
                : target
            )
          send(UpdateTargets(newTargets))
        }
      : ()
    send(UpdateTargetGroup(targetGroup))
    send(UpdateEditorAction(Hidden))
  }

  <div className="flex-1 flex flex-col">
    <div className="bg-white p-4 md:hidden shadow border-b">
      <button
        className="sa-toggle__menu-btn sa-toggle__menu-btn--arrow hover:bg-gray-200 focus:outline-none">
        <span className="sa-toggle__menu-btn-box">
          <span className="sa-toggle__menu-btn-inner" />
        </span>
      </button>
    </div>
    <CurriculumEditor__TargetDrawer
      targets=state.targets targetGroups evaluationCriteria course updateTargetCB
    />
    {switch state.editorAction {
    | Hidden => ReasonReact.null
    | ShowTargetGroupEditor(targetGroup) =>
      <CurriculumEditor__TargetGroupEditor
        targetGroup currentLevelId authenticityToken updateTargetGroupsCB hideEditorActionCB
      />
    | ShowLevelEditor(level) =>
      <CurriculumEditor__LevelEditor
        level course authenticityToken hideEditorActionCB updateLevelsCB
      />
    }}
    <div className="px-6 pb-4 flex-1 bg-gray-100 relative overflow-y-scroll">
      <div className="w-full py-4 relative md:sticky top-0 z-20 bg-gray-100 border-b">
        <div className="max-w-3xl flex items-center justify-between mx-auto">
          <div className="flex">
            <div className="inline-block relative w-auto md:w-64">
              <select
                onChange={event => {
                  let level_name = ReactEvent.Form.target(event)["value"]
                  send(SelectLevel(Level.selectLevel(state.levels, level_name)))
                }}
                value={currentLevel->Level.name}
                className="block appearance-none w-full bg-white border text-sm border-gray-400 hover:border-gray-500 px-4 py-3 pr-8 rounded-r-none leading-tight focus:outline-none">
                {state.levels
                ->Level.sort
                ->List.map(level =>
                  <option key={Level.id(level)} value={level->Level.name}>
                    {"Level " ++
                    ((level->Level.number->string_of_int) ++
                    (": " ++ (level->Level.name)))->str}
                  </option>
                )
                ->Array.of_list
                ->ReasonReact.array}
              </select>
              <div
                className="pointer-events-none absolute inset-y-0 right-0 flex items-center px-3 text-gray-800">
                <i className="fas fa-chevron-down text-xs" />
              </div>
            </div>
            <button
              className="flex text-gray-600 hover:text-gray-900 text-sm font-bold border border-gray-400 border-l-0 py-1 px-2 rounded-r focus:outline-none"
              onClick={_ => send(UpdateEditorAction(ShowLevelEditor(Some(state.selectedLevel))))}>
              <i title="edit" className="fas fa-pencil-alt" />
            </button>
            <button
              className="btn btn-primary ml-4"
              onClick={_ => send(UpdateEditorAction(ShowLevelEditor(None)))}>
              <i className="fas fa-plus-square mr-2 text-lg" />
              <span> {"Create Level"->str} </span>
            </button>
          </div>
          {showArchivedButton(targetGroupsInLevel, state.targets)
            ? <button className="btn btn-default" onClick={_ => send(ToggleShowArchived)}>
                {(state.showArchived ? "Hide Archived" : "Show Archived")->str}
              </button>
            : ReasonReact.null}
        </div>
      </div>
      <div className="target-group__container max-w-3xl mt-5 mx-auto relative">
        {targetGroupsToDisplay
        ->List.mapi((index, targetGroup) =>
          <CurriculumEditor__TargetGroupShow
            key={targetGroup->TargetGroup.id}
            targetGroup
            targetGroups=targetGroupsToDisplay
            targets=state.targets
            showTargetGroupEditorCB
            updateTargetCB
            showArchived=state.showArchived
            updateTargetSortIndexCB={updateTargetSortIndex(state, send)}
            updateTargetGroupSortIndexCB={updateTargetGroupSortIndex(state, send)}
            index
            course
          />
        )
        ->Array.of_list
        ->ReasonReact.array}
        <div
          onClick={_ => send(UpdateEditorAction(ShowTargetGroupEditor(None)))}
          className="target-group__create flex flex-col items-center justify-center relative bg-white border-2 border-dashed border-gray-400 p-6 z-10 hover:text-primary-500 hover:shadow-lg hover:border-primary-400 hover:border-primary-400 rounded-lg mt-12 cursor-pointer">
          <span className="flex bg-gray-200 p-2 rounded-full">
            <i className="fas fa-plus-circle text-2xl" />
          </span>
          <h4 className="font-semibold ml-2"> {"Create a target group"->str} </h4>
        </div>
      </div>
    </div>
  </div>
}

%raw(`require("./CoursesCurriculum.css")`)

@module("../images/level-lock.svg")
external levelLockedImage: string = "default"

open CoursesCurriculum__Types

let str = React.string

type state = {
  selectedLevelId: string,
  showLevelZero: bool,
  latestSubmissions: list<LatestSubmission.t>,
  statusOfTargets: list<TargetStatus.t>,
  notice: Notice.t,
}

let targetStatusClasses = targetStatus => {
  let statusClasses =
    "curriculum__target-status--" ++
    (targetStatus->TargetStatus.statusToString->Js.String.toLowerCase)
  "curriculum__target-status px-3 py-px ml-4 h-6 " ++ statusClasses
}

let rendertarget = (target, statusOfTargets) => {
  let targetId = target->Target.id
  let targetStatus =
    statusOfTargets->ListUtils.unsafeFind(
      ts => ts->TargetStatus.targetId == targetId,
      "Could not find targetStatus for listed target with ID " ++ targetId,
    )

  <Link
    href={"/targets/" ++ targetId}
    key={"target-" ++ targetId}
    className="bg-white border-t p-6 flex items-center justify-between hover:bg-gray-200 hover:text-primary-500 cursor-pointer"
    ariaLabel={"Select Target " ++ targetId}>
    <span className="font-semibold text-left leading-snug"> {target->Target.title->str} </span>
    <span className={targetStatusClasses(targetStatus)}>
      {targetStatus->TargetStatus.statusToString->str}
    </span>
  </Link>
}

let renderTargetGroup = (targetGroup, targets, statusOfTargets) => {
  let targetGroupId = targetGroup->TargetGroup.id
  let targets = targets->List.filter(t => t->Target.targetGroupId == targetGroupId)

  <div
    key={"target-group-" ++ targetGroupId}
    className="curriculum__target-group-container relative mt-5 px-3">
    <div
      className="curriculum__target-group max-w-3xl mx-auto bg-white text-center rounded-lg shadow-md relative z-10 overflow-hidden ">
      {targetGroup->TargetGroup.milestone
        ? <div
            className="inline-block px-3 py-2 bg-orange-400 font-bold text-xs rounded-b-lg leading-tight text-white uppercase">
            {"Milestone targets"->str}
          </div>
        : React.null}
      <div className="p-6 pt-5">
        <div className="text-2xl font-bold leading-snug">
          {targetGroup->TargetGroup.name->str}
        </div>
        <div className="text-sm max-w-md mx-auto leading-snug mt-1">
          {targetGroup->TargetGroup.description->str}
        </div>
      </div>
      {targets
      ->List.sort((t1, t2) => (t1->Target.sortIndex) - (t2->Target.sortIndex))
      ->List.map(target => rendertarget(target, statusOfTargets))
      ->Array.of_list
      ->React.array}
    </div>
  </div>
}

let addSubmission = (setState, latestSubmission) =>
  setState(state => {
    let withoutSubmissionForThisTarget =
      state.latestSubmissions->List.filter(s =>
        s->LatestSubmission.targetId != (latestSubmission->LatestSubmission.targetId)
      )

    {
      ...state,
      latestSubmissions: list{latestSubmission, ...withoutSubmissionForThisTarget},
    }
  })

let handleLockedLevel = level =>
  <div className="max-w-xl mx-auto text-center mt-4">
    <div className="font-semibold text-2xl font-bold px-3"> {"Level Locked"->str} </div>
    <img className="max-w-sm mx-auto" src=levelLockedImage />
    {switch level->Level.unlockOn {
    | Some(date) =>
      let dateString = date->DateFns.parseString->DateFns.format("MMMM D, YYYY")
      <div className="font-semibold text-md px-3">
        <p> {"The level is currently locked!"->str} </p>
        <p> {"You can access the content on " ++ (dateString ++ ".")->str} </p>
      </div>
    | None => React.null
    }}
  </div>

let statusOfMilestoneTargets = (targetGroups, targets, level, statusOfTargets) => {
  let targetGroupsInLevel =
    targetGroups->List.filter(tg => tg->TargetGroup.levelId == (level->Level.id))
  let milestoneTargetGroupIds =
    targetGroupsInLevel
    ->List.filter(tg => tg->TargetGroup.milestone)
    ->List.map(tg => tg->TargetGroup.id)

  let milestoneTargetIds =
    targets
    ->List.filter(t => (t->Target.targetGroupId)->List.mem(milestoneTargetGroupIds))
    ->List.map(t => t->Target.id)

  statusOfTargets->List.filter(ts => (ts->TargetStatus.targetId)->List.mem(milestoneTargetIds))
}

let isLevelComplete = (targetStatuses, eligibleStatuses) =>
  targetStatuses->ListUtils.isNotEmpty &&
    targetStatuses->TargetStatus.matchesStatuses(eligibleStatuses)

let computeLevelUp = (
  levels,
  teamLevel,
  targetGroups,
  targets,
  statusOfTargets,
  accessLockedLevels,
) => {
  let lastLevel = switch teamLevel->Level.number {
  | 0
  | 1 =>
    None
  | levelTwoOrAbove => levels->ListUtils.findOpt(l => l->Level.number == levelTwoOrAbove - 1)
  }

  let statusOfCurrentMilestoneTargets = statusOfMilestoneTargets(
    targetGroups,
    targets,
    teamLevel,
    statusOfTargets,
  )

  let currentLevelComplete = isLevelComplete(
    statusOfCurrentMilestoneTargets,
    TargetStatus.currentLevelStatuses,
  )

  let lastLevelComplete = switch lastLevel {
  | None => true
  | Some(level) =>
    let statusOfLastMilestoneTargets = statusOfMilestoneTargets(
      targetGroups,
      targets,
      level,
      statusOfTargets,
    )

    isLevelComplete(statusOfLastMilestoneTargets, TargetStatus.lastLevelStatuses)
  }

  let nextLevel =
    levels->ListUtils.findOpt(l => l->Level.number == (teamLevel->Level.number) + 1)

  switch (nextLevel, currentLevelComplete, lastLevelComplete) {
  | (Some(level), true, true) =>
    level->Level.isUnlocked || accessLockedLevels ? Notice.LevelUp : Nothing
  | (None, true, true) => CourseComplete
  | (Some(_), true, false) => LevelUpBlocked(teamLevel->Level.number)
  | (Some(_) | None, false, false | true)
  | (None, true, false) =>
    Nothing
  }
}

let computeNotice = (
  levels,
  teamLevel,
  targetGroups,
  targets,
  statusOfTargets,
  course,
  team,
  preview,
  accessLockedLevels,
) =>
  switch (preview, course->Course.hasEnded, team->Team.accessEnded) {
  | (true, _, _) => Notice.Preview
  | (false, true, true | false) => CourseEnded
  | (false, false, true) => AccessEnded
  | (false, false, false) =>
    computeLevelUp(levels, teamLevel, targetGroups, targets, statusOfTargets, accessLockedLevels)
  }

let navigationLink = (direction, level, setState) => {
  let (leftIcon, longText, shortText, rightIcon) = switch direction {
  | #Previous => (Some("fa-arrow-left"), "Previous Level", "Previous", None)
  | #Next => (None, "Next Level", "Next", Some("fa-arrow-right"))
  }

  let arrow = icon =>
    icon->Belt.Option.mapWithDefault(React.null, icon => <FaIcon classes={"fas " ++ icon} />)

  <button
    onClick={_ => setState(state => {...state, selectedLevelId: level->Level.id})}
    className="block w-full focus:outline-none p-4 text-center border rounded-lg bg-gray-100 hover:bg-gray-200 cursor-pointer">
    {arrow(leftIcon)}
    <span className="mx-2 hidden md:inline"> {longText->str} </span>
    <span className="mx-2 inline md:hidden"> {shortText->str} </span>
    {arrow(rightIcon)}
  </button>
}

let quickNavigationLinks = (levels, selectedLevel, setState) => {
  let previous = selectedLevel->Level.previous(levels)
  let next = selectedLevel->Level.next(levels)

  <div>
    <hr className="my-6" />
    <div className="container mx-auto max-w-3xl flex px-3 lg:px-0">
      {switch (previous, next) {
      | (Some(previousLevel), Some(nextLevel)) =>
        [
          <div key="previous" className="w-1/2 mr-2">
            {navigationLink(#Previous, previousLevel, setState)}
          </div>,
          <div key="next" className="w-1/2 ml-2">
            {navigationLink(#Next, nextLevel, setState)}
          </div>,
        ]->React.array

      | (Some(previousUrl), None) =>
        <div className="w-full"> {navigationLink(#Previous, previousUrl, setState)} </div>
      | (None, Some(nextUrl)) =>
        <div className="w-full"> {navigationLink(#Next, nextUrl, setState)} </div>
      | (None, None) => React.null
      }}
    </div>
  </div>
}

@react.component
let make = (
  ~course,
  ~levels,
  ~targetGroups,
  ~targets,
  ~submissions,
  ~team,
  ~coaches,
  ~users,
  ~evaluationCriteria,
  ~preview,
  ~accessLockedLevels,
) => {
  let url = ReasonReactRouter.useUrl()

  let selectedTarget = switch url.path {
  | list{"targets", targetId, ..._} =>
    Some(
      targets->ListUtils.unsafeFind(
        t => t->Target.id == targetId,
        "Could not find selectedTarget with ID " ++ targetId,
      ),
    )
  | _ => None
  }

  /* Level selection is a bit complicated because of how the selector for L0 is
   * separate from the other levels. selectedLevelId is the numbered level
   * selected by the user, whereas showLevelZero is the toggle on the title of
   * L0 determining whether the user has picked it or not - it'll show up only
   * if L0 is available, and will override the selectedLevelId. This rule is
   * used to determine currentLevelId, which is the actual level whose contents
   * are shown on the page. */

  let levelZero = levels->ListUtils.findOpt(l => l->Level.number == 0)
  let teamLevelId = team->Team.levelId

  let teamLevel =
    levels->ListUtils.unsafeFind(
      l => l->Level.id == teamLevelId,
      "Could not find teamLevel with ID " ++ teamLevelId,
    )

  let targetLevelId = switch selectedTarget {
  | Some(target) =>
    let targetGroupId = target->Target.targetGroupId

    let targetGroup =
      targetGroups->ListUtils.unsafeFind(
        t => t->TargetGroup.id == targetGroupId,
        "Could not find targetGroup with ID " ++ targetGroupId,
      )

    Some(targetGroup->TargetGroup.levelId)
  | None => None
  }

  /* Curried function so that this can be re-used when a new submission is created. */
  let computeTargetStatus = TargetStatus.compute(
    preview,
    team,
    course,
    levels,
    targetGroups,
    targets,
  )

  let initialRender = React.useRef(true)

  let (state, setState) = React.useState(() => {
    let statusOfTargets = computeTargetStatus(submissions)

    {
      selectedLevelId: switch (preview, targetLevelId, levelZero) {
      | (true, None, None) => levels->Level.first->Level.id
      | (_, Some(targetLevelId), Some(levelZero)) =>
        levelZero->Level.id == targetLevelId ? teamLevelId : targetLevelId
      | (_, Some(targetLevelId), None) => targetLevelId
      | (_, None, _) => teamLevelId
      },
      showLevelZero: switch (levelZero, targetLevelId) {
      | (Some(levelZero), Some(targetLevelId)) => levelZero->Level.id == targetLevelId
      | (Some(_), None)
      | (None, Some(_))
      | (None, None) => false
      },
      latestSubmissions: submissions,
      statusOfTargets: statusOfTargets,
      notice: computeNotice(
        levels,
        teamLevel,
        targetGroups,
        targets,
        statusOfTargets,
        course,
        team,
        preview,
        accessLockedLevels,
      ),
    }
  })

  let currentLevelId = switch (levelZero, state.showLevelZero) {
  | (Some(levelZero), true) => levelZero->Level.id
  | (Some(_), false)
  | (None, true | false) =>
    state.selectedLevelId
  }

  let currentLevel =
    levels->ListUtils.unsafeFind(
      l => l->Level.id == currentLevelId,
      "Could not find currentLevel with id " ++ currentLevelId,
    )

  let selectedLevel =
    levels->ListUtils.unsafeFind(
      l => l->Level.id == state.selectedLevelId,
      "Could not find selectedLevel with id " ++ state.selectedLevelId,
    )

  React.useEffect1(() => {
    if initialRender->React.Ref.current {
      initialRender->React.Ref.setCurrent(false)
    } else {
      let newStatusOfTargets = computeTargetStatus(state.latestSubmissions)

      setState(state => {
        ...state,
        statusOfTargets: newStatusOfTargets,
        notice: computeNotice(
          levels,
          teamLevel,
          targetGroups,
          targets,
          newStatusOfTargets,
          course,
          team,
          preview,
          accessLockedLevels,
        ),
      })
    }
    None
  }, [state.latestSubmissions])

  let targetGroupsInLevel =
    targetGroups->List.filter(tg => tg->TargetGroup.levelId == currentLevelId)

  <div className="bg-gray-100 pt-11 pb-8 -mt-7">
    {switch selectedTarget {
    | Some(target) =>
      let targetStatus =
        state.statusOfTargets->ListUtils.unsafeFind(
          ts => ts->TargetStatus.targetId == (target->Target.id),
          "Could not find targetStatus for selectedTarget with ID " ++ (target->Target.id),
        )

      <CoursesCurriculum__Overlay
        target
        course
        targetStatus
        addSubmissionCB={addSubmission(setState)}
        targets
        statusOfTargets=state.statusOfTargets
        users
        evaluationCriteria
        coaches
        preview
      />

    | None => React.null
    }}
    <CoursesCurriculum__NoticeManager notice=state.notice course />
    {switch state.notice {
    | LevelUp => React.null
    | _anyOtherNotice =>
      <div className="relative">
        <CoursesCurriculum__LevelSelector
          levels
          teamLevel
          selectedLevel
          setSelectedLevelId={selectedLevelId =>
            setState(state => {...state, selectedLevelId: selectedLevelId})}
          showLevelZero=state.showLevelZero
          setShowLevelZero={showLevelZero =>
            setState(state => {...state, showLevelZero: showLevelZero})}
          levelZero
        />
        {currentLevel->Level.isLocked && accessLockedLevels
          ? <div className="text-center p-3 mt-5 border rounded-lg bg-blue-100 max-w-3xl mx-auto">
              {"This level is still locked for students, and will be unlocked on "->str}
              <strong> {currentLevel->Level.unlockDateString->str} </strong>
              {"."->str}
            </div>
          : React.null}
        {currentLevel->Level.isUnlocked || accessLockedLevels
          ? targetGroupsInLevel
            ->TargetGroup.sort
            ->List.map(targetGroup =>
              renderTargetGroup(targetGroup, targets, state.statusOfTargets)
            )
            ->Array.of_list
            ->React.array
          : handleLockedLevel(currentLevel)}
      </div>
    }}
    {state.showLevelZero ? React.null : quickNavigationLinks(levels, selectedLevel, setState)}
  </div>
}

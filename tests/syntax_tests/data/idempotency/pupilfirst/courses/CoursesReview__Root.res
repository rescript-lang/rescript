%raw(`require("./CoursesReview__Root.css")`)

open CoursesReview__Types
let str = React.string

type sortBy = {
  criterion: string,
  criterionType: [#String | #Number],
}

type selectedTab = [#Reviewed | #Pending]

type state = {
  pendingSubmissions: Submissions.t,
  reviewedSubmissions: Submissions.t,
  selectedTab: selectedTab,
  selectedLevel: option<Level.t>,
  selectedCoach: option<Coach.t>,
  filterString: string,
  sortBy: sortBy,
  sortDirection: [#Ascending | #Descending],
  reloadAt: Js.Date.t,
}

type action =
  | SelectLevel(Level.t)
  | DeselectLevel
  | ReloadSubmissions
  | SetSubmissions(array<IndexSubmission.t>, selectedTab, bool, option<string>, int)
  | UpdateReviewedSubmission(IndexSubmission.t)
  | SelectPendingTab
  | SelectReviewedTab
  | SelectCoach(Coach.t)
  | DeselectCoach
  | UpdateFilterString(string)
  | UpdateSortDirection([#Ascending | #Descending])
  | SyncSubmissionStatus(OverlaySubmission.t)

let reducer = (state, action) =>
  switch action {
  | SelectLevel(level) => {
      ...state,
      selectedLevel: Some(level),
      filterString: "",
    }
  | DeselectLevel => {...state, selectedLevel: None}
  | ReloadSubmissions => {
      ...state,
      pendingSubmissions: Unloaded,
      reviewedSubmissions: Unloaded,
      reloadAt: Js.Date.make(),
    }
  | SetSubmissions(submissions, selectedTab, hasNextPage, endCursor, totalCount) =>
    let filter = Submissions.makeFilter(state.selectedLevel, state.selectedCoach)

    let updatedSubmissions = switch (hasNextPage, endCursor) {
    | (_, None)
    | (false, Some(_)) =>
      Submissions.fullyLoaded(
        ~submissions,
        ~filter,
        ~sortDirection=state.sortDirection,
        ~totalCount,
      )
    | (true, Some(cursor)) =>
      Submissions.partiallyLoaded(
        ~submissions,
        ~filter,
        ~sortDirection=state.sortDirection,
        ~totalCount,
        ~cursor,
      )
    }

    switch selectedTab {
    | #Pending => {...state, pendingSubmissions: updatedSubmissions}
    | #Reviewed => {...state, reviewedSubmissions: updatedSubmissions}
    }
  | UpdateReviewedSubmission(submission) =>
    let filter = Submissions.makeFilter(state.selectedLevel, state.selectedCoach)

    {
      ...state,
      reviewedSubmissions: switch state.reviewedSubmissions {
      | Unloaded => Unloaded
      | PartiallyLoaded({submissions, totalCount}, cursor) =>
        Submissions.partiallyLoaded(
          ~submissions=submissions->IndexSubmission.replace(submission),
          ~filter,
          ~sortDirection=state.sortDirection,
          ~totalCount,
          ~cursor,
        )
      | FullyLoaded({submissions, totalCount}) =>
        Submissions.fullyLoaded(
          ~submissions=submissions->IndexSubmission.replace(submission),
          ~filter,
          ~totalCount,
          ~sortDirection=state.sortDirection,
        )
      },
    }
  | SelectPendingTab => {...state, selectedTab: #Pending}
  | SelectReviewedTab => {...state, selectedTab: #Reviewed}
  | SelectCoach(coach) => {
      ...state,
      selectedCoach: Some(coach),
      filterString: "",
    }
  | DeselectCoach => {...state, selectedCoach: None}
  | UpdateFilterString(filterString) => {...state, filterString: filterString}
  | UpdateSortDirection(sortDirection) => {...state, sortDirection: sortDirection}
  | SyncSubmissionStatus(overlaySubmission) =>
    let skipReload =
      state.pendingSubmissions
      ->Submissions.toArray
      ->Array.append(state.reviewedSubmissions->Submissions.toArray)
      ->Js.Array.find(indexSubmission =>
        indexSubmission->IndexSubmission.id == OverlaySubmission.id(overlaySubmission)
      )
      ->OptionUtils.mapWithDefault(
        indexSubmission => indexSubmission->IndexSubmission.statusEq(overlaySubmission),
        true,
      )

    skipReload
      ? state
      : {
          ...state,
          pendingSubmissions: Unloaded,
          reviewedSubmissions: Unloaded,
          reloadAt: Js.Date.make(),
        }
  }

let computeInitialState = currentTeamCoach => {
  pendingSubmissions: Unloaded,
  reviewedSubmissions: Unloaded,
  selectedTab: #Pending,
  selectedLevel: None,
  selectedCoach: currentTeamCoach,
  filterString: "",
  sortBy: {
    criterion: "Submitted At",
    criterionType: #Number,
  },
  sortDirection: #Descending,
  reloadAt: Js.Date.make(),
}

let openOverlay = (submissionId, event) => {
  event->ReactEvent.Mouse.preventDefault
  ReasonReactRouter.push("/submissions/" ++ submissionId)
}

let dropdownElementClasses = (level, selectedLevel) =>
  "p-3 w-full text-left font-semibold focus:outline-none " ++
  switch (selectedLevel, level) {
  | (Some(sl), Some(l)) if l->Level.id == (sl->Level.id) => "bg-gray-200 text-primary-500"
  | (None, None) => "bg-gray-200 text-primary-500"
  | _ => ""
  }

let buttonClasses = selected =>
  "w-1/2 md:w-auto py-2 px-3 md:px-6 font-semibold text-sm focus:outline-none " ++ (
    selected
      ? "bg-primary-100 shadow-inner text-primary-500"
      : "bg-white shadow-md hover:shadow hover:text-primary-500 hover:bg-gray-100"
  )

module Selectable = {
  type t =
    | Level(Level.t)
    | AssignedToCoach(Coach.t, string)

  let label = t =>
    switch t {
    | Level(level) => Some("Level " ++ (level->Level.number->string_of_int))
    | AssignedToCoach(_) => Some("Assigned to")
    }

  let value = t =>
    switch t {
    | Level(level) => level->Level.name
    | AssignedToCoach(coach, currentCoachId) =>
      coach->Coach.id == currentCoachId ? "Me" : coach->Coach.name
    }

  let searchString = t =>
    switch t {
    | Level(level) =>
      "level " ++ ((level->Level.number->string_of_int) ++ (" " ++ (level->Level.name)))
    | AssignedToCoach(coach, currentCoachId) =>
      if coach->Coach.id == currentCoachId {
        (coach->Coach.name) ++ " assigned to me"
      } else {
        "assigned to " ++ (coach->Coach.name)
      }
    }

  let color = _t => "gray"
  let level = level => Level(level)
  let assignedToCoach = (coach, currentCoachId) => AssignedToCoach(coach, currentCoachId)
}

module Multiselect = MultiselectDropdown.Make(Selectable)

let unselected = (levels, coaches, currentCoachId, state) => {
  let unselectedLevels =
    levels
    ->Js.Array.filter(level =>
      state.selectedLevel->OptionUtils.mapWithDefault(
        selectedLevel => level->Level.id != (selectedLevel->Level.id),
        true,
      )
    )
    ->Array.map(Selectable.level)

  let unselectedCoaches =
    coaches
    ->Js.Array.filter(coach =>
      state.selectedCoach->OptionUtils.mapWithDefault(
        selectedCoach => coach->Coach.id != Coach.id(selectedCoach),
        true,
      )
    )
    ->Array.map(coach => Selectable.assignedToCoach(coach, currentCoachId))

  unselectedLevels->Array.append(unselectedCoaches)
}

let selected = (state, currentCoachId) => {
  let selectedLevel =
    state.selectedLevel->OptionUtils.mapWithDefault(
      selectedLevel => [Selectable.level(selectedLevel)],
      [],
    )

  let selectedCoach =
    state.selectedCoach->OptionUtils.mapWithDefault(
      selectedCoach => [Selectable.assignedToCoach(selectedCoach, currentCoachId)],
      [],
    )

  selectedLevel->Array.append(selectedCoach)
}

let onSelectFilter = (send, selectable) =>
  switch selectable {
  | Selectable.AssignedToCoach(coach, _currentCoachId) => send(SelectCoach(coach))
  | Level(level) => send(SelectLevel(level))
  }

let onDeselectFilter = (send, selectable) =>
  switch selectable {
  | Selectable.AssignedToCoach(_) => send(DeselectCoach)
  | Level(_) => send(DeselectLevel)
  }

let filterPlaceholder = state =>
  switch (state.selectedLevel, state.selectedCoach) {
  | (None, Some(_)) => "Filter by level"
  | (None, None) => "Filter by level, or only show submissions assigned to a coach"
  | (Some(_), Some(_)) => "Filter by another level"
  | (Some(_), None) => "Filter by another level, or only show submissions assigned to a coach"
  }

let restoreFilterNotice = (send, currentCoach, message) =>
  <div
    className="mb-4 text-sm italic flex flex-col md:flex-row items-center justify-between p-3 border border-gray-300 bg-white rounded-lg">
    <span> {message->str} </span>
    <button
      className="px-2 py-1 rounded text-xs overflow-hidden border border-gray-300 bg-gray-200 text-gray-800 border-gray-300 bg-gray-200 hover:bg-gray-300 mt-1 md:mt-0"
      onClick={_ => send(SelectCoach(currentCoach))}>
      {"Assigned to: Me"->str} <i className="fas fa-level-up-alt ml-2" />
    </button>
  </div>

let restoreAssignedToMeFilter = (state, send, currentTeamCoach) =>
  currentTeamCoach->OptionUtils.mapWithDefault(currentCoach =>
    switch state.selectedCoach {
    | None =>
      restoreFilterNotice(
        send,
        currentCoach,
        "Now showing submissions from all students in this course.",
      )
    | Some(selectedCoach) if selectedCoach->Coach.id == Coach.id(currentCoach) => React.null
    | Some(selectedCoach) =>
      restoreFilterNotice(
        send,
        currentCoach,
        "Now showing submissions assigned to " ++ ((selectedCoach->Coach.name) ++ "."),
      )
    }
  , React.null)

let filterSubmissions = (selectedLevel, selectedCoach, submissions) => {
  let levelFiltered =
    selectedLevel->OptionUtils.mapWithDefault(
      level =>
        submissions->Js.Array.filter(l => l->IndexSubmission.levelId == (level->Level.id)),
      submissions,
    )

  selectedCoach->OptionUtils.mapWithDefault(
    coach =>
      levelFiltered->Js.Array.filter(l =>
        l->IndexSubmission.coachIds->Array.mem(coach->Coach.id)
      ),
    levelFiltered,
  )
}

module Sortable = {
  type t = sortBy

  let criterion = t => t.criterion
  let criterionType = t => t.criterionType
}

module SubmissionsSorter = Sorter.Make(Sortable)

let submissionsSorter = (state, send) => {
  let criteria = [{criterion: "Submitted At", criterionType: #Number}]
  <div ariaLabel="Change submissions sorting" className="flex-shrink-0 mt-3 md:mt-0 md:ml-2">
    <label className="block text-tiny font-semibold uppercase"> {"Sort by:"->str} </label>
    <SubmissionsSorter
      criteria
      selectedCriterion=state.sortBy
      direction=state.sortDirection
      onDirectionChange={sortDirection => send(UpdateSortDirection(sortDirection))}
      onCriterionChange={_ => ()}
    />
  </div>
}

let displayedSubmissions = state =>
  switch state.selectedTab {
  | #Pending => state.pendingSubmissions
  | #Reviewed => state.reviewedSubmissions
  }

let submissionsCount = submissions =>
  submissions
  ->Submissions.totalCount
  ->OptionUtils.mapWithDefault(
    count =>
      <span
        className="course-review__status-tab-badge ml-2 text-white text-xs bg-red-500 w-auto h-5 px-1 inline-flex items-center justify-center rounded-full">
        {count->string_of_int->str}
      </span>,
    React.null,
  )

@react.component
let make = (~levels, ~courseId, ~teamCoaches, ~currentCoach) => {
  let (currentTeamCoach, _) = React.useState(() =>
    teamCoaches->Belt.Array.some(coach => coach->Coach.id == (currentCoach->Coach.id))
      ? Some(currentCoach)
      : None
  )

  let (state, send) = React.useReducerWithMapState(reducer, currentTeamCoach, computeInitialState)

  let url = ReasonReactRouter.useUrl()

  <div>
    {switch url.path {
    | list{"submissions", submissionId, ..._} =>
      <CoursesReview__SubmissionOverlay
        courseId
        submissionId
        currentCoach
        teamCoaches
        syncSubmissionCB={submission => send(SyncSubmissionStatus(submission))}
        removePendingSubmissionCB={() => send(ReloadSubmissions)}
        updateReviewedSubmissionCB={submission => send(UpdateReviewedSubmission(submission))}
      />
    | _ => React.null
    }}
    <div className="bg-gray-100 pt-9 pb-8 px-3 -mt-7">
      <div className="bg-gray-100 static md:sticky md:top-0">
        <div className="max-w-3xl mx-auto">
          <div className="flex flex-col md:flex-row items-end lg:items-center py-4">
            <div
              ariaLabel="status-tab"
              className="course-review__status-tab w-full md:w-auto flex rounded-lg border border-gray-400">
              <button
                className={buttonClasses(state.selectedTab == #Pending)}
                onClick={_ => send(SelectPendingTab)}>
                {"Pending"->str} {submissionsCount(state.pendingSubmissions)}
              </button>
              <button
                className={buttonClasses(state.selectedTab == #Reviewed)}
                onClick={_ => send(SelectReviewedTab)}>
                {"Reviewed"->str}
              </button>
            </div>
          </div>
          <div className="md:flex w-full items-start pb-4">
            <div className="flex-1">
              <label className="block text-tiny font-semibold uppercase">
                {"Filter by:"->str}
              </label>
              <Multiselect
                id="filter"
                unselected={unselected(levels, teamCoaches, currentCoach->Coach.id, state)}
                selected={selected(state, currentCoach->Coach.id)}
                onSelect={onSelectFilter(send)}
                onDeselect={onDeselectFilter(send)}
                value=state.filterString
                onChange={filterString => send(UpdateFilterString(filterString))}
                placeholder={filterPlaceholder(state)}
              />
            </div>
            {submissionsSorter(state, send)}
          </div>
        </div>
      </div>
      <div className="max-w-3xl mx-auto">
        {restoreAssignedToMeFilter(state, send, currentTeamCoach)}
      </div>
      <div className="max-w-3xl mx-auto">
        <CoursesReview__SubmissionsTab
          courseId
          selectedTab=state.selectedTab
          selectedLevel=state.selectedLevel
          selectedCoach=state.selectedCoach
          sortDirection=state.sortDirection
          levels
          submissions={displayedSubmissions(state)}
          reloadAt=state.reloadAt
          updateSubmissionsCB={(
            ~submissions,
            ~selectedTab,
            ~hasNextPage,
            ~totalCount,
            ~endCursor,
          ) => send(SetSubmissions(submissions, selectedTab, hasNextPage, endCursor, totalCount))}
        />
      </div>
    </div>
  </div>
}

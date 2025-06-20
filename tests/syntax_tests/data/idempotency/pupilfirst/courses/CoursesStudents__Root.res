%raw(`require("./CoursesStudents__Root.css")`)

open CoursesStudents__Types
let str = React.string

type filter = {
  nameOrEmail: option<string>,
  level: option<Level.t>,
  coach: option<Coach.t>,
}

type loading =
  | NotLoading
  | Reloading
  | LoadingMore

type state = {
  loading: loading,
  teams: Teams.t,
  filterString: string,
  filter: filter,
}

type action =
  | SelectLevel(Level.t)
  | DeselectLevel
  | SelectCoach(Coach.t)
  | DeselectCoach
  | SetNameOrEmail(string)
  | UnsetNameOrEmail
  | UpdateFilterString(string)
  | LoadTeams(option<string>, bool, array<TeamInfo.t>)
  | BeginLoadingMore
  | BeginReloading

let reducer = (state, action) =>
  switch action {
  | SelectLevel(level) => {
      ...state,
      filter: {
        ...state.filter,
        level: Some(level),
      },
      filterString: "",
    }
  | DeselectLevel => {
      ...state,
      filter: {
        ...state.filter,
        level: None,
      },
    }
  | SelectCoach(coach) => {
      ...state,
      filter: {
        ...state.filter,
        coach: Some(coach),
      },
      filterString: "",
    }
  | DeselectCoach => {
      ...state,
      filter: {
        ...state.filter,
        coach: None,
      },
    }
  | SetNameOrEmail(search) => {
      ...state,
      filter: {
        ...state.filter,
        nameOrEmail: Some(search),
      },
      filterString: "",
    }
  | UnsetNameOrEmail => {
      ...state,
      filter: {
        ...state.filter,
        nameOrEmail: None,
      },
    }
  | UpdateFilterString(filterString) => {...state, filterString: filterString}
  | LoadTeams(endCursor, hasNextPage, newTeams) =>
    let updatedTeams = switch state.loading {
    | LoadingMore => newTeams->Array.append(state.teams->Teams.toArray)
    | Reloading => newTeams
    | NotLoading => newTeams
    }

    {
      ...state,
      teams: switch (hasNextPage, endCursor) {
      | (_, None)
      | (false, Some(_)) =>
        FullyLoaded(updatedTeams)
      | (true, Some(cursor)) => PartiallyLoaded(updatedTeams, cursor)
      },
      loading: NotLoading,
    }
  | BeginLoadingMore => {...state, loading: LoadingMore}
  | BeginReloading => {...state, loading: Reloading}
  }

module TeamsQuery = %graphql(`
    query TeamsFromCoursesStudentsRootQuery($courseId: ID!, $levelId: ID, $coachId: ID, $search: String, $after: String) {
      teams(courseId: $courseId, levelId: $levelId,coachId: $coachId, search: $search, first: 10, after: $after) {
        nodes {
          id,
          name,
          levelId,
          students {
            id,
            name
            title
            avatarUrl
          }
          coachUserIds
          accessEndsAt
          droppedOutAt
        }
        pageInfo{
          endCursor,hasNextPage
        }
      }
    }
  `)

let getTeams = (send, courseId, cursor, filter) => {
  let levelId = filter.level->OptionUtils.map(Level.id)
  let coachId = filter.coach->OptionUtils.map(Coach.id)

  TeamsQuery.make(~courseId, ~after=?cursor, ~levelId?, ~coachId?, ~search=?filter.nameOrEmail, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(response => {
    let newTeams = switch response["teams"]["nodes"] {
    | None => []
    | Some(teamsArray) => teamsArray->TeamInfo.makeArrayFromJs
    }

    send(
      LoadTeams(
        response["teams"]["pageInfo"]["endCursor"],
        response["teams"]["pageInfo"]["hasNextPage"],
        newTeams,
      ),
    )

    Js.Promise.resolve()
  })
  ->ignore
}

let applicableLevels = levels => levels->Js.Array.filter(level => Level.number(level) != 0)

module Selectable = {
  type t =
    | Level(Level.t)
    | AssignedToCoach(Coach.t, string)
    | NameOrEmail(string)

  let label = t =>
    switch t {
    | Level(level) => Some("Level " ++ (level->Level.number->string_of_int))
    | AssignedToCoach(_) => Some("Assigned to")
    | NameOrEmail(_) => Some("Name or Email")
    }

  let value = t =>
    switch t {
    | Level(level) => level->Level.name
    | AssignedToCoach(coach, currentCoachId) =>
      coach->Coach.id == currentCoachId ? "Me" : coach->Coach.name
    | NameOrEmail(search) => search
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
    | NameOrEmail(search) => search
    }

  let color = _t => "gray"
  let level = level => Level(level)
  let assignedToCoach = (coach, currentCoachId) => AssignedToCoach(coach, currentCoachId)
  let nameOrEmail = search => NameOrEmail(search)
}

module Multiselect = MultiselectDropdown.Make(Selectable)

let unselected = (levels, coaches, currentCoachId, state) => {
  let unselectedLevels =
    levels
    ->Js.Array.filter(level =>
      state.filter.level->OptionUtils.mapWithDefault(
        selectedLevel => level->Level.id != (selectedLevel->Level.id),
        true,
      )
    )
    ->Array.map(Selectable.level)

  let unselectedCoaches =
    coaches
    ->Js.Array.filter(coach =>
      state.filter.coach->OptionUtils.mapWithDefault(
        selectedCoach => coach->Coach.id != Coach.id(selectedCoach),
        true,
      )
    )
    ->Array.map(coach => Selectable.assignedToCoach(coach, currentCoachId))

  let trimmedFilterString = state.filterString->String.trim
  let nameOrEmail = trimmedFilterString == "" ? [] : [Selectable.nameOrEmail(trimmedFilterString)]

  unselectedLevels->Array.append(unselectedCoaches)->Array.append(nameOrEmail)
}

let selected = (state, currentCoachId) => {
  let selectedLevel =
    state.filter.level->OptionUtils.mapWithDefault(
      selectedLevel => [Selectable.level(selectedLevel)],
      [],
    )

  let selectedCoach =
    state.filter.coach->OptionUtils.mapWithDefault(
      selectedCoach => [Selectable.assignedToCoach(selectedCoach, currentCoachId)],
      [],
    )

  let selectedSearchString =
    state.filter.nameOrEmail->OptionUtils.mapWithDefault(
      nameOrEmail => [Selectable.nameOrEmail(nameOrEmail)],
      [],
    )

  selectedLevel->Array.append(selectedCoach)->Array.append(selectedSearchString)
}

let onSelectFilter = (send, selectable) =>
  switch selectable {
  | Selectable.AssignedToCoach(coach, _currentCoachId) => send(SelectCoach(coach))
  | Level(level) => send(SelectLevel(level))
  | NameOrEmail(nameOrEmail) => send(SetNameOrEmail(nameOrEmail))
  }

let onDeselectFilter = (send, selectable) =>
  switch selectable {
  | Selectable.AssignedToCoach(_) => send(DeselectCoach)
  | Level(_) => send(DeselectLevel)
  | NameOrEmail(_) => send(UnsetNameOrEmail)
  }

let filterPlaceholder = state =>
  switch (state.filter.level, state.filter.coach, state.filter.nameOrEmail) {
  | (None, None, None) => "Filter by level, assigned coach, or search by name or email address"
  | _ => ""
  }

let restoreFilterNotice = (send, currentCoach, message) =>
  <div
    className="mt-2 text-sm italic flex flex-col md:flex-row items-center justify-between p-3 border border-gray-300 bg-white rounded-lg">
    <span> {message->str} </span>
    <button
      className="px-2 py-1 rounded text-xs overflow-hidden border border-gray-300 bg-gray-200 text-gray-800 border-gray-300 bg-gray-200 hover:bg-gray-300 mt-1 md:mt-0"
      onClick={_ => send(SelectCoach(currentCoach))}>
      {"Assigned to: Me"->str} <i className="fas fa-level-up-alt ml-2" />
    </button>
  </div>

let restoreAssignedToMeFilter = (state, send, currentTeamCoach) =>
  currentTeamCoach->OptionUtils.mapWithDefault(currentCoach =>
    switch state.filter.coach {
    | None => restoreFilterNotice(send, currentCoach, "Now showing all students in this course.")
    | Some(selectedCoach) if selectedCoach->Coach.id == Coach.id(currentCoach) => React.null
    | Some(selectedCoach) =>
      restoreFilterNotice(
        send,
        currentCoach,
        "Now showing students assigned to " ++ ((selectedCoach->Coach.name) ++ "."),
      )
    }
  , React.null)

let computeInitialState = currentTeamCoach => {
  loading: NotLoading,
  teams: Unloaded,
  filterString: "",
  filter: {
    nameOrEmail: None,
    level: None,
    coach: currentTeamCoach,
  },
}

@react.component
let make = (~levels, ~course, ~userId, ~teamCoaches, ~currentCoach) => {
  let (currentTeamCoach, _) = React.useState(() =>
    teamCoaches->Belt.Array.some(coach => coach->Coach.id == (currentCoach->Coach.id))
      ? Some(currentCoach)
      : None
  )

  let (state, send) = React.useReducerWithMapState(reducer, currentTeamCoach, computeInitialState)

  let courseId = course->Course.id

  let url = ReasonReactRouter.useUrl()

  React.useEffect1(() => {
    send(BeginReloading)
    getTeams(send, courseId, None, state.filter)

    None
  }, [state.filter])

  <div>
    {switch url.path {
    | list{"students", studentId, "report"} =>
      <CoursesStudents__StudentOverlay courseId studentId levels userId teamCoaches />
    | _ => React.null
    }}
    <div className="bg-gray-100 pt-8 pb-8 px-3 -mt-7">
      <CoursesStudents__LevelDistribution
        levels selectLevelCB={level => send(SelectLevel(level))}
      />
      <div className="w-full py-4 bg-gray-100 relative md:sticky md:top-0 z-10">
        <div className="max-w-3xl mx-auto bg-gray-100 sticky md:static md:top-0">
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
          {restoreAssignedToMeFilter(state, send, currentTeamCoach)}
        </div>
      </div>
      <div className=" max-w-3xl mx-auto">
        {switch state.teams {
        | Unloaded => SkeletonLoading.multiple(~count=10, ~element=SkeletonLoading.userCard())
        | PartiallyLoaded(teams, cursor) =>
          <div>
            <CoursesStudents__TeamsList levels teams teamCoaches />
            {switch state.loading {
            | LoadingMore => SkeletonLoading.multiple(~count=3, ~element=SkeletonLoading.card())
            | NotLoading =>
              <button
                className="btn btn-primary-ghost cursor-pointer w-full mt-4"
                onClick={_ => {
                  send(BeginLoadingMore)
                  getTeams(send, courseId, Some(cursor), state.filter)
                }}>
                {"Load More..."->str}
              </button>
            | Reloading => React.null
            }}
          </div>
        | FullyLoaded(teams) => <CoursesStudents__TeamsList levels teams teamCoaches />
        }}
      </div>
    </div>
    {switch state.teams {
    | Unloaded => React.null

    | _ =>
      let loading = switch state.loading {
      | NotLoading => false
      | Reloading => true
      | LoadingMore => false
      }
      <LoadingSpinner loading />
    }}
  </div>
}

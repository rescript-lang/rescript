open InactiveStudentsPanel__Types

let str = React.string

type state = {
  teams: list<Team.t>,
  students: list<Student.t>,
  selectedTeams: list<Team.t>,
  searchString: string,
}

type action =
  | RefreshData(list<Team.t>)
  | SelectTeam(Team.t)
  | DeselectTeam(Team.t)
  | UpdateSearchString(string)

let reducer = (state, action) =>
  switch action {
  | RefreshData(teams) => {...state, teams: teams, selectedTeams: list{}}
  | SelectTeam(team) => {
      ...state,
      selectedTeams: list{team, ...state.selectedTeams},
    }
  | DeselectTeam(team) => {
      ...state,
      selectedTeams: state.selectedTeams->List.filter(s => Team.id(s) !== Team.id(team)),
    }
  | UpdateSearchString(searchString) => {...state, searchString: searchString}
  }

let studentsInTeam = (students, team) =>
  students->List.filter(student => Student.teamId(student) === Team.id(team))

let markActive = (teams, courseId, responseCB, authenticityToken) => {
  let payload = Js.Dict.empty()
  Js.Dict.set(payload, "authenticity_token", authenticityToken->Js.Json.string)
  Js.Dict.set(
    payload,
    "team_ids",
    teams
    ->List.map(s => s->Team.id->int_of_string)
    ->{
      open Json.Encode
      list(int)
    },
  )
  let url = "/school/courses/" ++ (courseId ++ "/mark_teams_active")
  Api.create(url, payload, responseCB, () => ())
}

let handleActiveTeamResponse = (send, state, json) => {
  let message = json->{
    open Json.Decode
    field("message", string)
  }
  let updatedTeams =
    state.teams->List.filter(team =>
      !(state.selectedTeams->List.exists(removedTeam => Team.id(team) === Team.id(removedTeam)))
    )
  send(RefreshData(updatedTeams))
  Notification.success("Success!", message)
}

@react.component
let make = (~teams, ~courseId, ~students, ~authenticityToken, ~isLastPage, ~currentPage) => {
  let (state, send) = React.useReducer(
    reducer,
    {teams: teams, students: students, selectedTeams: list{}, searchString: ""},
  )

  <div className="flex flex-1 px-6 pb-4 flex-col bg-gray-100 overflow-y-scroll">
    <div className="max-w-3xl w-full mx-auto flex justify-between border-b mt-4">
      <ul className="flex font-semibold text-sm">
        <li
          className="rounded-t-lg cursor-pointer border-b-3 border-transparent hover:bg-gray-200 hover:text-gray-900">
          <a
            className="block px-3 py-3 md:py-2 text-gray-800"
            href={"/school/courses/" ++ (courseId ++ "/students")}>
            {"All Students"->str}
          </a>
        </li>
        <li className="px-3 py-3 md:py-2 text-primary-500 border-b-3 border-primary-500 -mb-px">
          <span> {"Inactive Students"->str} </span>
        </li>
      </ul>
    </div>
    <div className="bg-gray-100 sticky top-0 py-3">
      <div className="border rounded-lg mx-auto max-w-3xl bg-white ">
        <div className="flex w-full items-center justify-between p-4">
          <div className="flex flex-1">
            <input
              id="search"
              type_="search"
              className="text-sm bg-white border border-gray-400 rounded leading-relaxed max-w-xs w-full py-2 px-3 mr-2 focus:outline-none focus:bg-white focus:border-primary-300"
              placeholder="Search by student or team name..."
              value=state.searchString
              onChange={event => send(UpdateSearchString(ReactEvent.Form.target(event)["value"]))}
            />
            <a
              className="btn btn-default no-underline"
              href={"/school/courses/" ++
              (courseId ++
              ("/inactive_students?search=" ++ state.searchString))}>
              {"Search"->str}
            </a>
          </div>
          {state.selectedTeams->ListUtils.isEmpty
            ? React.null
            : <button
                onClick={_e =>
                  markActive(
                    state.selectedTeams,
                    courseId,
                    handleActiveTeamResponse(send, state),
                    authenticityToken,
                  )}
                className="btn btn-success ml-3 mr-3 focus:outline-none">
                {"Reactivate Students"->str}
              </button>}
        </div>
      </div>
    </div>
    <div className="max-w-3xl mx-auto w-full">
      {state.teams->List.length > 0
        ? state.teams
          ->List.sort((team1, team2) =>
            (team2->Team.id->int_of_string) - (team1->Team.id->int_of_string)
          )
          ->List.map(team => {
            let isSingleFounder = team->studentsInTeam(state.students)->List.length == 1

            <div
              key={team->Team.id}
              id={team->Team.name}
              className="student-team-container flex items-center shadow bg-white rounded-lg mb-4 overflow-hidden">
              {
                let isChecked = state.selectedTeams->List.mem(team)
                let checkboxId = "select-team-" ++ (team->Team.id)
                <label
                  className="flex self-stretch items-center text-grey leading-tight font-bold px-4 py-5 hover:bg-gray-100"
                  htmlFor=checkboxId>
                  <input
                    className="leading-tight"
                    type_="checkbox"
                    id=checkboxId
                    checked=isChecked
                    onChange={isChecked
                      ? _e => send(DeselectTeam(team))
                      : _e => send(SelectTeam(team))}
                  />
                </label>
              }
              <div className="flex-1 w-3/5 order-last border-l">
                {team
                ->studentsInTeam(state.students)
                ->List.map(student =>
                  <div
                    key={student->Student.id}
                    id={student->Student.name}
                    className="student-team__card flex items-center bg-white pl-4">
                    <div className="flex-1 w-3/5">
                      <div className="flex items-center">
                        <a
                          className="flex flex-1 self-stretch items-center py-4 pr-4"
                          id={(student->Student.name) ++ "_edit"}>
                          <img
                            className="w-10 h-10 rounded-full mr-4 object-cover"
                            src={student->Student.avatarUrl}
                          />
                          <div className="text-sm flex flex-col">
                            <p className="text-black font-semibold inline-block ">
                              {student->Student.name->str}
                            </p>
                          </div>
                        </a>
                      </div>
                    </div>
                  </div>
                )
                ->Array.of_list
                ->React.array}
              </div>
              {isSingleFounder
                ? React.null
                : <div className="flex w-2/5 items-center">
                    <div className="w-3/5 py-4 px-3">
                      <div className="students-team--name mb-5">
                        <p className="mb-1 text-xs"> {"Team"->str} </p>
                        <h4> {team->Team.name->str} </h4>
                      </div>
                    </div>
                  </div>}
            </div>
          })
          ->Array.of_list
          ->React.array
        : <div className="shadow bg-white rounded-lg mb-4 p-4">
            {"No inactive student matches your search criteria."->str}
          </div>}
      {teams->ListUtils.isNotEmpty
        ? <div className="max-w-3xl w-full flex flex-row mx-auto justify-center pb-8">
            {currentPage > 1
              ? <a
                  className="block btn btn-default no-underline border shadow mx-2"
                  href={"/school/courses/" ++
                  (courseId ++
                  ("/inactive_students?page=" ++ (currentPage - 1->string_of_int)))}>
                  <i className="fas fa-arrow-left" />
                  <span className="ml-2"> {"Prev"->str} </span>
                </a>
              : React.null}
            {isLastPage
              ? React.null
              : <a
                  className="block btn btn-default no-underline border shadow mx-2"
                  href={"/school/courses/" ++
                  (courseId ++
                  ("/inactive_students?page=" ++ (currentPage + 1->string_of_int)))}>
                  <span className="mr-2"> {"Next"->str} </span>
                  <i className="fas fa-arrow-right" />
                </a>}
          </div>
        : React.null}
    </div>
  </div>
}

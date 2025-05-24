open StudentsEditor__Types

type rec teamCoachlist = (coachId, coachName, selected)
and coachId = string
and coachName = string
and selected = bool

type state = {
  name: string,
  teamName: string,
  tagsToApply: array<string>,
  teamCoaches: array<string>,
  teamCoachSearchInput: string,
  excludedFromLeaderboard: bool,
  title: string,
  affiliation: string,
  saving: bool,
  accessEndsAt: option<Js.Date.t>,
}

type action =
  | UpdateName(string)
  | UpdateTeamName(string)
  | AddTag(string)
  | RemoveTag(string)
  | UpdateCoachesList(array<string>)
  | UpdateCoachSearchInput(string)
  | UpdateExcludedFromLeaderboard(bool)
  | UpdateTitle(string)
  | UpdateAffiliation(string)
  | UpdateSaving(bool)
  | UpdateAccessEndsAt(option<Js.Date.t>)

let str = ReasonReact.string

let stringInputInvalid = s => s->String.length < 2

let updateName = (send, name) => send(UpdateName(name))

let updateTeamName = (send, teamName) => send(UpdateTeamName(teamName))

let updateTitle = (send, title) => send(UpdateTitle(title))

let formInvalid = state =>
  state.name->stringInputInvalid ||
    (state.teamName->stringInputInvalid ||
    state.title->stringInputInvalid)

let handleErrorCB = (send, ()) => send(UpdateSaving(false))

let successMessage = (accessEndsAt, isSingleFounder) =>
  switch accessEndsAt {
  | Some(date) =>
    switch (date->DateFns.isBefore(Js.Date.make()), isSingleFounder) {
    | (true, true) => "Student has been updated, and moved to list of inactive students"
    | (true, false) => "Team has been updated, and moved to list of inactive students"
    | (false, true)
    | (false, false) => "Student updated successfully"
    }
  | None => "Student updated successfully"
  }

let enrolledCoachIds = teamCoaches =>
  teamCoaches
  ->Js.Array.filter(((_, _, selected)) => selected == true)
  ->Array.map(((key, _, _)) => key)

let handleResponseCB = (updateFormCB, state, student, oldTeam, _json) => {
  let affiliation = switch state.affiliation->String.trim {
  | "" => None
  | text => Some(text)
  }
  let newStudent = Student.update(
    ~name=state.name,
    ~tags=state.tagsToApply,
    ~excludedFromLeaderboard=state.excludedFromLeaderboard,
    ~title=state.title,
    ~affiliation,
    ~student,
  )
  let newTeam = Team.update(
    ~name=state.teamName,
    ~student=newStudent,
    ~coachIds=state.teamCoaches,
    ~accessEndsAt=state.accessEndsAt,
    ~team=oldTeam,
  )

  // Remove inactive teams from the list
  let team = newTeam->Team.active ? Some(newTeam) : None

  updateFormCB(state.tagsToApply, team)
  Notification.success(
    "Success",
    successMessage(state.accessEndsAt, newTeam->Team.isSingleStudent),
  )
}

let updateStudent = (student, state, send, responseCB) => {
  send(UpdateSaving(true))
  let payload = Js.Dict.empty()

  Js.Dict.set(payload, "authenticity_token", AuthenticityToken.fromHead()->Js.Json.string)

  let updatedStudent = Student.updateInfo(
    ~name=state.name,
    ~excludedFromLeaderboard=state.excludedFromLeaderboard,
    ~title=state.title,
    ~affiliation=Some(state.affiliation),
    ~student,
  )
  Js.Dict.set(payload, "founder", Student.encode(state.teamName, updatedStudent))
  Js.Dict.set(
    payload,
    "tags",
    state.tagsToApply->{
      open Json.Encode
      array(string)
    },
  )
  Js.Dict.set(
    payload,
    "coach_ids",
    state.teamCoaches->{
      open Json.Encode
      array(string)
    },
  )

  Js.Dict.set(
    payload,
    "access_ends_at",
    state.accessEndsAt
    ->OptionUtils.map(Js.Date.toString)
    ->OptionUtils.default("")
    ->{
      open Json.Encode
      string
    },
  )

  let url = "/school/students/" ++ (student->Student.id)
  Api.update(url, payload, responseCB, handleErrorCB(send))
}

let boolBtnClasses = selected => {
  let classes = "toggle-button__button"
  classes ++ (selected ? " toggle-button__button--active" : "")
}

let handleTeamCoachList = (schoolCoaches, team) => {
  let selectedTeamCoachIds = team->Team.coachIds
  schoolCoaches->Array.map(coach => {
    let coachId = coach->Coach.id
    let selected =
      selectedTeamCoachIds->Js.Array.findIndex(selectedCoachId => coachId == selectedCoachId) > -1

    (coach->Coach.id, coach->Coach.name, selected)
  })
}

module SelectablePrerequisiteTargets = {
  type t = Coach.t

  let value = t => t->Coach.name
  let searchString = value

  let make = (coach): t => coach
}

let setTeamCoachSearch = (send, value) => send(UpdateCoachSearchInput(value))

let selectTeamCoach = (send, state, coach) => {
  let updatedTeamCoaches = state.teamCoaches->Js.Array.concat([coach->Coach.id])
  send(UpdateCoachesList(updatedTeamCoaches))
}

let deSelectTeamCoach = (send, state, coach) => {
  let updatedTeamCoaches =
    state.teamCoaches->Js.Array.filter(coachId => coachId != Coach.id(coach))
  send(UpdateCoachesList(updatedTeamCoaches))
}

module MultiselectForTeamCoaches = MultiselectInline.Make(SelectablePrerequisiteTargets)

let teamCoachesEditor = (courseCoaches, state, send) => {
  let selected =
    courseCoaches
    ->Js.Array.filter(coach => state.teamCoaches->Array.mem(Coach.id(coach)))
    ->Array.map(coach => SelectablePrerequisiteTargets.make(coach))

  let unselected =
    courseCoaches
    ->Js.Array.filter(coach => !(state.teamCoaches->Array.mem(Coach.id(coach))))
    ->Array.map(coach => SelectablePrerequisiteTargets.make(coach))
  <div className="mt-2">
    <MultiselectForTeamCoaches
      placeholder="Search coaches"
      emptySelectionMessage="No coaches selected"
      allItemsSelectedMessage="You have selected all available coaches!"
      selected
      unselected
      onChange={setTeamCoachSearch(send)}
      value=state.teamCoachSearchInput
      onSelect={selectTeamCoach(send, state)}
      onDeselect={deSelectTeamCoach(send, state)}
    />
  </div>
}

let initialState = (student, team) => {
  name: student->Student.name,
  teamName: team->Team.name,
  tagsToApply: student->Student.tags,
  teamCoaches: team->Team.coachIds,
  teamCoachSearchInput: "",
  excludedFromLeaderboard: student->Student.excludedFromLeaderboard,
  title: student->Student.title,
  affiliation: student->Student.affiliation->OptionUtils.toString,
  saving: false,
  accessEndsAt: team->Team.accessEndsAt,
}

let reducer = (state, action) =>
  switch action {
  | UpdateName(name) => {...state, name: name}
  | UpdateTeamName(teamName) => {...state, teamName: teamName}
  | AddTag(tag) => {
      ...state,
      tagsToApply: state.tagsToApply->Array.append([tag]),
    }
  | RemoveTag(tag) => {
      ...state,
      tagsToApply: state.tagsToApply->Js.Array.filter(t => t !== tag),
    }
  | UpdateCoachesList(teamCoaches) => {...state, teamCoaches: teamCoaches}
  | UpdateCoachSearchInput(teamCoachSearchInput) => {
      ...state,
      teamCoachSearchInput: teamCoachSearchInput,
    }
  | UpdateExcludedFromLeaderboard(excludedFromLeaderboard) => {
      ...state,
      excludedFromLeaderboard: excludedFromLeaderboard,
    }
  | UpdateTitle(title) => {...state, title: title}
  | UpdateAffiliation(affiliation) => {...state, affiliation: affiliation}
  | UpdateSaving(bool) => {...state, saving: bool}
  | UpdateAccessEndsAt(accessEndsAt) => {...state, accessEndsAt: accessEndsAt}
  }

@react.component
let make = (~student, ~team, ~studentTags, ~courseCoaches, ~updateFormCB) => {
  let (state, send) = React.useReducer(reducer, initialState(student, team))

  let isSingleStudent = team->Team.isSingleStudent

  <DisablingCover disabled=state.saving>
    <div>
      <div className="pt-5">
        <label
          className="inline-block tracking-wide text-xs font-semibold mb-2 leading-tight"
          htmlFor="name">
          {"Name"->str}
        </label>
        <input
          value=state.name
          onChange={event => updateName(send, ReactEvent.Form.target(event)["value"])}
          className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 leading-snug focus:outline-none focus:bg-white focus:border-gray-500"
          id="name"
          type_="text"
          placeholder="Student name here"
        />
        <School__InputGroupError
          message="Name must have at least two characters" active={state.name->stringInputInvalid}
        />
      </div>
      {isSingleStudent
        ? ReasonReact.null
        : <div className="mt-5">
            <label
              className="inline-block tracking-wide text-xs font-semibold mb-2 leading-tight"
              htmlFor="team_name">
              {"Team Name"->str}
            </label>
            <input
              value=state.teamName
              onChange={event => updateTeamName(send, ReactEvent.Form.target(event)["value"])}
              maxLength=50
              className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 leading-snug focus:outline-none focus:bg-white focus:border-gray-500"
              id="team_name"
              type_="text"
              placeholder="Team name here"
            />
            <School__InputGroupError
              message="Team Name must have at least two characters"
              active={state.teamName->stringInputInvalid}
            />
          </div>}
      <div className="mt-5">
        <label
          className="inline-block tracking-wide text-xs font-semibold mb-2 leading-tight"
          htmlFor="title">
          {"Title"->str}
        </label>
        <input
          value=state.title
          onChange={event => updateTitle(send, ReactEvent.Form.target(event)["value"])}
          className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 leading-snug focus:outline-none focus:bg-white focus:border-gray-500"
          id="title"
          type_="text"
          placeholder="Student, Coach, CEO, etc."
        />
        <School__InputGroupError
          message="Title must have at least two characters"
          active={state.title->stringInputInvalid}
        />
      </div>
      <div className="mt-5">
        <label
          className="inline-block tracking-wide text-xs font-semibold mb-2 leading-tight"
          htmlFor="affiliation">
          {"Affiliation"->str}
        </label>
        <span className="text-xs ml-1"> {"(optional)"->str} </span>
        <input
          value=state.affiliation
          onChange={event => send(UpdateAffiliation(ReactEvent.Form.target(event)["value"]))}
          className="appearance-none block w-full bg-white border border-gray-400 rounded py-3 px-4 leading-snug focus:outline-none focus:bg-white focus:border-gray-500"
          id="affiliation"
          type_="text"
          placeholder="Acme Inc., Acme University, etc."
        />
      </div>
      <div className="mt-5">
        <div className="border-b pb-4 mb-2 mt-5 ">
          <span className="inline-block mr-1 text-xs font-semibold">
            {(isSingleStudent ? "Personal Coaches" : "Team Coaches")->str}
          </span>
          {teamCoachesEditor(courseCoaches, state, send)}
        </div>
      </div>
      <div className="mt-5">
        <div className="mb-2 text-xs font-semibold"> {"Tags applied:"->str} </div>
        <StudentsEditor__SearchableTagList
          unselectedTags={studentTags->Js.Array.filter(tag =>
            !(state.tagsToApply->Array.mem(tag))
          )}
          selectedTags=state.tagsToApply
          addTagCB={tag => send(AddTag(tag))}
          removeTagCB={tag => send(RemoveTag(tag))}
          allowNewTags=true
        />
      </div>
      <div className="mt-5">
        <div className="flex items-center flex-shrink-0">
          <label className="block tracking-wide text-xs font-semibold mr-3">
            {"Should this student be excluded from leaderboards?"->str}
          </label>
          <div className="flex flex-shrink-0 rounded-lg overflow-hidden border border-gray-400">
            <button
              title="Exclude this student from the leaderboard"
              onClick={event => {
                ReactEvent.Mouse.preventDefault(event)
                send(UpdateExcludedFromLeaderboard(true))
              }}
              className={boolBtnClasses(state.excludedFromLeaderboard)}>
              {"Yes"->str}
            </button>
            <button
              title="Include this student in the leaderboard"
              onClick={_event => {
                ReactEvent.Mouse.preventDefault(_event)
                send(UpdateExcludedFromLeaderboard(false))
              }}
              className={boolBtnClasses(!state.excludedFromLeaderboard)}>
              {"No"->str}
            </button>
          </div>
        </div>
      </div>
      <div className="mt-5">
        <label className="tracking-wide text-xs font-semibold" htmlFor="access-ends-at-input">
          {(isSingleStudent ? "Student's" : "Team's") ++ " Access Ends On"->str}
        </label>
        <span className="ml-1 text-xs"> {"(optional)"->str} </span>
        <HelpIcon
          className="ml-2" link="https://docs.pupilfirst.com/#/students?id=editing-student-details">
          {"If set, students will not be able to complete targets after this date."->str}
        </HelpIcon>
        <DatePicker
          onChange={date => send(UpdateAccessEndsAt(date))}
          selected=?state.accessEndsAt
          id="access-ends-at-input"
        />
      </div>
    </div>
    <div className="my-5 w-auto">
      <button
        disabled={formInvalid(state)}
        onClick={_e =>
          updateStudent(student, state, send, handleResponseCB(updateFormCB, state, student, team))}
        className="w-full btn btn-large btn-primary">
        {"Update Student"->str}
      </button>
    </div>
  </DisablingCover>
}

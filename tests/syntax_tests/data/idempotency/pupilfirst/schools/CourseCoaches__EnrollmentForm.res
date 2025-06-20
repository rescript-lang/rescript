open CourseCoaches__Types

let str = React.string

type action =
  | UpdateCoachesList(array<string>)
  | UpdateCoachSearchInput(string)
  | ToggleSaving

type state = {
  courseCoaches: array<string>,
  coachSearchInput: string,
  saving: bool,
}

let reducer = (state, action) =>
  switch action {
  | UpdateCoachesList(courseCoaches) => {...state, courseCoaches: courseCoaches}
  | ToggleSaving => {...state, saving: !state.saving}
  | UpdateCoachSearchInput(coachSearchInput) => {...state, coachSearchInput: coachSearchInput}
  }

let makePayload = state => {
  let payload = Js.Dict.empty()

  Js.Dict.set(payload, "authenticity_token", AuthenticityToken.fromHead()->Js.Json.string)

  Js.Dict.set(
    payload,
    "coach_ids",
    state.courseCoaches->{
      open Json.Encode
      array(string)
    },
  )

  payload
}

module SelectableCourseCoaches = {
  type t = SchoolCoach.t

  let value = t => t->SchoolCoach.name
  let searchString = value
}

let setCoachSearchInput = (send, value) => send(UpdateCoachSearchInput(value))

let selectCoach = (send, state, coach) => {
  let updatedCoaches = state.courseCoaches->Js.Array.concat([coach->SchoolCoach.id])
  send(UpdateCoachesList(updatedCoaches))
}

let deSelectCoach = (send, state, coach) => {
  let updatedCoaches =
    state.courseCoaches->Js.Array.filter(coachId => coachId != SchoolCoach.id(coach))
  send(UpdateCoachesList(updatedCoaches))
}

module MultiselectForCourseCoaches = MultiselectInline.Make(SelectableCourseCoaches)

let courseCoachEditor = (coaches, state, send) => {
  let selected =
    coaches->Js.Array.filter(coach => state.courseCoaches->Array.mem(SchoolCoach.id(coach)))
  let unselected =
    coaches->Js.Array.filter(coach => !(state.courseCoaches->Array.mem(SchoolCoach.id(coach))))
  <MultiselectForCourseCoaches
    placeholder="Search coaches"
    emptySelectionMessage="No coaches selected"
    allItemsSelectedMessage="You have selected all coaches!"
    selected
    unselected
    onChange={setCoachSearchInput(send)}
    value=state.coachSearchInput
    onSelect={selectCoach(send, state)}
    onDeselect={deSelectCoach(send, state)}
  />
}

let handleResponseCB = (updateCoachesCB, json) => {
  let courseCoaches = json->{
    open Json.Decode
    field("course_coaches", array(CourseCoach.decode))
  }
  updateCoachesCB(courseCoaches)
  Notification.success("Success", "Coach enrollments updated successfully")
}

let updateCourseCoaches = (state, send, courseId, updateCoachesCB) => {
  send(ToggleSaving)

  let payload = makePayload(state)
  let url = "/school/courses/" ++ (courseId ++ "/update_coach_enrollments")

  Api.create(url, payload, handleResponseCB(updateCoachesCB), () => send(ToggleSaving))
}

let computeAvailableCoaches = (schoolCoaches, courseCoaches) => {
  let courseCoachIds = courseCoaches->Array.map(CourseCoach.id)
  schoolCoaches->Js.Array.filter(coach => !(courseCoachIds->Array.mem(coach->SchoolCoach.id)))
}

@react.component
let make = (~schoolCoaches, ~courseCoaches, ~courseId, ~updateCoachesCB) => {
  let (state, send) = React.useReducer(
    reducer,
    {courseCoaches: [], coachSearchInput: "", saving: false},
  )

  let coaches = computeAvailableCoaches(schoolCoaches, courseCoaches)

  let saveDisabled = state.courseCoaches->ArrayUtils.isEmpty || state.saving

  <div className="w-full">
    <div className="w-full">
      <div className="mx-auto bg-white">
        <div className="max-w-2xl pt-6 px-6 mx-auto">
          <h5 className="uppercase text-center border-b border-gray-400 pb-2 mb-4">
            {"ASSIGN COACHES TO THE COURSE"->str}
          </h5>
          {coaches->Array.length > 0
            ? <div>
                <div id="course_coaches">
                  <span className="inline-block mr-1 mb-2 text-xs font-semibold">
                    {"Select coaches:"->str}
                  </span>
                  {courseCoachEditor(coaches, state, send)}
                </div>
              </div>
            : React.null}
        </div>
        <div className="flex max-w-2xl w-full mt-5 px-6 pb-5 mx-auto">
          <button
            disabled=saveDisabled
            onClick={_e => updateCourseCoaches(state, send, courseId, updateCoachesCB)}
            className="w-full btn btn-primary btn-large">
            {"Add Course Coaches"->str}
          </button>
        </div>
      </div>
    </div>
  </div>
}

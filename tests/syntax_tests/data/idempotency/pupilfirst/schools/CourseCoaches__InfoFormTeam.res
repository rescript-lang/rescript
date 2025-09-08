open CourseCoaches__Types

let str = React.string

let deleteIconClasses = deleting => deleting ? "fas fa-spinner fa-pulse" : "far fa-trash-alt"

module DeleteCoachTeamEnrollmentQuery = %graphql(`
  mutation($teamId: ID!, $coachId: ID!) {
    deleteCoachTeamEnrollment(teamId: $teamId, coachId: $coachId) {
      success
    }
  }
`)

let deleteTeamEnrollment = (team, coach, setDeleting, removeTeamEnrollmentCB, event) => {
  event->ReactEvent.Mouse.preventDefault

  WindowUtils.confirm(
    "Are you sure you want to remove " ++
    ((team->Team.name) ++
    " from the list of assigned teams?"),
    () => {
      setDeleting(_ => true)
      DeleteCoachTeamEnrollmentQuery.make(~teamId=Team.id(team), ~coachId=CourseCoach.id(coach), ())
      ->GraphqlQuery.sendQuery
      ->Js.Promise.then_(response => {
        if response["deleteCoachTeamEnrollment"]["success"] {
          removeTeamEnrollmentCB(Team.id(team))
        } else {
          setDeleting(_ => false)
        }
        response->Js.Promise.resolve
      })
      ->ignore
    },
  )
}

@react.component
let make = (~team, ~coach, ~removeTeamEnrollmentCB) => {
  let (deleting, setDeleting) = React.useState(() => false)
  <div
    ariaLabel={"Team " ++ (team->Team.name)}
    className="flex items-center justify-between bg-gray-100 text-xs text-gray-900 border rounded pl-3 mt-2"
    key={team->Team.id}>
    <div className="flex flex-1 justify-between items-center">
      <div className="font-semibold w-1/2">
        {team
        ->Team.students
        ->Js.Array.mapi((student, index) =>
          <div className="p-1" key={index->string_of_int}> {student->str} </div>
        )
        ->React.array}
      </div>
      {team->Team.students->Array.length > 1
        ? <div className="w-1/2">
            <p className="text-tiny"> {"Team"->str} </p>
            <p className="font-semibold"> {team->Team.name->str} </p>
          </div>
        : React.null}
    </div>
    <div className="w-10 text-center flex-shrink-0 hover:text-gray-900 hover:bg-gray-200">
      <button
        title={"Delete " ++ Team.name(team)}
        onClick={deleteTeamEnrollment(team, coach, setDeleting, removeTeamEnrollmentCB)}
        className="p-3">
        <FaIcon classes={deleteIconClasses(deleting)} />
      </button>
    </div>
  </div>
}

%raw(`require("./CoursesStudents__Root.css")`)

open CoursesStudents__Types

let str = React.string

let levelInfo = (levelId, levels) =>
  <span
    className="inline-flex flex-col items-center rounded bg-orange-100 border border-orange-300 px-2 pt-2 pb-1 border">
    <div className="text-xs font-semibold"> {"Level"->str} </div>
    <div className="font-bold">
      {levels
      ->ArrayUtils.unsafeFind(
        (l: Level.t) => l.id == levelId,
        "Unable to find level with id: " ++ (levelId ++ "in CoursesStudents__TeamsList"),
      )
      ->Level.number
      ->string_of_int
      ->str}
    </div>
  </span>

let showStudent = (team, levels, teamCoaches) => {
  let student = TeamInfo.students(team)[0]
  <Link
    href={"/students/" ++ ((student->TeamInfo.studentId) ++ "/report")}
    key={student->TeamInfo.studentId}
    ariaLabel={"student-card-" ++ (student->TeamInfo.studentId)}
    className="flex md:flex-row justify-between bg-white mt-4 rounded-lg shadow cursor-pointer hover:border-primary-500 hover:text-primary-500 hover:shadow-md">
    <div className="flex flex-1 flex-col justify-center md:flex-row md:w-3/5">
      <div className="flex w-full items-start md:items-center p-3 md:px-4 md:py-5">
        {CoursesStudents__TeamCoaches.avatar(
          student->TeamInfo.studentAvatarUrl,
          student->TeamInfo.studentName,
        )}
        <div className="ml-2 md:ml-3 block text-sm md:pr-2">
          <p className="font-semibold inline-block leading-snug">
            {student->TeamInfo.studentName->str}
          </p>
          <p className="text-gray-600 font-semibold text-xs mt-px leading-snug">
            {student->TeamInfo.studentTitle->str}
          </p>
        </div>
      </div>
    </div>
    <div
      ariaLabel={"team-level-info-" ++ (team->TeamInfo.id)}
      className="w-2/5 flex items-center justify-end md:justify-between p-3 md:p-4">
      <CoursesStudents__TeamCoaches
        title={<div className="mb-1"> {"Personal Coaches"->str} </div>}
        className="hidden md:inline-block"
        coaches=teamCoaches
      />
      {levelInfo(team->TeamInfo.levelId, levels)}
    </div>
  </Link>
}

let showTeam = (team, levels, teamCoaches) =>
  <div
    key={team->TeamInfo.id}
    ariaLabel={"Info of team " ++ (team->TeamInfo.id)}
    className="flex shadow bg-white rounded-lg mt-4 overflow-hidden flex-col-reverse md:flex-row">
    <div className="flex flex-col flex-1 w-full md:w-3/5">
      {team
      ->TeamInfo.students
      ->Array.map(student =>
        <Link
          href={"/students/" ++ ((student->TeamInfo.studentId) ++ "/report")}
          key={student->TeamInfo.studentId}
          ariaLabel={"student-card-" ++ (student->TeamInfo.studentId)}
          className="flex items-center bg-white cursor-pointer hover:border-primary-500 hover:text-primary-500 hover:bg-gray-100">
          <div className="flex w-full md:flex-1 p-3 md:px-4 md:py-5">
            {CoursesStudents__TeamCoaches.avatar(
              student->TeamInfo.studentAvatarUrl,
              student->TeamInfo.studentName,
            )}
            <div className="ml-2 md:ml-3 text-sm flex flex-col">
              <p className="font-semibold inline-block leading-snug ">
                {student->TeamInfo.studentName->str}
              </p>
              <p className="text-gray-600 font-semibold text-xs mt-px leading-snug ">
                {student->TeamInfo.studentTitle->str}
              </p>
            </div>
          </div>
        </Link>
      )
      ->React.array}
    </div>
    <div
      className="flex w-full md:w-2/5 items-center bg-gray-200 md:bg-white border-l py-2 md:py-0 px-3 md:px-4">
      <div className="flex-1 pb-3 md:py-3 pr-3">
        <div>
          <p className="text-xs inline-block leading-tight"> {"Team"->str} </p>
          <h3 className="text-base font-semibold leading-snug"> {team->TeamInfo.name->str} </h3>
          <CoursesStudents__TeamCoaches
            title={<div className="mb-1"> {"Team Coaches"->str} </div>}
            className="hidden md:inline-block mt-3"
            coaches=teamCoaches
          />
        </div>
      </div>
      <div ariaLabel={"team-level-info-" ++ (team->TeamInfo.id)} className="flex-shrink-0">
        {levelInfo(team->TeamInfo.levelId, levels)}
      </div>
    </div>
  </div>

@react.component
let make = (~levels, ~teams, ~teamCoaches) =>
  <div>
    {teams->ArrayUtils.isEmpty
      ? <div className="course-review__reviewed-empty text-lg font-semibold text-center py-4">
          <h5 className="py-4 mt-4 bg-gray-200 text-gray-800 font-semibold">
            {"No teams to show"->str}
          </h5>
        </div>
      : teams
        ->Array.map(team => {
          let coaches = team->TeamInfo.coaches(teamCoaches)

          Array.length(team->TeamInfo.students) == 1
            ? showStudent(team, levels, coaches)
            : showTeam(team, levels, coaches)
        })
        ->React.array}
  </div>

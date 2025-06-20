%raw(`require("./CoursesStudents__StudentOverlay.css")`)

open CoursesStudents__Types
let str = React.string

type selectedTab =
  | Notes
  | Submissions

type studentData =
  | Loading
  | Loaded(StudentDetails.t)

type state = {
  selectedTab: selectedTab,
  studentData: studentData,
  submissions: Submissions.t,
}

let initialState = {
  studentData: Loading,
  selectedTab: Notes,
  submissions: Unloaded,
}

let closeOverlay = courseId => ReasonReactRouter.push("/courses/" ++ (courseId ++ "/students"))

module StudentDetailsQuery = %graphql(`
    query StudentDetailsQuery($studentId: ID!) {
      studentDetails(studentId: $studentId) {
        email, phone, socialLinks,
        evaluationCriteria {
          id, name, maxGrade, passGrade
        },
        coachNotes {
          id
          note
          createdAt
          author {
            id
            name
            title
            avatarUrl
          }
        },
        team {
          id
          name
          levelId
          droppedOutAt
          accessEndsAt
          students {
            id
            name
            title
            avatarUrl
          }
          coachUserIds
        }
        socialLinks
        totalTargets
        targetsCompleted
        completedLevelIds
        quizScores
        averageGrades {
          evaluationCriterionId
          averageGrade
        }
      }
    }
  `)

let updateStudentDetails = (setState, studentId, details) =>
  setState(state => {
    ...state,
    studentData: Loaded(details->StudentDetails.makeFromJs(studentId)),
  })

let getStudentDetails = (studentId, setState, ()) => {
  setState(state => {...state, studentData: Loading})
  StudentDetailsQuery.make(~studentId, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(response => {
    response["studentDetails"]->updateStudentDetails(setState, studentId)
    Js.Promise.resolve()
  })
  ->ignore

  None
}

let updateSubmissions = (setState, submissions) =>
  setState(state => {...state, submissions: submissions})

let doughnutChart = (color, percentage) =>
  <svg viewBox="0 0 36 36" className={"student-overlay__doughnut-chart " ++ color}>
    <path
      className="student-overlay__doughnut-chart-bg"
      d="M18 2.0845 a 15.9155 15.9155 0 0 1 0 31.831 a 15.9155 15.9155 0 0 1 0 -31.831"
    />
    <path
      className="student-overlay__doughnut-chart-stroke"
      strokeDasharray={percentage ++ ", 100"}
      d="M18 2.0845 a 15.9155 15.9155 0 0 1 0 31.831 a 15.9155 15.9155 0 0 1 0 -31.831"
    />
    <text x="50%" y="58%" className="student-overlay__doughnut-chart-text font-semibold">
      {percentage ++ "%"->str}
    </text>
  </svg>

let targetsCompletionStatus = (targetsCompleted, totalTargets) => {
  let targetCompletionPercent =
    targetsCompleted /. totalTargets *. 100.0->int_of_float->string_of_int
  <div ariaLabel="target-completion-status" className="w-full lg:w-1/2 px-2">
    <div className="student-overlay__doughnut-chart-container">
      {doughnutChart("purple", targetCompletionPercent)}
      <p className="text-sm font-semibold text-center mt-3"> {"Total Targets Completed"->str} </p>
      <p className="text-sm text-gray-700 font-semibold text-center mt-1">
        {(targetsCompleted->int_of_float->string_of_int) ++
          ("/" ++
          ((totalTargets->int_of_float->string_of_int) ++ " Targets"))->str}
      </p>
    </div>
  </div>
}

let quizPerformanceChart = (averageQuizScore, quizzesAttempted) =>
  switch averageQuizScore {
  | Some(score) =>
    <div ariaLabel="quiz-performance-chart" className="w-full lg:w-1/2 px-2 mt-2 lg:mt-0">
      <div className="student-overlay__doughnut-chart-container">
        {doughnutChart("pink", score->int_of_float->string_of_int)}
        <p className="text-sm font-semibold text-center mt-3"> {"Average Quiz Score"->str} </p>
        <p className="text-sm text-gray-700 font-semibold text-center leading-tight mt-1">
          {quizzesAttempted ++ " Quizzes Attempted"->str}
        </p>
      </div>
    </div>
  | None => React.null
  }

let averageGradeCharts = (
  evaluationCriteria: array<CoursesStudents__EvaluationCriterion.t>,
  averageGrades: array<StudentDetails.averageGrade>,
) =>
  averageGrades
  ->Array.map(grade => {
    let criterion = StudentDetails.evaluationCriterionForGrade(
      grade,
      evaluationCriteria,
      "CoursesStudents__StudentOverlay",
    )
    let passGrade = criterion->CoursesStudents__EvaluationCriterion.passGrade->float_of_int
    let averageGrade = grade->StudentDetails.gradeValue
    <div
      ariaLabel={"average-grade-for-criterion-" ++
      (criterion->CoursesStudents__EvaluationCriterion.id)}
      key={criterion->CoursesStudents__EvaluationCriterion.id}
      className="flex w-full lg:w-1/2 px-2 mt-2">
      <div className="student-overlay__pie-chart-container">
        <div className="flex px-5 pt-4 text-center items-center">
          <svg
            className={"student-overlay__pie-chart " ++ (
              averageGrade < passGrade
                ? "student-overlay__pie-chart--fail"
                : "student-overlay__pie-chart--pass"
            )}
            viewBox="0 0 32 32">
            <circle
              className={"student-overlay__pie-chart-circle " ++ (
                averageGrade < passGrade
                  ? "student-overlay__pie-chart-circle--fail"
                  : "student-overlay__pie-chart-circle--pass"
              )}
              strokeDasharray={StudentDetails.gradeAsPercentage(grade, criterion) ++ ", 100"}
              r="16"
              cx="16"
              cy="16"
            />
          </svg>
          <span className="ml-3 text-lg font-semibold">
            {(grade.grade->Js.Float.toString) ++ ("/" ++ (criterion.maxGrade->string_of_int))
              ->str}
          </span>
        </div>
        <p className="text-sm font-semibold px-5 pt-3 pb-4">
          {criterion->CoursesStudents__EvaluationCriterion.name->str}
        </p>
      </div>
    </div>
  })
  ->React.array

let test = (value, url) => {
  let tester = Js.Re.fromString(value)
  url->Js.Re.test_(tester)
}

let socialLinkIconClass = url =>
  switch url {
  | url if url->test("twitter") => "fab fa-twitter"
  | url if url->test("facebook") => "fab fa-facebook-f"
  | url if url->test("instagram") => "fab fa-instagram"
  | url if url->test("youtube") => "fab fa-youtube"
  | url if url->test("linkedin") => "fab fa-linkedin"
  | url if url->test("reddit") => "fab fa-reddit"
  | url if url->test("flickr") => "fab fa-flickr"
  | url if url->test("github") => "fab fa-github"
  | _unknownUrl => "fas fa-users"
  }

let showSocialLinks = socialLinks =>
  <div
    className="inline-flex flex-wrap justify-center text-lg text-gray-800 mt-3 bg-gray-100 px-2 rounded-lg">
    {socialLinks
    ->Array.mapi((index, link) =>
      <a
        className="px-2 py-1 inline-block hover:text-primary-500"
        key={index->string_of_int}
        target="_blank"
        href=link>
        <i className={socialLinkIconClass(link)} />
      </a>
    )
    ->React.array}
  </div>

let personalInfo = studentDetails =>
  <div className="mt-2 text-center">
    <div className="flex flex-wrap justify-center text-xs font-semibold text-gray-800">
      <div className="flex items-center px-2">
        <i className="fas fa-envelope" />
        <p className="ml-2 tracking-wide"> {studentDetails->StudentDetails.email->str} </p>
      </div>
      {switch studentDetails->StudentDetails.phone {
      | Some(phone) =>
        <div className="flex items-center px-2">
          <i className="fas fa-phone" /> <p className="ml-2 tracking-wide"> {phone->str} </p>
        </div>
      | None => React.null
      }}
    </div>
    {
      let socialLinks = studentDetails->StudentDetails.socialLinks
      socialLinks->ArrayUtils.isNotEmpty ? showSocialLinks(socialLinks) : React.null
    }
  </div>

let setSelectedTab = (selectedTab, setState) =>
  setState(state => {...state, selectedTab: selectedTab})

let studentLevelClasses = (levelNumber, levelCompleted, currentLevelNumber) => {
  let reached = levelNumber <= currentLevelNumber ? "student-overlay__student-level--reached" : ""

  let current = levelNumber == currentLevelNumber ? " student-overlay__student-level--current" : ""

  let completed = levelCompleted ? " student-overlay__student-level--completed" : ""

  reached ++ (current ++ completed)
}

let levelProgressBar = (levelId, levels, levelsCompleted) => {
  let applicableLevels = levels->Js.Array.filter(level => Level.number(level) != 0)

  let courseCompleted =
    applicableLevels->Array.for_all(level => levelsCompleted->Array.mem(level->Level.id))

  let currentLevelNumber =
    applicableLevels
    ->ArrayUtils.unsafeFind(
      level => Level.id(level) == levelId,
      "Unable to find level with id" ++ (levelId ++ "in StudentOverlay"),
    )
    ->Level.number

  <div className="mb-8">
    <div className="flex justify-between items-end">
      <h6 className="text-sm font-semibold"> {"Level Progress"->str} </h6>
      {courseCompleted
        ? <p className="text-green-600 font-semibold">
            {`🎉`->str} <span className="text-xs ml-px"> {"Course Completed!"->str} </span>
          </p>
        : React.null}
    </div>
    <div className="h-12 flex items-center">
      <ul
        className={"student-overlay__student-level-progress flex w-full " ++ (
          courseCompleted ? "student-overlay__student-level-progress--completed" : ""
        )}>
        {applicableLevels
        ->Level.sort
        ->Array.map(level => {
          let levelNumber = level->Level.number
          let levelCompleted = levelsCompleted->Array.mem(level->Level.id)

          <li
            key={level->Level.id}
            className={"flex-1 student-overlay__student-level " ++
            studentLevelClasses(levelNumber, levelCompleted, currentLevelNumber)}>
            <span className="student-overlay__student-level-count">
              {levelNumber->string_of_int->str}
            </span>
          </li>
        })
        ->React.array}
      </ul>
    </div>
  </div>
}

let addNoteCB = (setState, studentDetails, note) =>
  setState(state => {
    ...state,
    studentData: Loaded(StudentDetails.addNewNote(note, studentDetails)),
  })

let removeNoteCB = (setState, studentDetails, noteId) =>
  setState(state => {
    ...state,
    studentData: Loaded(StudentDetails.removeNote(noteId, studentDetails)),
  })

let userInfo = (~key, ~avatarUrl, ~name, ~title) =>
  <div key className="shadow rounded-lg p-4 flex items-center mt-2">
    {CoursesStudents__TeamCoaches.avatar(avatarUrl, name)}
    <div className="ml-2 md:ml-3">
      <div className="text-sm font-semibold"> {name->str} </div>
      <div className="text-xs"> {title->str} </div>
    </div>
  </div>

let coachInfo = (teamCoaches, studentDetails) => {
  let coaches = studentDetails->StudentDetails.team->TeamInfo.coaches(teamCoaches)

  let title =
    studentDetails->StudentDetails.teamHasManyStudents ? "Team Coaches" : "Personal Coaches"

  coaches->ArrayUtils.isNotEmpty
    ? <div className="mb-8">
        <h6 className="font-semibold"> {title->str} </h6>
        {coaches
        ->Array.map(coach =>
          userInfo(
            ~key=coach->Coach.userId,
            ~avatarUrl=coach->Coach.avatarUrl,
            ~name=coach->Coach.name,
            ~title=coach->Coach.title,
          )
        )
        ->React.array}
      </div>
    : React.null
}

let navigateToStudent = (setState, _event) => setState(_ => initialState)

let otherTeamMembers = (setState, studentId, studentDetails) =>
  if studentDetails->StudentDetails.teamHasManyStudents {
    <div className="block mb-8">
      <h6 className="font-semibold"> {"Other Team Members"->str} </h6>
      {studentDetails
      ->StudentDetails.team
      ->TeamInfo.otherStudents(studentId)
      ->Array.map(student => {
        let path = "/students/" ++ ((student->TeamInfo.studentId) ++ "/report")

        <Link
          className="block"
          href=path
          onClick={navigateToStudent(setState)}
          key={student->TeamInfo.studentId}>
          {userInfo(
            ~key=student->TeamInfo.studentId,
            ~avatarUrl=student->TeamInfo.studentAvatarUrl,
            ~name=student->TeamInfo.studentName,
            ~title=student->TeamInfo.studentTitle,
          )}
        </Link>
      })
      ->React.array}
    </div>
  } else {
    React.null
  }

let inactiveWarning = teamInfo => {
  let warning = switch (teamInfo->TeamInfo.droppedOutAt, teamInfo->TeamInfo.accessEndsAt) {
  | (Some(droppedOutAt), _) =>
    Some(
      "This student dropped out of the course on " ++
      ((droppedOutAt->DateTime.format(DateTime.OnlyDate)) ++
      "."),
    )
  | (None, Some(accessEndsAt)) =>
    accessEndsAt->DateFns.isPast
      ? Some(
          "This student's access to the course ended on " ++
          ((accessEndsAt->DateTime.format(DateTime.OnlyDate)) ++
          "."),
        )
      : None
  | (None, None) => None
  }

  warning->OptionUtils.mapWithDefault(
    warning =>
      <div className="border border-yellow-400 rounded bg-yellow-400 py-2 px-3 mt-3">
        <i className="fas fa-exclamation-triangle" />
        <span className="ml-2"> {warning->str} </span>
      </div>,
    React.null,
  )
}

@react.component
let make = (~courseId, ~studentId, ~levels, ~userId, ~teamCoaches) => {
  let (state, setState) = React.useState(() => initialState)

  React.useEffect0(() => {
    ScrollLock.activate()
    Some(() => ScrollLock.deactivate())
  })

  React.useEffect1(getStudentDetails(studentId, setState), [studentId])

  <div
    className="fixed z-30 top-0 left-0 w-full h-full overflow-y-scroll md:overflow-hidden bg-white">
    {switch state.studentData {
    | Loaded(studentDetails) =>
      <div className="flex flex-col md:flex-row md:h-screen">
        <div
          className="w-full md:w-2/5 bg-white p-4 md:p-8 md:py-6 2xl:px-16 2xl:py-12 md:overflow-y-auto">
          <div className="student-overlay__student-details relative pb-8">
            <div
              onClick={_ => closeOverlay(courseId)}
              className="absolute z-50 left-0 cursor-pointer top-0 inline-flex p-1 rounded-full bg-gray-200 h-10 w-10 justify-center items-center text-gray-700 hover:text-gray-900 hover:bg-gray-300">
              <Icon className="if i-times-regular text-xl lg:text-2xl" />
            </div>
            <div
              className="student-overlay__student-avatar mx-auto w-18 h-18 md:w-24 md:h-24 text-xs border border-yellow-500 rounded-full overflow-hidden flex-shrink-0">
              {switch studentDetails->StudentDetails.avatarUrl {
              | Some(avatarUrl) => <img className="w-full object-cover" src=avatarUrl />
              | None =>
                <Avatar name={studentDetails->StudentDetails.name} className="object-cover" />
              }}
            </div>
            <h2 className="text-lg text-center mt-3">
              {studentDetails->StudentDetails.name->str}
            </h2>
            <p className="text-sm font-semibold text-center mt-1">
              {studentDetails->StudentDetails.title->str}
            </p>
            {personalInfo(studentDetails)}
            {inactiveWarning(studentDetails->StudentDetails.team)}
          </div>
          {levelProgressBar(
            studentDetails->StudentDetails.levelId,
            levels,
            studentDetails->StudentDetails.completedLevelIds,
          )}
          <div className="mb-8">
            <h6 className="font-semibold"> {"Targets Overview"->str} </h6>
            <div className="flex -mx-2 flex-wrap mt-2">
              {targetsCompletionStatus(
                studentDetails->StudentDetails.targetsCompleted,
                studentDetails->StudentDetails.totalTargets,
              )}
              {quizPerformanceChart(
                studentDetails->StudentDetails.averageQuizScore,
                studentDetails->StudentDetails.quizzesAttempted,
              )}
            </div>
          </div>
          {studentDetails->StudentDetails.averageGrades->ArrayUtils.isNotEmpty
            ? <div className="mb-8">
                <h6 className="font-semibold"> {"Average Grades"->str} </h6>
                <div className="flex -mx-2 flex-wrap">
                  {averageGradeCharts(
                    studentDetails->StudentDetails.evaluationCriteria,
                    studentDetails->StudentDetails.averageGrades,
                  )}
                </div>
              </div>
            : React.null}
          {coachInfo(teamCoaches, studentDetails)}
          {otherTeamMembers(setState, studentId, studentDetails)}
        </div>
        <div
          className="w-full relative md:w-3/5 bg-gray-100 md:border-l pb-6 2xl:pb-12 md:overflow-y-auto">
          <div
            className="sticky top-0 bg-gray-100 pt-2 md:pt-4 px-4 md:px-8 2xl:px-16 2xl:pt-10 z-30">
            <ul className="flex font-semibold border-b text-sm">
              <li
                onClick={_event => setSelectedTab(Notes, setState)}
                className={"px-3 py-3 md:py-2 cursor-pointer text-gray-800 rounded-t-lg " ++
                switch state.selectedTab {
                | Notes => "border-b-3 border-primary-500 text-primary-500 -mb-px"
                | Submissions => "hover:bg-gray-200 hover:text-gray-900"
                }}>
                {"Notes"->str}
              </li>
              <li
                onClick={_event => setSelectedTab(Submissions, setState)}
                className={"px-3 py-3 md:py-2 cursor-pointer text-gray-800 " ++
                switch state.selectedTab {
                | Submissions => "border-b-3 border-primary-500 text-primary-500 -mb-px"
                | Notes => "hover:bg-gray-200 hover:text-gray-900"
                }}>
                {"Submissions"->str}
              </li>
            </ul>
          </div>
          <div className="pt-2 px-4 md:px-8 2xl:px-16">
            {switch state.selectedTab {
            | Notes =>
              <CoursesStudents__CoachNotes
                studentId
                coachNotes={studentDetails->StudentDetails.coachNotes}
                addNoteCB={addNoteCB(setState, studentDetails)}
                userId
                removeNoteCB={removeNoteCB(setState, studentDetails)}
              />
            | Submissions =>
              <CoursesStudents__SubmissionsList
                studentId
                levels
                submissions=state.submissions
                updateSubmissionsCB={updateSubmissions(setState)}
              />
            }}
          </div>
        </div>
      </div>
    | Loading =>
      <div className="flex flex-col md:flex-row md:h-screen">
        <div className="w-full md:w-2/5 bg-white p-4 md:p-8 2xl:p-16">
          {SkeletonLoading.image()}
          {SkeletonLoading.multiple(~count=2, ~element=SkeletonLoading.profileCard())}
        </div>
        <div className="w-full relative md:w-3/5 bg-gray-100 md:border-l p-4 md:p-8 2xl:p-16">
          {SkeletonLoading.contents()} {SkeletonLoading.profileCard()}
        </div>
      </div>
    }}
  </div>
}

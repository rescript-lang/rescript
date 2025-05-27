type averageGrade = {
  evaluationCriterionId: string,
  grade: float,
}

type t = {
  id: string,
  email: string,
  phone: option<string>,
  coachNotes: array<CoursesStudents__CoachNote.t>,
  evaluationCriteria: array<CoursesStudents__EvaluationCriterion.t>,
  socialLinks: array<string>,
  totalTargets: int,
  targetsCompleted: int,
  quizScores: array<string>,
  averageGrades: array<averageGrade>,
  completedLevelIds: array<string>,
  team: CoursesStudents__TeamInfo.t,
}

let team = t => t.team

let student = t => t.team->CoursesStudents__TeamInfo.studentWithId(t.id)

let name = t => t->student->CoursesStudents__TeamInfo.studentName

let title = t => t->student->CoursesStudents__TeamInfo.studentTitle

let email = t => t.email

let levelId = t => t.team->CoursesStudents__TeamInfo.levelId

let phone = t => t.phone

let socialLinks = t => t.socialLinks

let avatarUrl = t => t->student->CoursesStudents__TeamInfo.studentAvatarUrl

let coachNotes = t => t.coachNotes

let teamCoachUserIds = t => t.team->CoursesStudents__TeamInfo.coachUserIds

let makeAverageGrade = gradesData =>
  gradesData->Js.Array.map(gradeData => {
    evaluationCriterionId: gradeData["evaluationCriterionId"],
    grade: gradeData["averageGrade"],
  })

let totalTargets = t => t.totalTargets->float_of_int

let gradeAsPercentage = (
  averageGrade: averageGrade,
  evaluationCriterion: CoursesStudents__EvaluationCriterion.t,
) => {
  let maxGrade = evaluationCriterion.maxGrade->float_of_int
  averageGrade.grade /. maxGrade *. 100.0->int_of_float->string_of_int
}

let targetsCompleted = t => t.targetsCompleted->float_of_int

let quizzesAttempted = t => t.quizScores->Array.length->string_of_int

let evaluationCriteria = t => t.evaluationCriteria

let averageGrades = t => t.averageGrades

let completedLevelIds = t => t.completedLevelIds

let gradeValue = averageGrade => averageGrade.grade

let evaluationCriterionForGrade = (grade, evaluationCriteria, componentName) =>
  evaluationCriteria->ArrayUtils.unsafeFind(
    ec => CoursesStudents__EvaluationCriterion.id(ec) == grade.evaluationCriterionId,
    "Unable to find evaluation criterion with id: " ++
    (grade.evaluationCriterionId ++
    (" in component: " ++ componentName)),
  )

let addNewNote = (note, t) => {
  let notes = Array.append(t.coachNotes, [note])
  {...t, coachNotes: notes}
}

let removeNote = (noteId, t) => {
  let notes = t.coachNotes->Js.Array.filter(note => CoursesStudents__CoachNote.id(note) != noteId)
  {...t, coachNotes: notes}
}

let computeAverageQuizScore = quizScores => {
  let sumOfPercentageScores =
    quizScores
    ->Array.map(quizScore => {
      let fractionArray = quizScore->String.split_on_char('/')->Array.of_list
      let (numerator, denominator) = (
        fractionArray[0]->float_of_string,
        fractionArray[1]->float_of_string,
      )
      numerator /. denominator *. 100.0
    })
    ->Js.Array.reduce((a, b) => a +. b, 0.0)
  sumOfPercentageScores /. (quizScores->Array.length->float_of_int)
}

let averageQuizScore = t =>
  t.quizScores->ArrayUtils.isEmpty ? None : Some(computeAverageQuizScore(t.quizScores))

let makeFromJs = (id, studentDetails) => {
  id: id,
  email: studentDetails["email"],
  phone: studentDetails["phone"],
  coachNotes: studentDetails["coachNotes"]->Js.Array.map(note =>
    note->CoursesStudents__CoachNote.makeFromJs
  ),
  evaluationCriteria: studentDetails["evaluationCriteria"]->CoursesStudents__EvaluationCriterion.makeFromJs,
  socialLinks: studentDetails["socialLinks"],
  totalTargets: studentDetails["totalTargets"],
  targetsCompleted: studentDetails["targetsCompleted"],
  quizScores: studentDetails["quizScores"],
  averageGrades: studentDetails["averageGrades"]->makeAverageGrade,
  completedLevelIds: studentDetails["completedLevelIds"],
  team: studentDetails["team"]->CoursesStudents__TeamInfo.makeFromJS,
}

let teamHasManyStudents = t => t.team->CoursesStudents__TeamInfo.students->Array.length > 1

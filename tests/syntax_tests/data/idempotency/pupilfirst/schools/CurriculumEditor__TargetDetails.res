exception InvalidVisibilityValue(string)
exception InvalidRoleValue(string)

type role =
  | Student
  | Team

type visibility =
  | Draft
  | Live
  | Archived

type t = {
  title: string,
  role: role,
  evaluationCriteria: array<string>,
  prerequisiteTargets: array<string>,
  targetGroupId: string,
  quiz: array<CurriculumEditor__QuizQuestion.t>,
  checklist: array<TargetChecklistItem.t>,
  linkToComplete: option<string>,
  visibility: visibility,
  completionInstructions: option<string>,
}

let title = t => t.title
let role = t => t.role

let targetGroupId = t => t.targetGroupId

let visibility = t => t.visibility

let quiz = t => t.quiz

let prerequisiteTargets = t => t.prerequisiteTargets

let evaluationCriteria = t => t.evaluationCriteria

let visibilityFromJs = visibilityString =>
  switch visibilityString {
  | "draft" => Draft
  | "live" => Live
  | "archived" => Archived
  | _ => raise(InvalidVisibilityValue("Unknown Value"))
  }

let visibilityAsString = visibility =>
  switch visibility {
  | Draft => "draft"
  | Live => "live"
  | Archived => "archived"
  }

let roleAsString = role =>
  switch role {
  | Student => "student"
  | Team => "team"
  }

let roleFromJs = roleString =>
  switch roleString {
  | "student" => Student
  | "team" => Team
  | role => raise(InvalidRoleValue("Unknown Value :" ++ role))
  }

let makeFromJs = targetData => {
  title: targetData["title"],
  role: roleFromJs(targetData["role"]),
  targetGroupId: targetData["targetGroupId"],
  evaluationCriteria: targetData["evaluationCriteria"],
  prerequisiteTargets: targetData["prerequisiteTargets"],
  quiz: targetData["quiz"]->Array.map(quizQuestion =>
    quizQuestion->CurriculumEditor__QuizQuestion.makeFromJs
  ),
  linkToComplete: targetData["linkToComplete"],
  completionInstructions: targetData["completionInstructions"],
  checklist: targetData["checklist"]->Json.Decode.array(TargetChecklistItem.decode),
  visibility: visibilityFromJs(targetData["visibility"]),
}

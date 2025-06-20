type t = {
  submissionId: string,
  evaluationCriterionId: string,
  grade: int,
}

let decode = json => {
  open Json.Decode
  {
    submissionId: json->field("submissionId", string),
    evaluationCriterionId: json->field("evaluationCriterionId", string),
    grade: json->field("grade", int),
  }
}

let sort = (criteria, grades) =>
  grades
  ->Array.of_list
  ->ArrayUtils.copyAndSort((g1, g2) => {
    let ec1 =
      criteria
      ->Array.of_list
      ->ArrayUtils.unsafeFind(
        ec => EvaluationCriterion.id(ec) == g1.evaluationCriterionId,
        "Unable to find evaluation criterion with ID: " ++
        (g1.evaluationCriterionId ++
        " in CoursesCurriculum__Grade"),
      )
    let ec2 =
      criteria
      ->Array.of_list
      ->ArrayUtils.unsafeFind(
        ec => EvaluationCriterion.id(ec) == g2.evaluationCriterionId,
        "Unable to find evaluation criterion with ID: " ++
        (g2.evaluationCriterionId ++
        " in CoursesCurriculum__Grade"),
      )
    String.compare(ec1->EvaluationCriterion.name, ec2->EvaluationCriterion.name)
  })
  ->Array.to_list

let grade = t => t.grade
let submissionId = t => t.submissionId
let evaluationCriterionId = t => t.evaluationCriterionId

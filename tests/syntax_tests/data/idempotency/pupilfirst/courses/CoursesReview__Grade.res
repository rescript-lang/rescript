type t = {
  evaluationCriterionId: string,
  value: int,
}

let make = (~evaluationCriterionId, ~value) => {
  evaluationCriterionId: evaluationCriterionId,
  value: value,
}

let sort = (criteria, grades) =>
  grades->ArrayUtils.copyAndSort((g1, g2) => {
    let ec1 =
      criteria->ArrayUtils.unsafeFind(
        ec => EvaluationCriterion.id(ec) == g1.evaluationCriterionId,
        "Unable to find evaluation criterion with ID: " ++
        (g1.evaluationCriterionId ++
        " in CoursesReview__Grade"),
      )
    let ec2 =
      criteria->ArrayUtils.unsafeFind(
        ec => EvaluationCriterion.id(ec) == g2.evaluationCriterionId,
        "Unable to find evaluation criterion with ID: " ++
        (g2.evaluationCriterionId ++
        " in CoursesReview__Grade"),
      )
    String.compare(ec1->EvaluationCriterion.name, ec2->EvaluationCriterion.name)
  })

let evaluationCriterionId = t => t.evaluationCriterionId
let value = t => t.value
let asJsType = t =>
  {
    "evaluationCriterionId": t.evaluationCriterionId,
    "grade": t.value,
  }

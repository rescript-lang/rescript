type t = {
  criterionId: string,
  criterionName: string,
  grade: option<int>,
}

let decode = json => {
  open Json.Decode
  {
    criterionId: json->field("criterionId", string),
    criterionName: json->field("criterionName", string),
    grade: json->field("grade", nullable(int))->Js.Null.toOption,
  }
}

let grade = t => t.grade

let pending = evaluation => evaluation->List.exists(grading => grading.grade == None)

let isFail = (passGrade, grading) =>
  switch grading.grade {
  | Some(grade) => grade < passGrade
  | None => false
  }

let anyFail = (passGrade, evaluation) =>
  evaluation->List.exists(grading => grading->isFail(passGrade))

let criterionId = t => t.criterionId

let criterionName = t => t.criterionName

let updateGrade = (newGrade, t) => {
  criterionId: t.criterionId,
  criterionName: t.criterionName,
  grade: Some(newGrade),
}

let gradingEncoder = grading => {
  open Json.Encode
  object_(list{
    ("criterionId", grading.criterionId->string),
    (
      "grade",
      switch grading.grade {
      | Some(grade) => grade->int
      | None => null
      },
    ),
  })
}

let make = (~criterionId, ~criterionName, ~grade) => {
  criterionId: criterionId,
  criterionName: criterionName,
  grade: Some(grade),
}

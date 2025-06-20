exception GradeLabelsEmpty

type t = {
  label: string,
  grade: int,
}

let decode = json => {
  open Json.Decode
  {
    label: json->field("label", string),
    grade: json->field("grade", int),
  }
}

let grade = t => t.grade
let label = t => t.label

let labelFor = (gradeLabels, grade) =>
  gradeLabels->List.find(gradeLabel => gradeLabel.grade == grade)->label

let create = (grade, label) => {grade: grade, label: label}

let empty = grade => {grade: grade, label: ""}

let update = (label, t) => {...t, label: label}

let asJsObject = t => {"grade": t.grade, "label": t.label}

let valid = t => t.label->Js.String.trim->Js.String.length >= 1

let makeFromJs = rawGradeLabel => {
  label: rawGradeLabel["label"],
  grade: rawGradeLabel["grade"],
}

let maxGrade = gradeLabels => {
  let rec aux = (max, remains) =>
    switch remains {
    | list{} => max
    | list{head, ...tail} => aux(Js.Math.max_int(head.grade, max), tail)
    }

  switch aux(0, gradeLabels) {
  | 0 =>
    Rollbar.error("GradeLabel.maxGrade received an empty list of gradeLabels")
    throw(GradeLabelsEmpty)
  | validGrade => validGrade
  }
}

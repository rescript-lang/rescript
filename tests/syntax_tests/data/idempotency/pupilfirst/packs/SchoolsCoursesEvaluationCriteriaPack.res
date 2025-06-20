type props = {
  courseId: string,
  evaluationCriteria: array<EvaluationCriterion.t>,
}

let decodeProps = json => {
  open Json.Decode
  {
    courseId: json->field("courseId", string),
    evaluationCriteria: json->field("evaluationCriteria", array(EvaluationCriterion.decode)),
  }
}

let props =
  DomUtils.parseJsonTag(~id="schools-courses-evaluation-criteria__props", ())->decodeProps

ReactDOMRe.renderToElementWithId(
  <EvaluationCriteria__Index courseId=props.courseId evaluationCriteria=props.evaluationCriteria />,
  "schools-courses-evaluation-criteria__root",
)

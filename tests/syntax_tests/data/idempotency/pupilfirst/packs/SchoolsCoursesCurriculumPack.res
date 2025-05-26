open CurriculumEditor__Types

type props = {
  course: Course.t,
  evaluationCriteria: list<EvaluationCriteria.t>,
  levels: list<Level.t>,
  targetGroups: list<TargetGroup.t>,
  targets: list<Target.t>,
  authenticityToken: string,
}

let decodeProps = json => {
  open Json.Decode
  {
    course: json->field("course", Course.decode),
    evaluationCriteria: json->field("evaluationCriteria", list(EvaluationCriteria.decode)),
    levels: json->field("levels", list(Level.decode)),
    targetGroups: json->field("targetGroups", list(TargetGroup.decode)),
    targets: json->field("targets", list(Target.decode)),
    authenticityToken: json->field("authenticityToken", string),
  }
}

let props =
  DomUtils.parseJsonAttribute(~id="curriculum-editor", ~attribute="data-props", ())->decodeProps

ReactDOMRe.renderToElementWithId(
  <CurriculumEditor
    course=props.course
    evaluationCriteria=props.evaluationCriteria
    levels=props.levels
    targetGroups=props.targetGroups
    targets=props.targets
    authenticityToken=props.authenticityToken
  />,
  "curriculum-editor",
)

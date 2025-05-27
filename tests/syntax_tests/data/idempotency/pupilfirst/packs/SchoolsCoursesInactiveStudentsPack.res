open InactiveStudentsPanel__Types

type props = {
  teams: list<Team.t>,
  courseId: string,
  students: list<Student.t>,
  authenticityToken: string,
  isLastPage: bool,
  currentPage: int,
}

let decodeProps = json => {
  open Json.Decode
  {
    teams: json->field("teams", list(Team.decode)),
    courseId: json->field("courseId", string),
    students: json->field("students", list(Student.decode)),
    authenticityToken: json->field("authenticityToken", string),
    currentPage: json->field("currentPage", int),
    isLastPage: json->field("isLastPage", bool),
  }
}

let props =
  DomUtils.parseJsonAttribute(~id="sa-students-panel", ~attribute="data-props", ())->decodeProps

ReactDOMRe.renderToElementWithId(
  <SA_InactiveStudentsPanel
    teams=props.teams
    courseId=props.courseId
    currentPage=props.currentPage
    isLastPage=props.isLastPage
    students=props.students
    authenticityToken=props.authenticityToken
  />,
  "sa-students-panel",
)

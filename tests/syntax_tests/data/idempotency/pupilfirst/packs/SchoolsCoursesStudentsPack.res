open StudentsEditor__Types

type props = {
  courseId: string,
  courseCoachIds: array<string>,
  schoolCoaches: array<Coach.t>,
  levels: array<Level.t>,
  studentTags: array<string>,
}

let decodeProps = json => {
  open Json.Decode
  {
    courseId: json->field("courseId", string),
    courseCoachIds: json->field("courseCoachIds", array(string)),
    schoolCoaches: json->field("schoolCoaches", array(Coach.decode)),
    levels: json->field("levels", array(Level.decode)),
    studentTags: json->field("studentTags", array(string)),
  }
}

let props = DomUtils.parseJsonTag(~id="sa-students-panel-data", ())->decodeProps

ReactDOMRe.renderToElementWithId(
  <StudentsEditor__Root
    courseId=props.courseId
    courseCoachIds=props.courseCoachIds
    schoolCoaches=props.schoolCoaches
    levels=props.levels
    studentTags=props.studentTags
  />,
  "sa-students-panel",
)

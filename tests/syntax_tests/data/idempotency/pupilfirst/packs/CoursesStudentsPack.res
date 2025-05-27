open CoursesStudents__Types

let decodeProps = json => {
  open Json.Decode
  (
    json->field("levels", array(Level.decode)),
    json->field("course", Course.decode),
    json->field("userId", string),
    json->field("teamCoaches", array(Coach.decode)),
    json->field("currentCoach", Coach.decode),
  )
}

let (levels, course, userId, teamCoaches, currentCoach) =
  DomUtils.parseJsonTag(~id="school-course-students__props", ())->decodeProps

ReactDOMRe.renderToElementWithId(
  <CoursesStudents__Root levels course userId teamCoaches currentCoach />,
  "react-root",
)

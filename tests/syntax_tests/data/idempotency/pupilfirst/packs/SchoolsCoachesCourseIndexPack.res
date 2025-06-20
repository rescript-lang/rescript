open CourseCoaches__Types

type props = {
  courseCoaches: array<CourseCoach.t>,
  schoolCoaches: array<SchoolCoach.t>,
  authenticityToken: string,
  courseId: string,
}

let decodeProps = json => {
  open Json.Decode
  {
    courseCoaches: json->field("courseCoaches", array(CourseCoach.decode)),
    schoolCoaches: json->field("schoolCoaches", array(SchoolCoach.decode)),
    courseId: json->field("courseId", string),
    authenticityToken: json->field("authenticityToken", string),
  }
}

let props = DomUtils.parseJsonTag(~id="course-coaches__props", ())->decodeProps

ReactDOMRe.renderToElementWithId(
  <CourseCoaches__Root
    courseCoaches=props.courseCoaches
    schoolCoaches=props.schoolCoaches
    courseId=props.courseId
    authenticityToken=props.authenticityToken
  />,
  "sa-coaches-enrollment-panel",
)

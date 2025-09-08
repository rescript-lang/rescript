open CourseAuthors__Types

let decodeProps = json => {
  open Json.Decode
  (json->field("courseId", string), json->field("authors", array(Author.decode)))
}

let (courseId, authors) =
  DomUtils.parseJsonTag(~id="schools-courses-authors__props", ())->decodeProps

ReactDOMRe.renderToElementWithId(
  <CourseAuthors__Root courseId authors />,
  "schools-courses-authors__root",
)

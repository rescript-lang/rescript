type props = {
  currentSchoolAdminId: string,
  admins: array<SchoolAdmin.t>,
}

let decodeProps = json => {
  open Json.Decode
  {
    currentSchoolAdminId: json->field("currentSchoolAdminId", string),
    admins: json->field("admins", array(SchoolAdmin.decode)),
  }
}

let props = DomUtils.parseJsonTag(~id="school-admins-data", ())->decodeProps

ReactDOMRe.renderToElementWithId(
  <SchoolAdmins__Editor currentSchoolAdminId=props.currentSchoolAdminId admins=props.admins />,
  "school-admins",
)

open StudentTopNav__Types

type props = {
  schoolName: string,
  logoUrl: option<string>,
  links: list<NavLink.t>,
  isLoggedIn: bool,
}

let decodeProps = json => {
  open Json.Decode
  {
    schoolName: json->field("schoolName", string),
    logoUrl: json->field("logoUrl", nullable(string))->Js.Null.toOption,
    links: json->field("links", list(NavLink.decode)),
    isLoggedIn: json->field("isLoggedIn", bool),
  }
}

let props = DomUtils.parseJsonTag(~id="student-top-nav-props", ())->decodeProps

ReactDOMRe.renderToElementWithId(
  <StudentTopNav
    schoolName=props.schoolName logoUrl=props.logoUrl links=props.links isLoggedIn=props.isLoggedIn
  />,
  "student-top-nav",
)

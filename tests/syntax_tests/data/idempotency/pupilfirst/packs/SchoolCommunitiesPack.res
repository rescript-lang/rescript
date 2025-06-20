type props = {
  communities: list<SchoolCommunities__Community.t>,
  courses: list<SchoolCommunities__Course.t>,
  connections: list<SchoolCommunities__Connection.t>,
}

let decodeProps = json => {
  open Json.Decode
  {
    communities: json->field("communities", list(SchoolCommunities__Community.decode)),
    courses: json->field("courses", list(SchoolCommunities__Course.decode)),
    connections: json->field("connections", list(SchoolCommunities__Connection.decode)),
  }
}

let props =
  DomUtils.parseJsonAttribute(
    ~id="school-communities",
    ~attribute="data-json-props",
    (),
  )->decodeProps

ReactDOMRe.renderToElementWithId(
  <SchoolCommunities__Index
    communities=props.communities courses=props.courses connections=props.connections
  />,
  "school-communities",
)

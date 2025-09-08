open CoachesSchoolIndex__Types

type props = {
  coaches: list<Coach.t>,
  authenticityToken: string,
}

let decodeProps = json => {
  open Json.Decode
  {
    coaches: json->field("coaches", list(Coach.decode)),
    authenticityToken: json->field("authenticityToken", string),
  }
}

let props =
  DomUtils.parseJsonAttribute(~id="sa-coaches-panel", ~attribute="data-props", ())->decodeProps

ReactDOMRe.renderToElementWithId(
  <SA_Coaches_SchoolIndex coaches=props.coaches authenticityToken=props.authenticityToken />,
  "sa-coaches-panel",
)

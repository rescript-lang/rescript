open QuestionsShow__Types

type props = {
  communityId: string,
  target: option<LinkedTarget.t>,
}

let decodeProps = json => {
  open Json.Decode
  {
    communityId: json->field("communityId", string),
    target: json->field("target", nullable(LinkedTarget.decode))->Js.Null.toOption,
  }
}

let props =
  DomUtils.parseJsonAttribute(
    ~id="questions-editor",
    ~attribute="data-json-props",
    (),
  )->decodeProps

ReactDOMRe.renderToElementWithId(
  <QuestionsShow__QuestionEditor communityId=props.communityId target=props.target />,
  "questions-editor",
)

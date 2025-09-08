let f = () => id
let f = () => id

if isArray(children) {
  // Scenario 1
  let code = children->asStringArray->Js.Array2.joinWith("")
  <InlineCode> {code->s} </InlineCode>
} else if isObject(children) {
  // Scenario 2
  children->asElement
} else {
  // Scenario 3
  let code = unknownAsString(children)
  makeCodeElement(~code, ~metastring, ~lang)
}

let getDailyNewCases = x =>
  switch x {
  | First(ret) => ret
  | Pair({prevRecord, record}) =>
    let confirmed = record.confirmed - prevRecord.confirmed
    let deaths = record.deaths - prevRecord.deaths
    {confirmed: confirmed, deaths: deaths}
  }

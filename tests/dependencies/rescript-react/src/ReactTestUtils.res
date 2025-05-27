type undefined = Js.undefined<unit>

let undefined: undefined = Js.Undefined.empty

@module("react-dom/test-utils")
external reactAct: (unit => undefined) => unit = "act"

let act: (unit => unit) => unit = func => {
  let reactFunc = () => {
    func()
    undefined
  }
  reactAct(reactFunc)
}

@module("react-dom/test-utils")
external reactActAsync: (unit => Js.Promise.t<'a>) => Js.Promise.t<unit> = "act"

let actAsync = func => {
  let reactFunc = () => func()
  reactActAsync(reactFunc)
}

@module("react-dom/test-utils")
external isElement: 'element => bool = "isElement"

@module("react-dom/test-utils")
external isElementOfType: ('element, React.component<'props>) => bool = "isElementOfType"

@module("react-dom/test-utils")
external isDOMComponent: 'element => bool = "isDOMComponent"

@module("react-dom/test-utils")
external isCompositeComponent: 'element => bool = "isCompositeComponent"

@module("react-dom/test-utils")
external isCompositeComponentWithType: ('element, React.component<'props>) => bool =
  "isCompositeComponentWithType"

module Simulate = {
  @module("react-dom/test-utils") @scope("Simulate")
  external click: Dom.element => unit = "click"
  @module("react-dom/test-utils") @scope("Simulate")
  external clickWithEvent: (Dom.element, 'event) => unit = "click"
  @module("react-dom/test-utils") @scope("Simulate")
  external change: Dom.element => unit = "change"
  @module("react-dom/test-utils") @scope("Simulate")
  external blur: Dom.element => unit = "blur"
  @module("react-dom/test-utils") @scope("Simulate")
  external changeWithEvent: (Dom.element, 'event) => unit = "change"
  let changeWithValue = (element, value) => {
    let event = {
      "target": {
        "value": value,
      },
    }
    changeWithEvent(element, event)
  }
  let changeWithChecked = (element, value) => {
    let event = {
      "target": {
        "checked": value,
      },
    }
    changeWithEvent(element, event)
  }
  @module("react-dom/test-utils") @scope("Simulate")
  external canPlay: Dom.element => unit = "canPlay"
  @module("react-dom/test-utils") @scope("Simulate")
  external timeUpdate: Dom.element => unit = "timeUpdate"
  @module("react-dom/test-utils") @scope("Simulate")
  external ended: Dom.element => unit = "ended"
  @module("react-dom/test-utils") @scope("Simulate")
  external focus: Dom.element => unit = "focus"
}

@val external document: Dom.document = "document"

@send
external querySelector: (Dom.element, string) => option<Dom.element> = "querySelector"

@send
external querySelectorAll: (Dom.element, string) => Js.Array.array_like<Dom.element> =
  "querySelectorAll"

@get external textContent: Dom.element => string = "textContent"
@get external body: Dom.document => option<Dom.element> = "body"
@send
external createElement: (Dom.document, string) => Dom.element = "createElement"
@send external remove: Dom.element => unit = "remove"
@send
external appendChild: (Dom.element, Dom.element) => Dom.element = "appendChild"

let querySelectorAll = (element, string) => Js.Array.from(querySelectorAll(element, string))

module DOM = {
  @return(nullable) @get
  external value: Dom.element => option<string> = "value"

  let findBySelector = (element, selector) => querySelector(element, selector)

  let findByAllSelector = (element, selector) => querySelectorAll(element, selector)

  let findBySelectorAndTextContent = (element, selector, content) =>
    querySelectorAll(element, selector)->Js.Array2.find(node => node->textContent === content)

  let findBySelectorAndPartialTextContent = (element, selector, content) =>
    querySelectorAll(element, selector)->Js.Array2.find(node =>
      node->textContent->Js.String2.includes(content)
    )
}

let prepareContainer = (container: ref<option<Dom.element>>, ()) => {
  let containerElement = document->createElement("div")
  switch document->body {
  | Some(body) => body->appendChild(containerElement)->ignore
  | None => ()
  }
  container := Some(containerElement)
}

let cleanupContainer = (container: ref<option<Dom.element>>, ()) => {
  switch container.contents {
  | Some(contents) => remove(contents)
  | None => ()
  }
  container := None
}

let getContainer = container =>
  switch container.contents {
  | Some(contents) => contents
  | None => raise(Not_found)
  }

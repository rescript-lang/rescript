open Webapi.Dom

let handleSelection = _ => print_endline("change")

let elm = document->Document.createElement("strong")

Element.addSelectionChangeEventListenerWithOptions(
  handleSelection,
  {"passive": true, "once": true, "capture": false},
  elm,
)
Element.addSelectionChangeEventListenerUseCapture(handleSelection, elm)
Element.removeSelectionChangeEventListener(handleSelection, elm)
Element.removeSelectionChangeEventListenerWithOptions(
  handleSelection,
  {"passive": true, "capture": false},
  elm,
)
Element.removeSelectionChangeEventListenerUseCapture(handleSelection, elm)

let htmlElm =
  document
  ->Document.createElement("strong")
  ->HtmlElement.ofElement
  ->TestHelpers.unsafelyUnwrapOption

HtmlElement.addSelectionChangeEventListenerWithOptions(
  handleSelection,
  {"passive": true, "once": true, "capture": false},
  htmlElm,
)
HtmlElement.addSelectionChangeEventListenerUseCapture(handleSelection, htmlElm)
HtmlElement.removeSelectionChangeEventListener(handleSelection, htmlElm)
HtmlElement.removeSelectionChangeEventListenerWithOptions(
  handleSelection,
  {"passive": true, "capture": false},
  htmlElm,
)
HtmlElement.removeSelectionChangeEventListenerUseCapture(handleSelection, htmlElm)

let htmlDoc = document->Document.asHtmlDocument->TestHelpers.unsafelyUnwrapOption

HtmlDocument.addSelectionChangeEventListenerWithOptions(
  handleSelection,
  {"passive": true, "once": true, "capture": false},
  htmlDoc,
)
HtmlDocument.addSelectionChangeEventListenerUseCapture(handleSelection, htmlDoc)
HtmlDocument.removeSelectionChangeEventListener(handleSelection, htmlDoc)
HtmlDocument.removeSelectionChangeEventListenerWithOptions(
  handleSelection,
  {"passive": true, "capture": false},
  htmlDoc,
)
HtmlDocument.removeSelectionChangeEventListenerUseCapture(handleSelection, htmlDoc)

Window.addSelectionChangeEventListenerWithOptions(
  handleSelection,
  {"passive": true, "once": true, "capture": false},
  window,
)
Window.addSelectionChangeEventListenerUseCapture(handleSelection, window)
Window.removeSelectionChangeEventListener(handleSelection, window)
Window.removeSelectionChangeEventListenerWithOptions(
  handleSelection,
  {"passive": true, "capture": false},
  window,
)
Window.removeSelectionChangeEventListenerUseCapture(handleSelection, window)

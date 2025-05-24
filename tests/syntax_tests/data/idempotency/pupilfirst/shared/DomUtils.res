exception DataElementMissing(string)
exception RootElementMissing(string)
exception RootAttributeMissing(string)

open Webapi.Dom

let parseJsonTag = (~id="react-root-data", ()) =>
  switch document->Document.getElementById(id) {
  | Some(rootElement) => rootElement->Element.innerHTML
  | None => raise(DataElementMissing(id))
  }->Json.parseOrRaise

let parseJsonAttribute = (~id="react-root", ~attribute="data-json-props", ()) =>
  switch document->Document.getElementById(id) {
  | Some(rootElement) =>
    switch rootElement->Element.getAttribute(attribute) {
    | Some(props) => props
    | None => raise(RootAttributeMissing(attribute))
    }
  | None => raise(RootElementMissing(id))
  }->Json.parseOrRaise

let redirect = path => path->Webapi.Dom.Window.setLocation(window)

let reload = () => location->Location.reload

let isDevelopment = () =>
  switch document->Document.documentElement->Element.getAttribute("data-env") {
  | Some(props) if props == "development" => true
  | Some(_)
  | None => false
  }

module FormData = {
  type t = Fetch.formData

  @new external create: Dom.element => t = "FormData"
  @send external append: (t, 'a) => unit = "append"
}

module EventTarget = {
  type t = {.}

  /* Be careful when using this function. Event targets need not be an 'element'. */

  external unsafeToElement: t => Dom.element = "%identity"
  external unsafeToHtmlInputElement: t => Dom.htmlInputElement = "%identity"
}

module Element = {
  type t = Dom.element

  external unsafeToHtmlInputElement: t => Dom.htmlInputElement = "%identity"
}

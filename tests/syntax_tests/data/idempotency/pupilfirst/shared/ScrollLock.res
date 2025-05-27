open Webapi.Dom

let handleScrollLock = add => {
  let classes = add ? "overflow-hidden" : ""

  let body = document->Document.getElementsByTagName("body")->HtmlCollection.toArray

  body[0]->Element.setClassName(classes)
}
let activate = () => handleScrollLock(true)
let deactivate = () => handleScrollLock(false)

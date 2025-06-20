open Webapi.Dom
open Webapi.Dom.DomStringMap

let dataset =
  document->Document.createElement("div")->Element.unsafeAsHtmlElement->HtmlElement.dataset

let () = set("fooKey", "barValue", dataset)
let _ = get("fooKey", dataset)
let () = unsafeDeleteKey("fooKey", dataset)

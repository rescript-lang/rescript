open Webapi.Dom
open EventTarget

let target = document->Document.createElement("strong")->Element.asEventTarget
let event = Event.make("my-event")
let handleClick = _ => print_endline("asd")

addEventListener("click", handleClick, target)
addEventListenerWithOptions(
  "click",
  handleClick,
  {"passive": true, "once": true, "capture": false},
  target,
)
addEventListenerUseCapture("click", handleClick, target)
removeEventListener("click", handleClick, target)
removeEventListenerWithOptions("click", handleClick, {"passive": true, "capture": false}, target)
removeEventListenerUseCapture("click", handleClick, target)
let _ = dispatchEvent(event, target)

/* https://github.com/reasonml-community/bs-webapi-incubator/issues/103 */
let customEvent = CustomEvent.makeWithOptions("custom-event", {"detail": {"test": "test"}})
dispatchEvent(customEvent, target)

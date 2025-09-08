open Webapi.Dom
open SvgZoomEvent

let event = make("my-event")

/* Event */
let _ = bubbles(event)
let _ = cancelable(event)
let _ = composed(event)
let _ = currentTarget(event)
let _ = defaultPrevented(event)
let _ = eventPhase(event)
let _ = target(event)
let _ = timeStamp(event)
let _ = type_(event)
let _ = isTrusted(event)

preventDefault(event)
stopImmediatePropagation(event)
stopPropagation(event)

/* UIEvent */
let _ = detail(event)
let _ = view(event)

/* SvgZoomEvent */
let _ = zoomRectScreen(event)
let _ = previousScale(event)
let _ = previousTranslate(event)
let _ = newScale(event)
let _ = newTranslate(event)

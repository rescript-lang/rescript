open Webapi.Dom
open! KeyboardEvent

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

/* KeyboardEvent */
let _ = altKey(event)
let _ = code(event)
let _ = ctrlKey(event)
let _ = isComposing(event)
let _ = key(event)
let _ = locale(event)
let _ = location(event)
let _ = metaKey(event)
let _ = repeat(event)
let _ = shiftKey(event)
let _ = getModifierState(Alt, event)

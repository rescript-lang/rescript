open Webapi.Dom
open PointerEvent

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

/* MouseEvent */
let _ = altKey(event)
let _ = button(event)
let _ = buttons(event)
let _ = clientX(event)
let _ = clientY(event)
let _ = ctrlKey(event)
let _ = metaKey(event)
let _ = movementX(event)
let _ = movementY(event)
let _ = offsetX(event)
let _ = offsetY(event)
let _ = pageX(event)
let _ = pageY(event)
let _ = region(event)
let _ = relatedTarget(event)
let _ = screenX(event)
let _ = screenY(event)
let _ = shiftKey(event)
let _ = x(event)
let _ = y(event)
let _ = getModifierState(Alt, event)

/* PointerEvent */
let _ = pointerId(event)
let _ = width(event)
let _ = height(event)
let _ = pressure(event)
let _ = tiltX(event)
let _ = tiltY(event)
let _ = pointerType(event)
let _ = isPrimary(event)

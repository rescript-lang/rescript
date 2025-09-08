type t = Dom.pointerEvent
type pointerId = Dom.eventPointerId

include Webapi__Dom__Event.Impl({
  type t = t
})
include Webapi__Dom__UiEvent.Impl({
  type t = t
})
include Webapi__Dom__MouseEvent.Impl({
  type t = t
})

@new external make: string => t = "PointerEvent"
@new external makeWithOptions: (string, {..}) => t = "PointerEvent"

@get external pointerId: t => pointerId = ""
@get external width: t => int = ""
@get external height: t => int = ""
@get external pressure: t => float = ""
@get external tiltX: t => int = ""
@get external tiltY: t => int = ""
@get external pointerType: t => string /* pointerType enum */ = ""
let pointerType: t => Webapi__Dom__Types.pointerType = self =>
  Webapi__Dom__Types.decodePointerType(pointerType(self))
@get external isPrimary: t => bool = ""

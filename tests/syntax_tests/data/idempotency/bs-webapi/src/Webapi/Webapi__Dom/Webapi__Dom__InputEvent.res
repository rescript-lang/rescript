type t = Dom.inputEvent

include Webapi__Dom__Event.Impl({
  type t = t
})
include Webapi__Dom__UiEvent.Impl({
  type t = t
})

@new external make: string => t = "InputEvent"
@new external makeWithOptions: (string, {..}) => t = "InputEvent"

@get external data: t => string = ""
@get external isComposing: t => bool = ""

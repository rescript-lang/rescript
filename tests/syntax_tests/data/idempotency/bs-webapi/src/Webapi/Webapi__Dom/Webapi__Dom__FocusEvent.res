type t = Dom.focusEvent

include Webapi__Dom__Event.Impl({
  type t = t
})
include Webapi__Dom__UiEvent.Impl({
  type t = t
})

@new external make: string => t = "FocusEvent"
@new external makeWithOptions: (string, {..}) => t = "FocusEvent"

@get @return(nullable) external relatedTarget: t => option<Dom.eventTarget> = ""

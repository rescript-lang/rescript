type t = Dom.compositionEvent

include Webapi__Dom__Event.Impl({
  type t = t
})
include Webapi__Dom__UiEvent.Impl({
  type t = t
})

@new external make: string => t = "CompositionEvent"
@new external makeWithOptions: (string, {..}) => t = "CompositionEvent"

@get external data: t => string = ""

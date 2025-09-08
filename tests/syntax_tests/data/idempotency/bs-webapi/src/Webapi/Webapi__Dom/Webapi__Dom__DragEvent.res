type t = Dom.dragEvent

include Webapi__Dom__Event.Impl({
  type t = t
})
include Webapi__Dom__UiEvent.Impl({
  type t = t
})
include Webapi__Dom__MouseEvent.Impl({
  type t = t
})

@new external make: string => t = "DragEvent"
@new external makeWithOptions: (string, {..}) => t = "DragEvent"

@get external dataTransfer: t => Dom.dataTransfer = ""

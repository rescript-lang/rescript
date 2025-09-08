type t = Dom.svgZoomEvent

include Webapi__Dom__Event.Impl({
  type t = t
})
include Webapi__Dom__UiEvent.Impl({
  type t = t
})

@new external make: string => t = "SVGZoomEvent"
@new external makeWithOptions: (string, {..}) => t = "SVGZoomEvent"

@get external zoomRectScreen: t => Dom.svgRect = ""
@get external previousScale: t => float = ""
@get external previousTranslate: t => Dom.svgPoint = ""
@get external newScale: t => float = ""
@get external newTranslate: t => Dom.svgPoint = ""

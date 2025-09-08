type t = Dom.relatedEvent

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "RelatedEvent"
@new external makeWithOptions: (string, {..}) => t = "RelatedEvent"

@get @return(nullable) external relatedTarget: t => option<Dom.eventTarget> = ""

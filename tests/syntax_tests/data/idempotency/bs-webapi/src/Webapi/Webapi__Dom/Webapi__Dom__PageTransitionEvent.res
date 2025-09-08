type t = Dom.pageTransitionEvent

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "PageTransitionEvent"
@new external makeWithOptions: (string, {..}) => t = "PageTransitionEvent"

@get external persisted: t => bool = ""

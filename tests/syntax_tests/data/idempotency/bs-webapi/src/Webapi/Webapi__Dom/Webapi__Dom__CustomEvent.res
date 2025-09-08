type t = Dom.customEvent

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "CustomEvent"
@new external makeWithOptions: (string, {..}) => t = "CustomEvent"

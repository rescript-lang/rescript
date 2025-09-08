type t = Dom.popStateEvent

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "PopStateEvent"
@new external makeWithOptions: (string, {..}) => t = "PopStateEvent"

@get external state: t => {..} = ""

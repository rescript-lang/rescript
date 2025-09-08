type t = Dom.timeEvent

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "TimeEvent"
@new external makeWithOptions: (string, {..}) => t = "TimeEvent"

@get external detail: t => int = ""
@get external view: t => Dom.window = "" /* technically returns a `WindowProxy` */

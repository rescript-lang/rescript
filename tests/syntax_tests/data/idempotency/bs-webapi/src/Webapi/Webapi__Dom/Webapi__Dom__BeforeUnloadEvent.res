type t = Dom.beforeUnloadEvent

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "BeforeUnloadEvent"
@new external makeWithOptions: (string, {..}) => t = "BeforeUnloadEvent"

@get external returnValue: t => string = ""

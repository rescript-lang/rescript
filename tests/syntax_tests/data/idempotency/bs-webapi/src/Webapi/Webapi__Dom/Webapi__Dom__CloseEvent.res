type t = Dom.closeEvent

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "CloseEvent"
@new external makeWithOptions: (string, {..}) => t = "CloseEvent"

@get external wasClean: t => bool = ""
@get external code: t => int = ""
@get external reason: t => string = ""

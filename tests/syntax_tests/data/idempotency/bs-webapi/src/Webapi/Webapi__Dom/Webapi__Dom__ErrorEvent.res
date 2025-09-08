type t = Dom.errorEvent

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "ErrorEvent"
@new external makeWithOptions: (string, {..}) => t = "ErrorEvent"

@get external message: t => string = ""
@get external filename: t => string = ""
@get external lineno: t => int = ""
@get external colno: t => int = ""
@get external error: t => {..} = ""

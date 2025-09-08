type t = Dom.clipboardEvent

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "ClipboardEvent"
@new external makeWithOptions: (string, {..}) => t = "ClipboardEvent"

@get external clipboardData: t => Dom.dataTransfer = ""

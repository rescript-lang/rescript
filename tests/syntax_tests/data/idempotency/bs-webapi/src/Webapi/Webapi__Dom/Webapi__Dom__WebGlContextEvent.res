type t = Dom.webGlContextEvent

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "WebGLContextEvent"
@new external makeWithOptions: (string, {..}) => t = "WebGLContextEvent"

@get external statusMessage: t => string = ""

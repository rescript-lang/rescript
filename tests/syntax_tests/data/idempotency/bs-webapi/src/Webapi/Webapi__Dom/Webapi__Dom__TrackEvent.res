type t = Dom.trackEvent
type track /* TODO: VideoTrack or AudioTrack or TextTrack */

include Webapi__Dom__Event.Impl({
  type t = t
})

@new external make: string => t = "TrackEvent"
@new external makeWithOptions: (string, {..}) => t = "TrackEvent"

@get external track: t => track = ""

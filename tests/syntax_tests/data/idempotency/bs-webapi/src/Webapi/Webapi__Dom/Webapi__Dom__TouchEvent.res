type touchList /* TODO, Touch Events */

module Impl = (
  T: {
    type t
  },
) => {
  @get external altKey: T.t => bool = ""
  @get external changedTouches: T.t => touchList = ""
  @get external ctrlKey: T.t => bool = ""
  @get external metaKey: T.t => bool = ""
  @get external shiftKey: T.t => bool = ""
  @get external targetTouches: T.t => touchList = ""
  @get external touches: T.t => touchList = ""
}

type t = Dom.touchEvent

include Webapi__Dom__Event.Impl({
  type t = t
})
include Webapi__Dom__UiEvent.Impl({
  type t = t
})
include Impl({
  type t = t
})

@new external make: string => t = "TouchEvent"
@new external makeWithOptions: (string, {..}) => t = "TouchEvent"

module Impl = (
  T: {
    type t
  },
) => {
  @get external altKey: T.t => bool = ""
  @get external button: T.t => int = ""
  @get external buttons: T.t => int /* bitmask */ = ""
  @get external clientX: T.t => int = ""
  @get external clientY: T.t => int = ""
  @get external ctrlKey: T.t => bool = ""
  @get external metaKey: T.t => bool = ""
  @get external movementX: T.t => int = ""
  @get external movementY: T.t => int = ""
  @get external offsetX: T.t => int = "" /* experimental, but widely supported */
  @get external offsetY: T.t => int = "" /* experimental, but widely supported */
  @get external pageX: T.t => int = "" /* experimental, but widely supported */
  @get external pageY: T.t => int = "" /* experimental, but widely supported */
  @get @return(nullable) external region: T.t => option<string> = ""
  @get @return(nullable) external relatedTarget: T.t => option<Dom.eventTarget> = ""
  @get external screenX: T.t => int = ""
  @get external screenY: T.t => int = ""
  @get external shiftKey: T.t => bool = ""
  @get external x: T.t => int = "" /* experimental */
  @get external y: T.t => int = "" /* experimental */
  let getModifierState: (Webapi__Dom__Types.modifierKey, T.t) => bool = (key, self) =>
    getModifierState(Webapi__Dom__Types.encodeModifierKey(key), self)
}

type t = Dom.mouseEvent

include Webapi__Dom__Event.Impl({
  type t = t
})
include Webapi__Dom__UiEvent.Impl({
  type t = t
})
include Impl({
  type t = t
})

@new external make: string => t = "MouseEvent"
@new external makeWithOptions: (string, {..}) => t = "MouseEvent"

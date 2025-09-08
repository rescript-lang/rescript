type t = Dom.shadowRoot

include Webapi__Dom__Node.Impl({
  type t = t
})
include Webapi__Dom__EventTarget.Impl({
  type t = t
})
include Webapi__Dom__NonElementParentNode.Impl({
  type t = t
})
include Webapi__Dom__DocumentOrShadowRoot.Impl({
  type t = t
})
include Webapi__Dom__ParentNode.Impl({
  type t = t
})

@get external shadowRootMode: t => string = ""
let shadowRootMode: t => Webapi__Dom__Types.shadowRootMode = self =>
  Webapi__Dom__Types.decodeShadowRootMode(shadowRootMode(self))
@get external host: t => Dom.element = ""

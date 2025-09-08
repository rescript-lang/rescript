type t = Dom.documentFragment

include Webapi__Dom__Node.Impl({
  type t = t
})
include Webapi__Dom__EventTarget.Impl({
  type t = t
})
include Webapi__Dom__NonElementParentNode.Impl({
  type t = t
})
include Webapi__Dom__ParentNode.Impl({
  type t = t
})

module Impl = (
  T: {
    type t
  },
) => {
  @get external data: T.t => string = ""
  @get external length: T.t => int = ""

}

type t = Dom.characterData

include Webapi__Dom__Node.Impl({
  type t = t
})
include Webapi__Dom__EventTarget.Impl({
  type t = t
})
include Webapi__Dom__NonDocumentTypeChildNode.Impl({
  type t = t
})
include Webapi__Dom__ChildNode.Impl({
  type t = t
})
include Impl({
  type t = t
})

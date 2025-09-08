type t = Dom.documentType

include Webapi__Dom__Node.Impl({
  type t = t
})
include Webapi__Dom__EventTarget.Impl({
  type t = t
})
include Webapi__Dom__ChildNode.Impl({
  type t = t
})

@get external name: t => string = ""
@get external publicId: t => string = ""
@get external systemId: t => string = ""

type t = Dom.attr

include Webapi__Dom__Node.Impl({
  type t = t
})
include Webapi__Dom__EventTarget.Impl({
  type t = t
})

@get external namespaceURI: t => string = ""
@get external prefix: t => string = ""
@get external localName: t => string = ""
@get external name: t => string = ""
@get external value: t => string = ""
@get @return(nullable) external ownerElement: t => option<Dom.element> = ""
@get external specified: t => bool = "" /* useless; always returns true (exact wording from spec) */

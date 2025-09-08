module Impl = (
  T: {
    type t
  },
) => {
  external asDocument: T.t => Dom.document = "%identity"

  let asHtmlDocument: T.t => Js.null<Dom.htmlDocument> = %raw(`
    function (document) {
      return document.doctype.name === "html" ?  document : null;
    }
  `)
  @deprecated("Will fail if no doctype is defined, consider using unsafeAsHtmlDocument instead")
  let asHtmlDocument: T.t => option<Dom.htmlDocument> = self =>
    Js.Null.toOption(asHtmlDocument(self))

  external unsafeAsHtmlDocument: T.t => Dom.htmlDocument = "%identity"

  let ofNode = (node: Dom.node): option<T.t> =>
    Webapi__Dom__Node.nodeType(node) == Document ? Some(Obj.magic(node)) : None

  @get external characterSet: T.t => string = ""
  @get external compatMode: T.t => string /* compatMode enum */ = "" /* experimental */
  let compatMode: T.t => Webapi__Dom__Types.compatMode = self =>
    Webapi__Dom__Types.decodeCompatMode(compatMode(self))
  @get external doctype: T.t => Dom.documentType = ""
  @get external documentElement: T.t => Dom.element = ""
  @get external documentURI: T.t => string = ""
  @get external hidden: T.t => bool = ""
  @get external implementation: T.t => Dom.domImplementation = ""
  @get external lastStyleSheetSet: T.t => string = ""
  @get @return(nullable)
  external pointerLockElement: T.t => option<Dom.element> = "" /* experimental */

  @get external preferredStyleSheetSet: T.t => string = ""
  @get @return(nullable) external scrollingElement: T.t => option<Dom.element> = ""
  @get external selectedStyleSheetSet: T.t => string = ""
  @set external setSelectedStyleSheetSet: (T.t, string) => unit = "selectedStyleSheetSet"
  @get
  external styleSheets: T.t => array<Dom.cssStyleSheet> = "" /* return StyleSheetList, not array */
  @get external styleSheetSets: T.t => array<string> = ""
  @get external visibilityState: T.t => string /* visibilityState enum */ = ""
  let visibilityState: T.t => Webapi__Dom__Types.visibilityState = self =>
    Webapi__Dom__Types.decodeVisibilityState(visibilityState(self))


  @@ocaml.doc(
    " XPath stuff "
    /* createExpression */
    /* createNSResolver */
    /* evaluate */

    /* GlobalEventHandlers interface */
  )
}

type t = Dom.document

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
include Impl({
  type t = t
})

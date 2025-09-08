module Impl = (
  T: {
    type t
  },
) => {
  type t_htmlDocument = T.t

  @get @return(nullable) external activeElement: t_htmlDocument => option<Dom.element> = ""
  @get @return(nullable)
  external body: t_htmlDocument => option<Dom.element> = "" /* returns option HTMLBodyElement */
  @set
  external setBody: (t_htmlDocument, Dom.element) => unit = "body" /* accepth HTMLBodyElement */
  @get external cookie: t_htmlDocument => string = ""
  @set external setCookie: (t_htmlDocument, string) => unit = "cookie"
  @get @return(nullable) external defaultView: t_htmlDocument => option<Dom.window> = ""
  @get external designMode: t_htmlDocument => string /* designMode enum */ = ""
  let designMode: t_htmlDocument => Webapi__Dom__Types.designMode = self =>
    Webapi__Dom__Types.decodeDesignMode(designMode(self))
  @set external setDesignMode: (t_htmlDocument, string /* designMode enum */) => unit = "designMode"
  let setDesignMode: (t_htmlDocument, Webapi__Dom__Types.designMode) => unit = (self, value) =>
    setDesignMode(self, Webapi__Dom__Types.encodeDesignMode(value))
  @get external dir: t_htmlDocument => string /* dir enum */ = ""
  let dir: t_htmlDocument => Webapi__Dom__Types.dir = self =>
    Webapi__Dom__Types.decodeDir(dir(self))
  @set external setDir: (t_htmlDocument, string /* dir enum */) => unit = "dir"
  let setDir: (t_htmlDocument, Webapi__Dom__Types.dir) => unit = (self, value) =>
    setDir(self, Webapi__Dom__Types.encodeDir(value))
  @get @return(nullable) external domain: t_htmlDocument => option<string> = ""
  @set external setDomain: (t_htmlDocument, string) => unit = "domain"
  @get external embeds: t_htmlDocument => Dom.nodeList = ""
  @get external forms: t_htmlDocument => Dom.htmlCollection = ""
  @get external head: t_htmlDocument => Dom.element = "" /* returns HTMLHeadElement */
  @get external images: t_htmlDocument => Dom.htmlCollection = ""
  @get external lastModified: t_htmlDocument => string = ""
  @get external links: t_htmlDocument => Dom.nodeList = ""
  @get external location: t_htmlDocument => Dom.location = ""
  @set external setLocation: (t_htmlDocument, string) => unit = "location"
  @get external plugins: t_htmlDocument => Dom.htmlCollection = ""
  @get external readyState: t_htmlDocument => string /* enum */ = ""
  let readyState: t_htmlDocument => Webapi__Dom__Types.readyState = self =>
    Webapi__Dom__Types.decodeReadyState(readyState(self))
  @get external referrer: t_htmlDocument => string = ""
  @get external scripts: t_htmlDocument => Dom.htmlCollection = ""
  @get external title: t_htmlDocument => string = ""
  @set external setTitle: (t_htmlDocument, string) => unit = "title"
  @get external url: t_htmlDocument => string = "URL"

}

type t = Dom.htmlDocument

include Webapi__Dom__Node.Impl({
  type t = t
})
include Webapi__Dom__EventTarget.Impl({
  type t = t
})
include Webapi__Dom__GlobalEventHandlers.Impl({
  type t = t
})
include Webapi__Dom__Document.Impl({
  type t = t
})
include Impl({
  type t = t
})

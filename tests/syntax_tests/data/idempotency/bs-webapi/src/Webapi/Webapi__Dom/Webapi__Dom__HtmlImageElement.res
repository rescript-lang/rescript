type t

@new external make: unit => t = "Image"
@new external makeWithSize: (int, int) => t = "Image"

@get external alt: t => string = ""
@set external setAlt: (t, string) => unit = "alt"
@get external src: t => string = ""
@set external setSrc: (t, string) => unit = "src"
@get external srcset: t => string = ""
@set external setSrcset: (t, string) => unit = "srcset"
@get external sizes: t => string = ""
@set external setSizes: (t, string) => unit = "sizes"
@get @return(nullable) external crossOrigin: t => option<string> = ""
@set external setCrossOrigin: (t, Js.null<string>) => unit = "crossOrigin"
let setCrossOrigin = (self, value) => setCrossOrigin(self, Js.Null.fromOption(value))
@get external useMap: t => string = ""
@set external setUseMap: (t, string) => unit = "useMap"
@get external isMap: t => bool = ""
@set external setIsMap: (t, bool) => unit = "isMap"
@get external height: t => int = ""
@set external setHeight: (t, int) => unit = "height"
@get external width: t => int = ""
@set external setWidth: (t, int) => unit = "width"
@get external naturalHeight: t => int = ""
@get external naturalWidth: t => int = ""
@get external complete: t => bool = ""
@get external currentSrc: t => string = ""
@get external referrerPolicy: t => string = ""
@set external setReferrerPolicy: (t, string) => unit = "referrerPolicy"
@get external decoding: t => string = ""
@set external setDecoding: (t, string) => unit = "decoding"

include Webapi__Dom__Node.Impl({
  type t = t
})
include Webapi__Dom__EventTarget.Impl({
  type t = t
})
include Webapi__Dom__Element.Impl({
  type t = t
})
include Webapi__Dom__HtmlElement.Impl({
  type t = t
})

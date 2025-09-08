include Css_Js_Core
include Css_Colors

include Css_Js_Core.Make({
  @module("emotion")
  external mergeStyles: (. array<string>) => string = "cx"

  @module("emotion") external make: (. Js.Json.t) => string = "css"

  @module("emotion")
  external injectRule: (. Js.Json.t) => unit = "injectGlobal"

  @module("emotion")
  external injectRaw: (. string) => unit = "injectGlobal"

  @module("emotion")
  external makeKeyFrames: (. dict<Js.Json.t>) => string = "keyframes"
})

type cache

@module("emotion") external cache: cache = "cache"

let fontFace = (~fontFamily, ~src, ~fontStyle=?, ~fontWeight=?, ~fontDisplay=?, ()) => {
  insertRule(.
    Css_Js_Core.fontFace(~fontFamily, ~src, ~fontStyle?, ~fontWeight?, ~fontDisplay?, ()),
  )
  fontFamily
}

module Canvas2d = Webapi__Canvas__Canvas2d
module WebGl = Webapi__Canvas__WebGl

module CanvasElement = {
  @send external getContext2d: (Dom.element, @as("2d") _) => Canvas2d.t = "getContext"
  @send external getContextWebGl: (Dom.element, @as("webgl") _) => WebGl.glT = "getContext"
  @get external height: Dom.element => int = "height"
  @set external setHeight: (Dom.element, int) => unit = "height"
  @get external width: Dom.element => int = "width"
  @set external setWidth: (Dom.element, int) => unit = "width"
}

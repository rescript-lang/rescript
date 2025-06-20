type t /* Main type, representing the 2d canvas rendering context object */
type gradient
type pattern
type measureText

/* Sub-modules (and their interfaces) for string enum arguments: */
module type CompositeType = {
  type t = private string

  let sourceOver: t
  let sourceIn: t
  let sourceOut: t
  let sourceAtop: t
  let destinationOver: t
  let destinationIn: t
  let destinationOut: t
  let destinationAtop: t
  let lighter: t
  let copy: t
  let xor: t
}

module Composite: CompositeType = {
  type t = string

  let sourceOver: t = "source-over"
  let sourceIn: t = "source-in"
  let sourceOut: t = "source-out"
  let sourceAtop: t = "source-atop"
  let destinationOver: t = "destination-over"
  let destinationIn: t = "destination-in"
  let destinationOut: t = "destination-out"
  let destinationAtop: t = "destination-atop"
  let lighter: t = "lighter"
  let copy: t = "copy"
  let xor: t = "xor"
}

module type LineCapType = {
  type t = private string

  let butt: t
  let round: t
  let square: t
}

module LineCap: LineCapType = {
  type t = string

  let butt: t = "butt"
  let round: t = "round"
  let square: t = "square"
}

module type LineJoinType = {
  type t = private string

  let round: t
  let bevel: t
  let miter: t
}

module LineJoin: LineJoinType = {
  type t = string

  let round: t = "round"
  let bevel: t = "bevel"
  let miter: t = "miter"
}

type rec image<'a> =
  | Number: image<float>
  | ImageData: image<Webapi__Dom__Image.t>

type rec style<_> =
  | String: style<string>
  | Gradient: style<gradient>
  | Pattern: style<pattern>


/* Transformation */
external transform: (
  ~m11: float,
  ~m12: float,
  ~m21: float,
  ~m22: float,
  ~dx: float,
  ~dy: float,
) => unit = ""

/* Compositing */
@set external globalAlpha: (t, float) => unit = ""
@set external globalCompositeOperation: (t, Composite.t) => unit = ""

/* Line Styles */
@set external lineWidth: (t, float) => unit = ""
@set external lineCap: (t, LineCap.t) => unit = ""
@set external lineJoin: (t, LineJoin.t) => unit = ""
@set external miterLimit: (t, float) => unit = ""

/* Colors, Styles, and Shadows */
@set external setFillStyle: (t, 'a) => unit = "fillStyle"
@set external setStrokeStyle: (t, 'a) => unit = "strokeStyle"

/* in re unused warnings
   awaiting release of https://github.com/rescript-lang/rescript-compiler/issues/1656
   to just use [@@set] directly with an ignored (style a) */
let setStrokeStyle = (type a, ctx: t, _: style<a>, v: a) => setStrokeStyle(ctx, v)

let setFillStyle = (type a, ctx: t, _: style<a>, v: a) => setFillStyle(ctx, v)

let reifyStyle = (type a, x: 'a): (style<a>, a) => {
  module Internal = {
    type constructor
    @val external canvasGradient: constructor = "CanvasGradient" /* internal */
    @val external canvasPattern: constructor = "CanvasPattern" /* internal */
    let instanceOf: (
      'a,
      constructor,
    ) => bool = %raw(`function(x,y) {return +(x instanceof y)}`) /* internal */
  }

  (
    if Js.typeof(x) == "string" {
      Obj.magic(String)
    } else if Internal.instanceOf(x, Internal.canvasGradient) {
      Obj.magic(Gradient)
    } else if Internal.instanceOf(x, Internal.canvasPattern) {
      Obj.magic(Pattern)
    } else {
      throw(
        Invalid_argument(
          "Unknown canvas style kind. Known values are: String, CanvasGradient, CanvasPattern",
        ),
      )
    },
    Obj.magic(x),
  )
}

@get external fillStyle: t => 'a = ""
@get external strokeStyle: t => 'a = ""

let fillStyle = (ctx: t) => ctx->fillStyle->reifyStyle

let strokeStyle = (ctx: t) => ctx->strokeStyle->reifyStyle

@set external shadowOffsetX: (t, float) => unit = ""
@set external shadowOffsetY: (t, float) => unit = ""
@set external shadowBlur: (t, float) => unit = ""
@set external shadowColor: (t, string) => unit = ""

/* Gradients */

/* Text */
@set external font: (t, string) => unit = ""
@set external textAlign: (t, string) => unit = ""
@set external textBaseline: (t, string) => unit = ""
@get external width: measureText => float = ""

/* Rectangles */

@send
external createImageDataCoords: (t, ~width: float, ~height: float) => Webapi__Dom__Image.t =
  "createImageData"
@send
external createImageDataFromImage: (t, Webapi__Dom__Image.t) => Webapi__Dom__Image.t =
  "createImageData"

@send
external getImageData: (t, ~sx: float, ~sy: float, ~sw: float, ~sh: float) => Webapi__Dom__Image.t =
  ""
@send
external putImageData: (
  t,
  ~imageData: Webapi__Dom__Image.t,
  ~dx: float,
  ~dy: float,
  ~dirtyX: float=?,
  ~dirtyY: float=?,
  ~dirtyWidth: float=?,
  ~dirtyHeight: float=?,
  unit,
) => unit = ""

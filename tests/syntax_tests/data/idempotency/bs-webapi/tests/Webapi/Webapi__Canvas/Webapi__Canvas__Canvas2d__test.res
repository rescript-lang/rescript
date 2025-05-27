open Webapi.Canvas
open Webapi.Canvas.Canvas2d
open Webapi.Dom

let canvasEl = Document.createElement("canvas", document)
let ctx = CanvasElement.getContext2d(canvasEl)

ctx->save
ctx->restore

ctx->scale(~x=1., ~y=2.)
ctx->rotate(2.)
ctx->translate(~x=2., ~y=3.)
ctx->transform(~m11=1., ~m12=2., ~m21=1., ~m22=1., ~dx=1., ~dy=1.)

globalAlpha(ctx, 0.9)
globalCompositeOperation(ctx, Composite.sourceOver)

lineWidth(ctx, 1.)
lineCap(ctx, LineCap.butt)
lineJoin(ctx, LineJoin.round)
miterLimit(ctx, 10.)

setStrokeStyle(ctx, String, "red")
setFillStyle(ctx, String, "red")

switch fillStyle(ctx) {
| (Gradient, g) => g->addColorStop(0.0, "red")
| (String, s) => Js.log(s)
| _ => ()
}

switch strokeStyle(ctx) {
| (Gradient, g) => g->addColorStop(1.2, "blue")
| (String, s) => Js.log(s)
| _ => ()
}

shadowOffsetX(ctx, 1.)
shadowOffsetY(ctx, 1.)
shadowBlur(ctx, 1.)
shadowColor(ctx, "red")

ctx->beginPath
ctx->closePath
ctx->fill
ctx->stroke
ctx->clip
ctx->moveTo(~x=1., ~y=1.)
ctx->lineTo(~x=1., ~y=2.)
ctx->quadraticCurveTo(~cp1x=1., ~cp1y=1., ~x=1., ~y=1.)
ctx->bezierCurveTo(~cp1x=1., ~cp1y=1., ~cp2x=2., ~cp2y=2., ~x=4., ~y=4.)
ctx->arcTo(~x1=1., ~y1=1., ~x2=2., ~y2=2., ~r=4.)
ctx->arc(~x=1., ~y=1., ~r=4., ~startAngle=1., ~endAngle=3., ~anticw=true)
ctx->rect(~x=0., ~y=0., ~w=10., ~h=10.)
let _ = ctx->isPointInPath(~x=0., ~y=0.)

let linearGradient = ctx->createLinearGradient(~x0=0.0, ~y0=0.0, ~x1=0.0, ~y1=0.0)
setStrokeStyle(ctx, Gradient, linearGradient)
let _ = ctx->createRadialGradient(~x0=0.0, ~y0=0.0, ~x1=0.0, ~y1=0.0, ~r0=0.0, ~r1=0.0)
linearGradient->addColorStop(0.0, "red")
let _ = List.map(
  createPattern(ctx, Document.createElement("img", document)),
  list{#noRepeat, #repeat, #repeatX, #repeatY},
)

let measureText = ctx->measureText("foo")
let width = width(measureText)
ctx->fillText("foo!", ~x=0.0, ~y=0.0, ~maxWidth=width)
ctx->strokeText("foo!", ~x=0.0, ~y=0.0, ~maxWidth=width)
let imageData = createImageDataCoords(ctx, ~width=0.0, ~height=0.0)
createImageDataFromImage(ctx, imageData)
Image.width(imageData)
Image.height(imageData)

getImageData(ctx, ~sx=0.0, ~sy=0.0, ~sw=0.0, ~sh=0.0)
let _: unit = putImageData(ctx, ~imageData, ~dx=0.0, ~dy=0.0, ())
let _: unit = putImageData(
  ctx,
  ~imageData,
  ~dx=0.0,
  ~dy=0.0,
  ~dirtyX=0.0,
  ~dirtyY=0.0,
  ~dirtyWidth=0.0,
  ~dirtyHeight=0.0,
  (),
)

font(ctx, "10px Courier")
textAlign(ctx, "left")
textBaseline(ctx, "top")
ctx->fillText("hi", ~x=1., ~y=0.)
ctx->strokeText("hi", ~x=1., ~y=0.)

ctx->fillRect(~x=1., ~y=0., ~w=10., ~h=10.)
ctx->strokeRect(~x=1., ~y=0., ~w=10., ~h=10.)
ctx->clearRect(~x=1., ~y=0., ~w=10., ~h=10.)

open Webapi.Dom.Image

let imageData = make(~width=0.0, ~height=0.0)

let arr = Js.Typed_array.Uint8ClampedArray.make([])
let _ = makeWithData(~array=arr, ~width=0.0, ~height=0.0)

let _ = height(imageData)
let _ = width(imageData)

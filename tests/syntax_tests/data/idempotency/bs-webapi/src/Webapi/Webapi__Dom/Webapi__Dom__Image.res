type t

@new
external makeWithData: (
  ~array: Js.Typed_array.Uint8ClampedArray.t,
  ~width: float,
  ~height: float,
) => t = "ImageData"

@new external make: (~width: float, ~height: float) => t = "ImageData"

@get external data: t => Js.Typed_array.Uint8ClampedArray.t = ""
@get external height: t => float = ""
@get external width: t => float = ""

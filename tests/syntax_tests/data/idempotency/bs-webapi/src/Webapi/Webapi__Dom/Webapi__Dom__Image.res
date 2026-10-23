type t

@new
external makeWithData: (
  ~array: Uint8ClampedArray.t,
  ~width: float,
  ~height: float,
) => t = "ImageData"

@new external make: (~width: float, ~height: float) => t = "ImageData"

@get external data: t => Uint8ClampedArray.t = ""
@get external height: t => float = ""
@get external width: t => float = ""

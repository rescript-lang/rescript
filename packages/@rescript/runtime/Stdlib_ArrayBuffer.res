@instanceOf("ArrayBuffer")
type t

@new external make: int => t = "ArrayBuffer"
@get external byteLength: t => int = "byteLength"

@send external slice: (t, ~start: int=?, ~end: int=?) => t = "slice"

external ignore: t => unit = "%ignore"

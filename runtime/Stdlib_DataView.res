type t

@new external fromBuffer: Stdlib_ArrayBuffer.t => t = "DataView"
@new external fromBufferToEnd: (Stdlib_ArrayBuffer.t, ~byteOffset: int) => t = "DataView"
@new
external fromBufferWithRange: (Stdlib_ArrayBuffer.t, ~byteOffset: int, ~length: int) => t =
  "DataView"

@get external buffer: t => Stdlib_ArrayBuffer.t = "buffer"
@get external byteLength: t => int = "byteLength"
@get external byteOffset: t => int = "byteOffset"

@send external getInt8: (t, int) => int = "getInt8"
@send external getUint8: (t, int) => int = "getUint8"
@send external getInt16: (t, int) => int = "getInt16"
@send external getUint16: (t, int) => int = "getUint16"
@send external getInt32: (t, int) => int = "getInt32"
@send external getUint32: (t, int) => int = "getUint32"

@send external getFloat32: (t, int) => float = "getFloat32"
@send external getFloat64: (t, int) => float = "getFloat64"

@send external getBigInt64: (t, int) => bigint = "getBigInt64"
@send external getBigUint64: (t, int) => bigint = "getBigUint64"

@send external setInt8: (t, int, int) => unit = "setInt8"
@send external setUint8: (t, int, int) => unit = "setUint8"
@send external setInt16: (t, int, int) => unit = "setInt16"
@send external setUint16: (t, int, int) => unit = "setUint16"
@send external setInt32: (t, int, int) => unit = "setInt32"
@send external setUint32: (t, int, int) => unit = "setUint32"

@send external setFloat32: (t, int, float) => unit = "setFloat32"
@send external setFloat64: (t, int, float) => unit = "setFloat64"

@send external setBigInt64: (t, int, bigint) => unit = "setBigInt64"
@send external setBigUint64: (t, int, bigint) => unit = "setBigUint64"

/**
  `ignore(dataView)` ignores the provided dataView and returns unit.

  This helper is useful when you want to discard a value (for example, the result of an operation with side effects)
  without having to store or process it further.
*/
external ignore: t => unit = "%ignore"

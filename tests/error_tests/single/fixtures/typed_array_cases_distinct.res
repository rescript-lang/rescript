// Each typed array is a type of its own, so one cannot stand in for another
@unboxed type ta = I8(Int8Array.t) | U8(Uint8Array.t)

let x = I8(Uint8Array.fromArray([1]))

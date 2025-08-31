let arr1 = Js.Typed_array.Int8Array.make([1, 2, 3])

let len = arr1->Js.Typed_array.Int8Array.length

let bytes = Js.Typed_array.Int8Array._BYTES_PER_ELEMENT
let off = Js.Typed_array.Int8Array.fromBufferOffset(ArrayBuffer.make(8), 2)
let range = Js.Typed_array.Int8Array.fromBufferRange(ArrayBuffer.make(8), ~offset=2, ~length=2)

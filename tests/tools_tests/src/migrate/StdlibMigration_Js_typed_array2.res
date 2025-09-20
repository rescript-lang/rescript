let arr = Js.TypedArray2.Int8Array.make([1, 2, 3])

let len1 = arr->Js.TypedArray2.Int8Array.length
let includes1 = arr->Js.TypedArray2.Int8Array.includes(2)
let idxFrom1 = arr->Js.TypedArray2.Int8Array.indexOfFrom(2, ~from=1)

let slice1 = arr->Js.TypedArray2.Int8Array.slice(~start=1, ~end_=2)
let sliceFrom1 = arr->Js.TypedArray2.Int8Array.sliceFrom(1)

let map1 = arr->Js.TypedArray2.Int8Array.map(x => x + 1)
let reduce1 = arr->Js.TypedArray2.Int8Array.reduce((acc, x) => acc + x, 0)

let bytes = Js.TypedArray2.Int8Array._BYTES_PER_ELEMENT

let fromBufToEnd = Js.TypedArray2.Int8Array.fromBufferOffset(ArrayBuffer.make(8), 2)
let fromBufRange = Js.TypedArray2.Int8Array.fromBufferRange(
  ArrayBuffer.make(8),
  ~offset=2,
  ~length=2,
)

let fromLength = Js.TypedArray2.Int8Array.fromLength(3)

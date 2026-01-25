let arr = Js.TypedArray2.Float32Array.make([1.0, 2.0, 3.0])

let len1 = arr->Js.TypedArray2.Float32Array.length
let includes1 = arr->Js.TypedArray2.Float32Array.includes(2.0)
let idxFrom1 = arr->Js.TypedArray2.Float32Array.indexOfFrom(2.0, ~from=1)

let slice1 = arr->Js.TypedArray2.Float32Array.slice(~start=1, ~end_=2)
let sliceFrom1 = arr->Js.TypedArray2.Float32Array.sliceFrom(1)

let map1 = arr->Js.TypedArray2.Float32Array.map(x => x +. 1.0)
let reduce1 = arr->Js.TypedArray2.Float32Array.reduce((acc, x) => acc +. x, 0.0)

let bytes = Js.TypedArray2.Float32Array._BYTES_PER_ELEMENT

let fromBufToEnd = Js.TypedArray2.Float32Array.fromBufferOffset(ArrayBuffer.make(8), 2)
let fromBufRange = Js.TypedArray2.Float32Array.fromBufferRange(
  ArrayBuffer.make(8),
  ~offset=2,
  ~length=2,
)

let fromLength = Js.TypedArray2.Float32Array.fromLength(3)

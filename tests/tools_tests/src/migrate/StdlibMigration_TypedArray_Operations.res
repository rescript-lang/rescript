let ta = Uint8Array.fromArray([1, 2, 3, 4])

let a1 = ta->TypedArray.copyWithinToEnd(~target=1, ~start=2)
let a2 = ta->TypedArray.fillToEnd(9, ~start=1)
let a3 = ta->TypedArray.sliceToEnd(~start=1)
let a4 = ta->TypedArray.subarrayToEnd(~start=1)

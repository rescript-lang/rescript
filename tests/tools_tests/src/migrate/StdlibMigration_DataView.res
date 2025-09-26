let a = DataView.fromBufferToEnd(ArrayBuffer.make(8), ~byteOffset=2)
let b = DataView.fromBufferWithRange(ArrayBuffer.make(8), ~byteOffset=2, ~length=4)

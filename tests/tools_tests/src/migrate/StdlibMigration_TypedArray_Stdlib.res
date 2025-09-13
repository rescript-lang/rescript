let a = Uint8Array.fromBufferToEnd(ArrayBuffer.make(8), ~byteOffset=2)
let b = Uint8Array.fromBufferWithRange(ArrayBuffer.make(8), ~byteOffset=2, ~length=2)
let c = Uint8Array.fromArrayLikeOrIterableWithMap([1, 2], (v, i) => v)

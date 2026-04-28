// Migration tests for Js.Array (old) -> Array module

external someArrayLike: Js_array.array_like<string> = "whatever"

let from1 = someArrayLike->Js.Array.from
let from2 = Js.Array.from(someArrayLike)

let fromMap1 = someArrayLike->Js.Array.fromMap(s => s ++ "!")
let fromMap2 = Js.Array.fromMap(someArrayLike, s => s ++ "!")

let isArray1 = [1, 2, 3]->Js.Array.isArray
let isArray2 = Js.Array.isArray([1, 2, 3])

let length1 = [1, 2, 3]->Js.Array.length
let length2 = Js.Array.length([1, 2, 3])

let pop1 = [1, 2, 3]->Js.Array.pop
let pop2 = Js.Array.pop([1, 2, 3])

let reverseInPlace1 = [1, 2, 3]->Js.Array.reverseInPlace
let reverseInPlace2 = Js.Array.reverseInPlace([1, 2, 3])

let shift1 = [1, 2, 3]->Js.Array.shift
let shift2 = Js.Array.shift([1, 2, 3])

let toString1 = [1, 2, 3]->Js.Array.toString
let toString2 = Js.Array.toString([1, 2, 3])

let toLocaleString1 = [1, 2, 3]->Js.Array.toLocaleString
let toLocaleString2 = Js.Array.toLocaleString([1, 2, 3])

// Type alias migration
let arrT: Js.Array.t<int> = [1, 2, 3]

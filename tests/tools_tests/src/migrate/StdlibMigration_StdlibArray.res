let ba1 = [1, 2, 3, 4, 5]->Array.copyAllWithin(~target=2)
let ba2 = Array.copyAllWithin([1, 2, 3, 4, 5], ~target=2)

let b1 = [1, 2, 3, 4]->Array.copyWithinToEnd(~target=0, ~start=2)
let b2 = Array.copyWithinToEnd([1, 2, 3, 4], ~target=0, ~start=2)

let c1 = [1, 2, 3]->Array.fillAll(0)
let c2 = [1, 2, 3, 4]->Array.fillToEnd(9, ~start=1)

let d1 = [1, 2, 1, 2]->Array.indexOfFrom(2, 2)
let d2 = Array.indexOfFrom([1, 2, 1, 2], 2, 2)

let e1 = ["a", "b"]->Array.joinWith("-")
let e2 = [1, 2]->Array.joinWithUnsafe(",")

let f1 = [1, 2, 3, 4]->Array.sliceToEnd(~start=2)

let g1 = [1, 2]->Array.lastIndexOfFrom(1, 1)

let h1 = [1, 2]->Array.unsafe_get(1)

let shift1 = [1, 2, 3]->Js.Array2.shift
let shift2 = Js.Array2.shift([1, 2, 3])

let slice1 = [1, 2, 3]->Js.Array2.slice(~start=1, ~end_=2)
let slice2 = Js.Array2.slice([1, 2, 3], ~start=1, ~end_=2)

external someArrayLike: Js_array2.array_like<string> = "whatever"

let from1 = someArrayLike->Js.Array2.from
let from2 = Js.Array2.from(someArrayLike)

let fromMap1 = someArrayLike->Js.Array2.fromMap(s => s ++ "!")
let fromMap2 = Js.Array2.fromMap(someArrayLike, s => s ++ "!")

let isArray1 = [1, 2, 3]->Js.Array2.isArray
let isArray2 = Js.Array2.isArray([1, 2, 3])

let length1 = [1, 2, 3]->Js.Array2.length
let length2 = Js.Array2.length([1, 2, 3])

let fillInPlace1 = [1, 2, 3]->Js.Array2.fillInPlace(0)
let fillInPlace2 = Js.Array2.fillInPlace([1, 2, 3], 0)

let fillFromInPlace1 = [1, 2, 3, 4]->Js.Array2.fillFromInPlace(0, ~from=2)
let fillFromInPlace2 = Js.Array2.fillFromInPlace([1, 2, 3, 4], 0, ~from=2)

let fillRangeInPlace1 = [1, 2, 3, 4]->Js.Array2.fillRangeInPlace(0, ~start=1, ~end_=3)
let fillRangeInPlace2 = Js.Array2.fillRangeInPlace([1, 2, 3, 4], 0, ~start=1, ~end_=3)

let pop1 = [1, 2, 3]->Js.Array2.pop
let pop2 = Js.Array2.pop([1, 2, 3])

let reverseInPlace1 = [1, 2, 3]->Js.Array2.reverseInPlace
let reverseInPlace2 = Js.Array2.reverseInPlace([1, 2, 3])

let concat1 = [1, 2]->Js.Array2.concat([3, 4])
let concat2 = Js.Array2.concat([1, 2], [3, 4])

let concatMany1 = [1, 2]->Js.Array2.concatMany([[3, 4], [5, 6]])
let concatMany2 = Js.Array2.concatMany([1, 2], [[3, 4], [5, 6]])

let includes1 = [1, 2, 3]->Js.Array2.includes(2)
let includes2 = Js.Array2.includes([1, 2, 3], 2)

let indexOf1 = [1, 2, 3]->Js.Array2.indexOf(2)
let indexOf2 = Js.Array2.indexOf([1, 2, 3], 2)

let indexOfFrom1 = [1, 2, 1, 3]->Js.Array2.indexOfFrom(1, ~from=2)
let indexOfFrom2 = Js.Array2.indexOfFrom([1, 2, 1, 3], 1, ~from=2)

let joinWith1 = [1, 2, 3]->Js.Array2.joinWith(",")
let joinWith2 = Js.Array2.joinWith([1, 2, 3], ",")

let lastIndexOf1 = [1, 2, 1, 3]->Js.Array2.lastIndexOf(1)
let lastIndexOf2 = Js.Array2.lastIndexOf([1, 2, 1, 3], 1)

let lastIndexOfFrom1 = [1, 2, 1, 3, 1]->Js.Array2.lastIndexOfFrom(1, ~from=3)
let lastIndexOfFrom2 = Js.Array2.lastIndexOfFrom([1, 2, 1, 3, 1], 1, ~from=3)

let copy1 = [1, 2, 3]->Js.Array2.copy
let copy2 = Js.Array2.copy([1, 2, 3])

let sliceFrom1 = [1, 2, 3, 4]->Js.Array2.sliceFrom(2)
let sliceFrom2 = Js.Array2.sliceFrom([1, 2, 3, 4], 2)

let toString1 = [1, 2, 3]->Js.Array2.toString
let toString2 = Js.Array2.toString([1, 2, 3])

let toLocaleString1 = [1, 2, 3]->Js.Array2.toLocaleString
let toLocaleString2 = Js.Array2.toLocaleString([1, 2, 3])

let every1 = [2, 4, 6]->Js.Array2.every(x => mod(x, 2) == 0)
let every2 = Js.Array2.every([2, 4, 6], x => mod(x, 2) == 0)

let everyi1 = [0, 1, 2]->Js.Array2.everyi((x, i) => x == i)
let everyi2 = Js.Array2.everyi([0, 1, 2], (x, i) => x == i)

let filter1 = [1, 2, 3, 4]->Js.Array2.filter(x => x > 2)
let filter2 = Js.Array2.filter([1, 2, 3, 4], x => x > 2)

let filteri1 = [0, 1, 2, 3]->Js.Array2.filteri((_x, i) => i > 1)
let filteri2 = Js.Array2.filteri([0, 1, 2, 3], (_x, i) => i > 1)

let find1 = [1, 2, 3, 4]->Js.Array2.find(x => x > 2)
let find2 = Js.Array2.find([1, 2, 3, 4], x => x > 2)

let findi1 = [0, 1, 2, 3]->Js.Array2.findi((_x, i) => i > 1)
let findi2 = Js.Array2.findi([0, 1, 2, 3], (_x, i) => i > 1)

let findIndex1 = [1, 2, 3, 4]->Js.Array2.findIndex(x => x > 2)
let findIndex2 = Js.Array2.findIndex([1, 2, 3, 4], x => x > 2)

let findIndexi1 = [0, 1, 2, 3]->Js.Array2.findIndexi((_x, i) => i > 1)
let findIndexi2 = Js.Array2.findIndexi([0, 1, 2, 3], (_x, i) => i > 1)

let forEach1 = [1, 2, 3]->Js.Array2.forEach(x => ignore(x))
let forEach2 = Js.Array2.forEach([1, 2, 3], x => ignore(x))

let forEachi1 = [1, 2, 3]->Js.Array2.forEachi((x, i) => ignore(x + i))
let forEachi2 = Js.Array2.forEachi([1, 2, 3], (x, i) => ignore(x + i))

let map1 = [1, 2, 3]->Js.Array2.map(x => x * 2)
let map2 = Js.Array2.map([1, 2, 3], x => x * 2)

let mapi1 = [1, 2, 3]->Js.Array2.mapi((x, i) => x + i)
let mapi2 = Js.Array2.mapi([1, 2, 3], (x, i) => x + i)

let some1 = [1, 2, 3, 4]->Js.Array2.some(x => x > 3)
let some2 = Js.Array2.some([1, 2, 3, 4], x => x > 3)

let somei1 = [0, 1, 2, 3]->Js.Array2.somei((_x, i) => i > 2)
let somei2 = Js.Array2.somei([0, 1, 2, 3], (_x, i) => i > 2)

let unsafeGet1 = [1, 2, 3]->Js.Array2.unsafe_get(1)
let unsafeGet2 = Js.Array2.unsafe_get([1, 2, 3], 1)

let unsafeSet1 = [1, 2, 3]->Js.Array2.unsafe_set(1, 5)
let unsafeSet2 = Js.Array2.unsafe_set([1, 2, 3], 1, 5)

let copyWithin1 = [1, 2, 3, 4, 5]->Js.Array2.copyWithin(~to_=2)
let copyWithin2 = Js.Array2.copyWithin([1, 2, 3, 4, 5], ~to_=2)

let copyWithinFrom1 = [1, 2, 3, 4, 5]->Js.Array2.copyWithinFrom(~to_=0, ~from=2)
let copyWithinFrom2 = Js.Array2.copyWithinFrom([1, 2, 3, 4, 5], ~to_=0, ~from=2)

let copyWithinFromRange1 =
  [1, 2, 3, 4, 5, 6]->Js.Array2.copyWithinFromRange(~to_=1, ~start=2, ~end_=5)
let copyWithinFromRange2 = Js.Array2.copyWithinFromRange(
  [1, 2, 3, 4, 5, 6],
  ~to_=1,
  ~start=2,
  ~end_=5,
)

let push1 = [1, 2, 3]->Js.Array2.push(4)
let push2 = Js.Array2.push([1, 2, 3], 4)

let pushMany1 = [1, 2, 3]->Js.Array2.pushMany([4, 5])
let pushMany2 = Js.Array2.pushMany([1, 2, 3], [4, 5])

let sortInPlace1 = ["c", "a", "b"]->Js.Array2.sortInPlace
let sortInPlace2 = Js.Array2.sortInPlace(["c", "a", "b"])

let unshift1 = [1, 2, 3]->Js.Array2.unshift(4)
let unshift2 = Js.Array2.unshift([1, 2, 3], 4)

let unshiftMany1 = [1, 2, 3]->Js.Array2.unshiftMany([4, 5])
let unshiftMany2 = Js.Array2.unshiftMany([1, 2, 3], [4, 5])

let reduce1 = [1, 2, 3]->Js.Array2.reduce((acc, x) => acc + x, 0)
let reduce2 = Js.Array2.reduce([1, 2, 3], (acc, x) => acc + x, 0)

let spliceInPlace1 = [1, 2, 3]->Js.Array2.spliceInPlace(~pos=1, ~remove=1, ~add=[4, 5])
let spliceInPlace2 = Js.Array2.spliceInPlace([1, 2, 3], ~pos=1, ~remove=1, ~add=[4, 5])

let removeFromInPlace1 = [1, 2, 3]->Js.Array2.removeFromInPlace(~pos=1)
let removeFromInPlace2 = Js.Array2.removeFromInPlace([1, 2, 3], ~pos=1)

let removeCountInPlace1 = [1, 2, 3]->Js.Array2.removeCountInPlace(~pos=1, ~count=1)
let removeCountInPlace2 = Js.Array2.removeCountInPlace([1, 2, 3], ~pos=1, ~count=1)

let reducei1 = [1, 2, 3]->Js.Array2.reducei((acc, x, i) => acc + x + i, 0)
let reducei2 = Js.Array2.reducei([1, 2, 3], (acc, x, i) => acc + x + i, 0)

let reduceRight1 = [1, 2, 3]->Js.Array2.reduceRight((acc, x) => acc + x, 0)
let reduceRight2 = Js.Array2.reduceRight([1, 2, 3], (acc, x) => acc + x, 0)

let reduceRighti1 = [1, 2, 3]->Js.Array2.reduceRighti((acc, x, i) => acc + x + i, 0)
let reduceRighti2 = Js.Array2.reduceRighti([1, 2, 3], (acc, x, i) => acc + x + i, 0)

let pipeChain =
  [1, 2, 3]
  ->Js.Array2.map(x => x * 2)
  ->Js.Array2.filter(x => x > 2)
  ->Js.Array2.reduce((acc, x) => acc + x, 0)

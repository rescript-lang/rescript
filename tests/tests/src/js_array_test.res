open Mocha
open Test_utils

describe(__MODULE__, () => {
  /* es2015, unable to test because nothing currently implements array_like
  test("from", () => {
    eq(__LOC__,
      [| 0; 1 |],
      [| "a"; "b" |] |. Js.Array2.keys |. Js.Array2.from)
  })
 */

  /* es2015, unable to test because nothing currently implements array_like
  test("fromMap", () => {
    eq(__LOC__,
      [| (-1); 0 |],
      Js.Array2.fromMap
        ([| "a"; "b" |] |. Js.Array2.keys)
        ((fun x -> x - 1) [@bs]))
  })
 */

  /* es2015 */
  test("isArray_array", () => {
    eq(__LOC__, true, []->Js.Array2.isArray)
  })
  test("isArray_int", () => {
    eq(__LOC__, false, 34->Js.Array2.isArray)
  })
  test("length", () => {
    eq(__LOC__, 3, [1, 2, 3]->Js.Array2.length)
  })
  /* es2015 */
  test("copyWithin", () => {
    eq(__LOC__, [1, 2, 3, 1, 2], [1, 2, 3, 4, 5]->Js.Array2.copyWithin(~to_=-2))
  })
  test("copyWithinFrom", () => {
    eq(__LOC__, [4, 5, 3, 4, 5], [1, 2, 3, 4, 5]->Js.Array2.copyWithinFrom(~to_=0, ~from=3))
  })
  test("copyWithinFromRange", () => {
    eq(
      __LOC__,
      [4, 2, 3, 4, 5],
      [1, 2, 3, 4, 5]->Js.Array2.copyWithinFromRange(~to_=0, ~start=3, ~end_=4),
    )
  })
  /* es2015 */
  test("fillInPlace", () => {
    eq(__LOC__, [4, 4, 4], [1, 2, 3]->Js.Array2.fillInPlace(4))
  })
  test("fillFromInPlace", () => {
    eq(__LOC__, [1, 4, 4], [1, 2, 3]->Js.Array2.fillFromInPlace(4, ~from=1))
  })
  test("fillRangeInPlace", () => {
    eq(__LOC__, [1, 4, 3], [1, 2, 3]->Js.Array2.fillRangeInPlace(4, ~start=1, ~end_=2))
  })
  test("pop", () => {
    eq(__LOC__, Some(3), [1, 2, 3]->Js.Array2.pop)
  })
  test("pop - empty array", () => {
    eq(__LOC__, None, []->Js.Array2.pop)
  })
  test("push", () => {
    eq(__LOC__, 4, [1, 2, 3]->Js.Array2.push(4))
  })
  test("pushMany", () => {
    eq(__LOC__, 5, [1, 2, 3]->Js.Array2.pushMany([4, 5]))
  })
  test("reverseInPlace", () => {
    eq(__LOC__, [3, 2, 1], [1, 2, 3]->Js.Array2.reverseInPlace)
  })
  test("shift", () => {
    eq(__LOC__, Some(1), [1, 2, 3]->Js.Array2.shift)
  })
  test("shift - empty array", () => {
    eq(__LOC__, None, []->Js.Array2.shift)
  })
  test("sortInPlace", () => {
    eq(__LOC__, [1, 2, 3], [3, 1, 2]->Js.Array2.sortInPlace)
  })
  test("sortInPlaceWith", () => {
    eq(__LOC__, [3, 2, 1], [3, 1, 2]->Js.Array2.sortInPlaceWith((a, b) => b - a))
  })
  test("spliceInPlace", () => {
    let arr = [1, 2, 3, 4]
    let removed = arr->Js.Array2.spliceInPlace(~pos=2, ~remove=0, ~add=[5])

    eq(__LOC__, ([1, 2, 5, 3, 4], []), (arr, removed))
  })
  test("removeFromInPlace", () => {
    let arr = [1, 2, 3, 4]
    let removed = arr->Js.Array2.removeFromInPlace(~pos=2)

    eq(__LOC__, ([1, 2], [3, 4]), (arr, removed))
  })
  test("removeCountInPlace", () => {
    let arr = [1, 2, 3, 4]
    let removed = arr->Js.Array2.removeCountInPlace(~pos=2, ~count=1)

    eq(__LOC__, ([1, 2, 4], [3]), (arr, removed))
  })
  test("unshift", () => {
    eq(__LOC__, 4, [1, 2, 3]->Js.Array2.unshift(4))
  })
  test("unshiftMany", () => {
    eq(__LOC__, 5, [1, 2, 3]->Js.Array2.unshiftMany([4, 5]))
  })
  test("append", () => {
    eq(__LOC__, [1, 2, 3, 4], [1, 2, 3]->Js.Array2.concat([4]))
  })
  test("concat", () => {
    eq(__LOC__, [1, 2, 3, 4, 5], [1, 2, 3]->Js.Array2.concat([4, 5]))
  })
  test("concatMany", () => {
    eq(__LOC__, [1, 2, 3, 4, 5, 6, 7], [1, 2, 3]->Js.Array2.concatMany([[4, 5], [6, 7]]))
  })
  /* es2016 */
  test("includes", () => {
    eq(__LOC__, true, [1, 2, 3]->Js.Array2.includes(3))
  })
  test("indexOf", () => {
    eq(__LOC__, 1, [1, 2, 3]->Js.Array2.indexOf(2))
  })
  test("indexOfFrom", () => {
    eq(__LOC__, 3, [1, 2, 3, 2]->Js.Array2.indexOfFrom(2, ~from=2))
  })
  test("join", () => {
    eq(__LOC__, "1,2,3", [1, 2, 3]->Js.Array.join)
  })
  test("joinWith", () => {
    eq(__LOC__, "1;2;3", [1, 2, 3]->Js.Array2.joinWith(";"))
  })
  test("lastIndexOf", () => {
    eq(__LOC__, 1, [1, 2, 3]->Js.Array2.lastIndexOf(2))
  })
  test("lastIndexOfFrom", () => {
    eq(__LOC__, 1, [1, 2, 3, 2]->Js.Array2.lastIndexOfFrom(2, ~from=2))
  })
  test("slice", () => {
    eq(__LOC__, [2, 3], [1, 2, 3, 4, 5]->Js.Array2.slice(~start=1, ~end_=3))
  })
  test("copy", () => {
    eq(__LOC__, [1, 2, 3, 4, 5], [1, 2, 3, 4, 5]->Js.Array2.copy)
  })
  test("sliceFrom", () => {
    eq(__LOC__, [3, 4, 5], [1, 2, 3, 4, 5]->Js.Array2.sliceFrom(2))
  })
  test("toString", () => {
    eq(__LOC__, "1,2,3", [1, 2, 3]->Js.Array2.toString)
  })
  test("toLocaleString", () => {
    eq(__LOC__, "1,2,3", [1, 2, 3]->Js.Array2.toLocaleString)
  })
  /* es2015, iterator
  test("entries", () => {
    eq(__LOC__,
      [| (0, "a"); (1, "b"); (2, "c") |],
         [| "a"; "b"; "c" |] |. Js.Array2.entries |. Js.Array2.from)
  })
 */

  test("every", () => {
    eq(__LOC__, true, [1, 2, 3]->Js.Array2.every(n => n > 0))
  })
  test("everyi", () => {
    eq(__LOC__, false, [1, 2, 3]->Js.Array2.everyi((_, i) => i > 0))
  })
  test("filter", () => {
    eq(__LOC__, [2, 4], [1, 2, 3, 4]->Js.Array2.filter(n => mod(n, 2) == 0))
  })
  test("filteri", () => {
    eq(__LOC__, [1, 3], [1, 2, 3, 4]->Js.Array2.filteri((_, i) => mod(i, 2) == 0))
  })
  /* es2015 */
  test("find", () => {
    eq(__LOC__, Some(2), [1, 2, 3, 4]->Js.Array2.find(n => mod(n, 2) == 0))
  })
  test("find - no match", () => {
    eq(__LOC__, None, [1, 2, 3, 4]->Js.Array2.find(n => mod(n, 2) == 5))
  })
  test("findi", () => {
    eq(__LOC__, Some(1), [1, 2, 3, 4]->Js.Array2.findi((_, i) => mod(i, 2) == 0))
  })
  test("findi - no match", () => {
    eq(__LOC__, None, [1, 2, 3, 4]->Js.Array2.findi((_, i) => mod(i, 2) == 5))
  })
  /* es2015 */
  test("findIndex", () => {
    eq(__LOC__, 1, [1, 2, 3, 4]->Js.Array2.findIndex(n => mod(n, 2) == 0))
  })
  test("findIndexi", () => {
    eq(__LOC__, 0, [1, 2, 3, 4]->Js.Array2.findIndexi((_, i) => mod(i, 2) == 0))
  })
  test("forEach", () => {
    let sum = ref(0)
    let _ = [1, 2, 3]->Js.Array2.forEach(n => sum := sum.contents + n)

    eq(__LOC__, 6, sum.contents)
  })
  test("forEachi", () => {
    let sum = ref(0)
    let _ = [1, 2, 3]->Js.Array2.forEachi((_, i) => sum := sum.contents + i)

    eq(__LOC__, 3, sum.contents)
  })
  /* es2015, iterator
  test("keys", () => {
    eq(__LOC__,
      [| 0; 1; 2 |],
         [| "a"; "b"; "c" |] |. Js.Array2.keys |. Js.Array2.from)
  })
 */

  test("map", () => {
    eq(__LOC__, [2, 4, 6, 8], [1, 2, 3, 4]->Js.Array2.map(n => n * 2))
  })
  test("mapi", () => {
    eq(__LOC__, [0, 2, 4, 6], [1, 2, 3, 4]->Js.Array2.mapi((_, i) => i * 2))
  })
  test("reduce", () => {
    eq(__LOC__, -10, [1, 2, 3, 4]->Js.Array2.reduce((acc, n) => acc - n, 0))
  })
  test("reducei", () => {
    eq(__LOC__, -6, [1, 2, 3, 4]->Js.Array2.reducei((acc, _, i) => acc - i, 0))
  })
  test("reduceRight", () => {
    eq(__LOC__, -10, [1, 2, 3, 4]->Js.Array2.reduceRight((acc, n) => acc - n, 0))
  })
  test("reduceRighti", () => {
    eq(__LOC__, -6, [1, 2, 3, 4]->Js.Array2.reduceRighti((acc, _, i) => acc - i, 0))
  })
  test("some", () => {
    eq(__LOC__, false, [1, 2, 3, 4]->Js.Array2.some(n => n <= 0))
  })
  test("somei", () => {
    eq(__LOC__, true, [1, 2, 3, 4]->Js.Array2.somei((_, i) => i <= 0))
  })

  /* es2015, iterator
  test("values", () => {
    eq(__LOC__,
      [| "a"; "b"; "c" |],
         [| "a"; "b"; "c" |] |. Js.Array2.values |. Js.Array2.from)
  })
 */
})

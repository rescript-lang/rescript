open Mocha
open Test_utils

module I = Array_data_util
module R = Belt.Range
module A = Belt.Array
module S = Belt.SortArray
module SI = Belt.SortArray.Int

let cmp = (x, y) => x - y

let unions = (xs, ys) => {
  let (lenX, lenY) = (A.length(xs), A.length(ys))
  let o = A.makeUninitializedUnsafe(lenX + lenY)
  let v = S.union(xs, 0, lenX, ys, 0, lenY, o, 0, cmp)
  A.truncateToLengthUnsafe(o, v)
  o
}

let inters = (xs, ys) => {
  let (lenX, lenY) = (A.length(xs), A.length(ys))
  let o = A.makeUninitializedUnsafe(lenX)
  let v = S.intersect(xs, 0, lenX, ys, 0, lenY, o, 0, cmp)
  A.truncateToLengthUnsafe(o, v)
  o
}

let diffs = (xs, ys) => {
  let (lenX, lenY) = (A.length(xs), A.length(ys))
  let o = A.makeUninitializedUnsafe(lenX)
  let v = S.diff(xs, 0, lenX, ys, 0, lenY, o, 0, cmp)
  A.truncateToLengthUnsafe(o, v)
  o
}

describe(__MODULE__, () => {
  test("union", () => {
    eq(__LOC__, unions(I.range(1, 10), I.range(3, 13)), I.range(1, 13))
    eq(__LOC__, unions(I.range(1, 10), I.range(9, 13)), I.range(1, 13))
    eq(__LOC__, unions(I.range(8, 10), I.range(9, 13)), I.range(8, 13))
    eq(__LOC__, unions(I.range(0, 2), I.range(4, 7)), [0, 1, 2, 4, 5, 6, 7])
  })

  test("intersect", () => {
    eq(__LOC__, inters(I.range(1, 10), I.range(3, 13)), I.range(3, 10))
    eq(__LOC__, inters(I.range(1, 10), I.range(9, 13)), I.range(9, 10))
    eq(__LOC__, inters(I.range(8, 10), I.range(9, 13)), I.range(9, 10))
    eq(__LOC__, inters(I.range(0, 2), I.range(4, 7)), [])
  })

  test("diff", () => {
    eq(__LOC__, diffs(I.range(1, 10), I.range(3, 13)), I.range(1, 2))
    eq(__LOC__, diffs(I.range(1, 10), I.range(9, 13)), I.range(1, 8))
    eq(__LOC__, diffs(I.range(8, 10), I.range(9, 13)), I.range(8, 8))
    eq(__LOC__, diffs(I.range(0, 2), I.range(4, 7)), [0, 1, 2])
  })

  test("isSorted", () => {
    ok(__LOC__, S.isSorted([], cmp))
    ok(__LOC__, S.isSorted([0], cmp))
    ok(__LOC__, S.isSorted([0, 1], cmp))
    ok(__LOC__, !S.isSorted([1, 0], cmp))
  })

  test("stableSortInPlaceBy", () => {
    ok(
      __LOC__,
      R.every(
        0,
        200,
        i => {
          let v = I.randomRange(0, i)
          S.stableSortInPlaceBy(v, cmp)
          S.isSorted(v, cmp)
        },
      ),
    )
  })

  test("stableSortInPlaceBy 2", () => {
    let u = I.randomRange(0, 1_000_000)
    let u1 = A.copy(u)

    S.stableSortInPlaceBy(u, cmp)
    ok(__LOC__, S.isSorted(u, cmp))
    SI.stableSortInPlace(u1)
    ok(__LOC__, S.isSorted(u1, cmp))
  })

  test("stableSortInPlaceBy 3", () => {
    let u = [(1, "a"), (1, "b"), (2, "a")]
    eq(__LOC__, S.stableSortBy(u, ((a, _), (b, _)) => a - b), [(1, "a"), (1, "b"), (2, "a")])
    let u = [(1, "b"), (1, "a"), (1, "b"), (2, "a")]
    eq(
      __LOC__,
      S.stableSortBy(u, ((a, _), (b, _)) => a - b),
      [(1, "b"), (1, "a"), (1, "b"), (2, "a")],
    )
    let u = [(1, "c"), (1, "b"), (1, "a"), (1, "b"), (1, "c"), (2, "a")]
    eq(
      __LOC__,
      S.stableSortBy(u, ((a, _), (b, _)) => a - b),
      [(1, "c"), (1, "b"), (1, "a"), (1, "b"), (1, "c"), (2, "a")],
    )
  })

  test("binarySearchBy", () => {
    eq(__LOC__, lnot(S.binarySearchBy([1, 3, 5, 7], 4, cmp)), 2)
    eq(__LOC__, S.binarySearchBy([1, 2, 3, 4, 33, 35, 36], 33, cmp), 4)
    eq(__LOC__, S.binarySearchBy([1, 2, 3, 4, 33, 35, 36], 1, cmp), 0)
    eq(__LOC__, S.binarySearchBy([1, 2, 3, 4, 33, 35, 36], 2, cmp), 1)
    eq(__LOC__, S.binarySearchBy([1, 2, 3, 4, 33, 35, 36], 3, cmp), 2)
    eq(__LOC__, S.binarySearchBy([1, 2, 3, 4, 33, 35, 36], 4, cmp), 3)
    let aa = I.range(0, 1000)
    ok(__LOC__, R.every(0, 1000, i => S.binarySearchBy(aa, i, cmp) == i))
    /* 0, 2, 4, ... 4000 */
    let cc = A.map(I.range(0, 2000), x => x * 2)
    eq(__LOC__, lnot(S.binarySearchBy(cc, 5000, cmp)), 2001)
    eq(__LOC__, lnot(S.binarySearchBy(cc, -1, cmp)), 0)
    eq(__LOC__, S.binarySearchBy(cc, 0, cmp), 0)

    eq(__LOC__, lnot(S.binarySearchBy(cc, 1, cmp)), 1)
    ok(
      __LOC__,
      R.every(0, 1999, i => lnot(S.binarySearchBy(cc, 2 * i + 1, cmp)) == i + 1),
      /* 1, 3, 5, ... , 3999 */
    )
  })

  test("strictlySortedLength", () => {
    let lt = (x: int, y) => x < y

    eq(__LOC__, S.strictlySortedLength([], lt), 0)
    eq(__LOC__, S.strictlySortedLength([1], lt), 1)
    eq(__LOC__, S.strictlySortedLength([1, 1], lt), 1)
    eq(__LOC__, S.strictlySortedLength([1, 1, 2], lt), 1)
    eq(__LOC__, S.strictlySortedLength([1, 2], lt), 2)
    eq(__LOC__, S.strictlySortedLength([1, 2, 3, 4, 3], lt), 4)
    eq(__LOC__, S.strictlySortedLength([4, 4, 3, 2, 1], lt), 1)
    eq(__LOC__, S.strictlySortedLength([4, 3, 2, 1], lt), -4)
    eq(__LOC__, S.strictlySortedLength([4, 3, 2, 1, 0], lt), -5)
  })
})

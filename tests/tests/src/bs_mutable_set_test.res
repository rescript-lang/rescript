open Mocha
open Test_utils

module N = Belt.MutableSet.Int

module I = Array_data_util
module R = Belt.Range
module A = Belt.Array
module L = Belt.List
let empty = N.make
let fromArray = N.fromArray

describe(__MODULE__, () => {
  test("mutable set basic operations", () => {
    let u = fromArray(I.range(0, 30))
    ok(__LOC__, N.removeCheck(u, 0))
    ok(__LOC__, !N.removeCheck(u, 0))
    ok(__LOC__, N.removeCheck(u, 30))
    ok(__LOC__, N.removeCheck(u, 20))
    eq(__LOC__, N.size(u), 28)
    let r = I.randomRange(0, 30)
    ok(__LOC__, Js.eqUndefined(29, N.maxUndefined(u)))
    ok(__LOC__, Js.eqUndefined(1, N.minUndefined(u)))
    N.add(u, 3)
    for i in 0 to A.length(r) - 1 {
      N.remove(u, A.getUnsafe(r, i))
    }
    ok(__LOC__, N.isEmpty(u))
    N.add(u, 0)
    N.add(u, 1)
    N.add(u, 2)
    N.add(u, 0)
    eq(__LOC__, N.size(u), 3)
    ok(__LOC__, !N.isEmpty(u))
    for i in 0 to 3 {
      N.remove(u, i)
    }
    ok(__LOC__, N.isEmpty(u))
    N.mergeMany(u, I.randomRange(0, 20000))
    N.mergeMany(u, I.randomRange(0, 200))
    eq(__LOC__, N.size(u), 20001)
    N.removeMany(u, I.randomRange(0, 200))
    eq(__LOC__, N.size(u), 19800)
    N.removeMany(u, I.randomRange(0, 1000))
    eq(__LOC__, N.size(u), 19000)
    N.removeMany(u, I.randomRange(0, 1000))
    eq(__LOC__, N.size(u), 19000)
    N.removeMany(u, I.randomRange(1000, 10000))
    eq(__LOC__, N.size(u), 10000)
    N.removeMany(u, I.randomRange(10000, 20000 - 1))
    eq(__LOC__, N.size(u), 1)
    ok(__LOC__, N.has(u, 20000))
    N.removeMany(u, I.randomRange(10_000, 30_000))
    ok(__LOC__, N.isEmpty(u))
  })

  test("mutable set add/remove operations", () => {
    let v = fromArray(I.randomRange(1_000, 2_000))
    let bs = A.map(I.randomRange(500, 1499), x => N.removeCheck(v, x))
    let indeedRemoved = A.reduce(
      bs,
      0,
      (acc, x) =>
        if x {
          acc + 1
        } else {
          acc
        },
    )
    eq(__LOC__, indeedRemoved, 500)
    eq(__LOC__, N.size(v), 501)
    let cs = A.map(I.randomRange(500, 2_000), x => N.addCheck(v, x))
    let indeedAded = A.reduce(
      cs,
      0,
      (acc, x) =>
        if x {
          acc + 1
        } else {
          acc
        },
    )
    eq(__LOC__, indeedAded, 1000)
    eq(__LOC__, N.size(v), 1_501)
    ok(__LOC__, N.isEmpty(empty()))
    eq(__LOC__, N.minimum(v), Some(500))
    eq(__LOC__, N.maximum(v), Some(2000))
    eq(__LOC__, N.minUndefined(v), Js.Undefined.return(500))
    eq(__LOC__, N.maxUndefined(v), Js.Undefined.return(2000))
    eq(__LOC__, N.reduce(v, 0, (x, y) => x + y), (500 + 2000) / 2 * 1501)
    ok(__LOC__, L.eq(N.toList(v), L.makeBy(1_501, i => i + 500), (x, y) => x == y))
    eq(__LOC__, N.toArray(v), I.range(500, 2000))
    N.checkInvariantInternal(v)
    eq(__LOC__, N.get(v, 3), None)
    eq(__LOC__, N.get(v, 1_200), Some(1_200))
    let ((aa, bb), pres) = N.split(v, 1000)
    ok(__LOC__, pres)
    ok(__LOC__, A.eq(N.toArray(aa), I.range(500, 999), (x, y) => x == y))
    ok(__LOC__, A.eq(N.toArray(bb), I.range(1_001, 2_000), (x, y) => x == y))
    ok(__LOC__, N.subset(aa, v))
    ok(__LOC__, N.subset(bb, v))
    ok(__LOC__, N.isEmpty(N.intersect(aa, bb)))
    let c = N.removeCheck(v, 1_000)
    ok(__LOC__, c)
    let ((aa, bb), pres) = N.split(v, 1_000)
    ok(__LOC__, !pres)
    ok(__LOC__, A.eq(N.toArray(aa), I.range(500, 999), (x, y) => x == y))
    ok(__LOC__, A.eq(N.toArray(bb), I.range(1_001, 2_000), (x, y) => x == y))
    ok(__LOC__, N.subset(aa, v))
    ok(__LOC__, N.subset(bb, v))
    ok(__LOC__, N.isEmpty(N.intersect(aa, bb)))
  })

  test("mutable set union operations", () => {
    let setUnion = N.union
    let f = fromArray
    let eq = N.eq
    let aa = f(I.randomRange(0, 100))
    let bb = f(I.randomRange(40, 120))
    let cc = setUnion(aa, bb)
    ok(__LOC__, eq(cc, f(I.randomRange(0, 120))))

    ok(
      __LOC__,
      N.eq(N.union(f(I.randomRange(0, 20)), f(I.randomRange(21, 40))), f(I.randomRange(0, 40))),
    )
    let dd = N.intersect(aa, bb)
    ok(__LOC__, eq(dd, f(I.randomRange(40, 100))))
    ok(__LOC__, eq(N.intersect(f(I.randomRange(0, 20)), f(I.randomRange(21, 40))), empty()))
    ok(__LOC__, eq(N.intersect(f(I.randomRange(21, 40)), f(I.randomRange(0, 20))), empty()))
    ok(__LOC__, eq(N.intersect(f([1, 3, 4, 5, 7, 9]), f([2, 4, 5, 6, 8, 10])), f([4, 5])))
    ok(__LOC__, eq(N.diff(aa, bb), f(I.randomRange(0, 39))))
    ok(__LOC__, eq(N.diff(bb, aa), f(I.randomRange(101, 120))))
    ok(
      __LOC__,
      eq(N.diff(f(I.randomRange(21, 40)), f(I.randomRange(0, 20))), f(I.randomRange(21, 40))),
    )
    ok(
      __LOC__,
      eq(N.diff(f(I.randomRange(0, 20)), f(I.randomRange(21, 40))), f(I.randomRange(0, 20))),
    )

    ok(
      __LOC__,
      eq(N.diff(f(I.randomRange(0, 20)), f(I.randomRange(0, 40))), f(I.randomRange(0, -1))),
    )
  })

  test("mutable set keep/partition operations", () => {
    let a0 = fromArray(I.randomRange(0, 1000))
    let (a1, a2) = (N.keep(a0, x => mod(x, 2) == 0), N.keep(a0, x => mod(x, 2) != 0))
    let (a3, a4) = N.partition(a0, x => mod(x, 2) == 0)
    ok(__LOC__, N.eq(a1, a3))
    ok(__LOC__, N.eq(a2, a4))
    L.forEach(list{a0, a1, a2, a3, a4}, x => N.checkInvariantInternal(x))
  })

  test("mutable set large scale operations", () => {
    let v = N.make()
    for i in 0 to 1_00_000 {
      N.add(v, i)
    }
    N.checkInvariantInternal(v)
    ok(__LOC__, R.every(0, 1_00_000, i => N.has(v, i)))
    eq(__LOC__, N.size(v), 1_00_001)
  })

  test("mutable set merge operations", () => {
    let u = A.concat(I.randomRange(30, 100), I.randomRange(40, 120))
    let v = N.make()
    N.mergeMany(v, u)
    eq(__LOC__, N.size(v), 91)
    eq(__LOC__, N.toArray(v), I.range(30, 120))
  })

  test("mutable set remove many operations", () => {
    let u = A.concat(I.randomRange(0, 100_000), I.randomRange(0, 100))
    let v = N.fromArray(u)
    eq(__LOC__, N.size(v), 100_001)
    let u = I.randomRange(50_000, 80_000)

    for i in 0 to A.length(u) - 1 {
      N.remove(v, i)
    }

    eq(__LOC__, N.size(v), 70_000)
    let count = 100_000
    let vv = I.randomRange(0, count)
    for i in 0 to A.length(vv) - 1 {
      N.remove(v, vv->Array.getUnsafe(i))
    }
    eq(__LOC__, N.size(v), 0)
    ok(__LOC__, N.isEmpty(v))
  })

  test("mutable set min/max operations", () => {
    let v = N.fromArray(A.makeBy(30, i => i))
    N.remove(v, 30)
    N.remove(v, 29)
    ok(__LOC__, Js.eqUndefined(28, N.maxUndefined(v)))
    N.remove(v, 0)
    ok(__LOC__, Js.eqUndefined(1, N.minUndefined(v)))
    eq(__LOC__, N.size(v), 28)
    let vv = I.randomRange(1, 28)
    for i in 0 to A.length(vv) - 1 {
      N.remove(v, vv->Array.getUnsafe(i))
    }
    eq(__LOC__, N.size(v), 0)
  })

  test("mutable set fromSortedArrayUnsafe", () => {
    let id = (loc, x) => {
      let u = N.fromSortedArrayUnsafe(x)
      N.checkInvariantInternal(u)
      ok(loc, A.every2(N.toArray(u), x, (x, y) => x == y))
    }

    id(__LOC__, [])
    id(__LOC__, [0])
    id(__LOC__, [0, 1])
    id(__LOC__, [0, 1, 2])
    id(__LOC__, [0, 1, 2, 3])
    id(__LOC__, [0, 1, 2, 3, 4])
    id(__LOC__, [0, 1, 2, 3, 4, 5])
    id(__LOC__, [0, 1, 2, 3, 4, 6])
    id(__LOC__, [0, 1, 2, 3, 4, 6, 7])
    id(__LOC__, [0, 1, 2, 3, 4, 6, 7, 8])
    id(__LOC__, [0, 1, 2, 3, 4, 6, 7, 8, 9])
    id(__LOC__, I.range(0, 1000))
  })

  test("mutable set keep/partition with mod 8", () => {
    let v = N.fromArray(I.randomRange(0, 1000))
    let copyV = N.keep(v, x => mod(x, 8) == 0)
    let (aa, bb) = N.partition(v, x => mod(x, 8) == 0)
    let cc = N.keep(v, x => mod(x, 8) != 0)
    for i in 0 to 200 {
      N.remove(v, i)
    }
    eq(__LOC__, N.size(copyV), 126)
    eq(__LOC__, N.toArray(copyV), A.makeBy(126, i => i * 8))
    eq(__LOC__, N.size(v), 800)
    ok(__LOC__, N.eq(copyV, aa))
    ok(__LOC__, N.eq(cc, bb))
  })

  test("mutable set split operations", () => {
    let v = N.fromArray(I.randomRange(0, 1000))
    let ((aa, bb), _) = N.split(v, 400)
    ok(__LOC__, N.eq(aa, N.fromArray(I.randomRange(0, 399))))
    ok(__LOC__, N.eq(bb, N.fromArray(I.randomRange(401, 1000))))
    let d = N.fromArray(A.map(I.randomRange(0, 1000), x => x * 2))
    let ((cc, dd), _) = N.split(d, 1001)
    ok(__LOC__, N.eq(cc, N.fromArray(A.makeBy(501, x => x * 2))))
    ok(__LOC__, N.eq(dd, N.fromArray(A.makeBy(500, x => 1002 + x * 2))))
  })

  test("mutable set final union operations", () => {
    let setUnion2 = N.union
    let f = N.fromArray
    let eq = N.eq
    let aa = f(I.randomRange(0, 100))
    let bb = f(I.randomRange(40, 120))
    let cc = setUnion2(aa, bb)
    ok(__LOC__, eq(cc, f(I.randomRange(0, 120))))

    ok(
      __LOC__,
      N.eq(N.union(f(I.randomRange(0, 20)), f(I.randomRange(21, 40))), f(I.randomRange(0, 40))),
    )
    let dd = N.intersect(aa, bb)
    ok(__LOC__, eq(dd, f(I.randomRange(40, 100))))
    ok(__LOC__, eq(N.intersect(f(I.randomRange(0, 20)), f(I.randomRange(21, 40))), N.make()))
    ok(__LOC__, eq(N.intersect(f(I.randomRange(21, 40)), f(I.randomRange(0, 20))), N.make()))
    ok(__LOC__, eq(N.intersect(f([1, 3, 4, 5, 7, 9]), f([2, 4, 5, 6, 8, 10])), f([4, 5])))
    ok(__LOC__, eq(N.diff(aa, bb), f(I.randomRange(0, 39))))
    ok(__LOC__, eq(N.diff(bb, aa), f(I.randomRange(101, 120))))
    ok(
      __LOC__,
      eq(N.diff(f(I.randomRange(21, 40)), f(I.randomRange(0, 20))), f(I.randomRange(21, 40))),
    )
    ok(
      __LOC__,
      eq(N.diff(f(I.randomRange(0, 20)), f(I.randomRange(21, 40))), f(I.randomRange(0, 20))),
    )

    ok(
      __LOC__,
      eq(N.diff(f(I.randomRange(0, 20)), f(I.randomRange(0, 40))), f(I.randomRange(0, -1))),
    )
  })
})

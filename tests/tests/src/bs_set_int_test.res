@@warning("-44")
let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~suites, ~test_id, loc, x, y)

let b = (loc, v) => Mt.bool_suites(~suites, ~test_id, loc, v)

module N = Belt.Set.Int
module I = Array_data_util
module L = Belt.List
module A = Belt.Array
let \"=~" = (s, i) => {
  open N
  eq(fromArray(i), s)
}
let \"=*" = (a, b) => {
  open N
  eq(fromArray(a), fromArray(b))
}
let ofA = N.fromArray

let () = b(__LOC__, \"=*"([1, 2, 3], [3, 2, 1]))
let u = {
  open N
  intersect(ofA([1, 2, 3]), ofA([3, 4, 5]))
}

let () = b(__LOC__, \"=~"(u, [3]))

/* inclusive */
let range = (i, j) => A.init(j - i + 1, k => k + i)

let revRange = (i, j) => L.toArray(L.reverse(L.fromArray(A.init(j - i + 1, k => k + i))))

let () = {
  let v = ofA(A.concat(range(100, 1000), revRange(400, 1500)))
  b(__LOC__, \"=~"(v, range(100, 1500)))
  let (l, r) = N.partition(v, x => mod(x, 3) == 0)
  let (nl, nr) = {
    let (l, r) = (ref(N.empty), ref(N.empty))
    for i in 100 to 1500 {
      if mod(i, 3) == 0 {
        l := N.add(l.contents, i)
      } else {
        r := N.add(r.contents, i)
      }
    }
    (l.contents, r.contents)
  }
  b(__LOC__, N.eq(l, nl))
  b(__LOC__, N.eq(r, nr))
}

let () = b(
  __LOC__,
  \"=~"(
    {
      open N
      intersect(ofA(range(1, 100)), ofA(range(50, 200)))
    },
    range(50, 100),
  ),
)

let () = b(
  __LOC__,
  \"=~"(
    {
      open N
      union(ofA(range(1, 100)), ofA(range(50, 200)))
    },
    range(1, 200),
  ),
)

let () = b(
  __LOC__,
  \"=~"(
    {
      open N
      diff(ofA(range(1, 100)), ofA(range(50, 200)))
    },
    range(1, 49),
  ),
)

let () = b(
  __LOC__,
  \"=~"(
    {
      open N
      intersect(ofA(revRange(1, 100)), ofA(revRange(50, 200)))
    },
    revRange(50, 100),
  ),
)

let () = b(
  __LOC__,
  \"=~"(
    {
      open N
      union(ofA(revRange(1, 100)), ofA(revRange(50, 200)))
    },
    revRange(1, 200),
  ),
)

let () = b(
  __LOC__,
  \"=~"(
    {
      open N
      diff(ofA(revRange(1, 100)), ofA(revRange(50, 200)))
    },
    revRange(1, 49),
  ),
)

let () = {
  let ss = [1, 222, 3, 4, 2, 0, 33, -1]
  let v = ofA([1, 222, 3, 4, 2, 0, 33, -1])
  let (minv, maxv) = (N.minUndefined(v), N.maxUndefined(v))
  let approx = (loc, x: int, y) => b(loc, Js.eqUndefined(x, y))
  eq(__LOC__, N.reduce(v, 0, (x, y) => x + y), A.reduce(ss, 0, \"+"))
  approx(__LOC__, -1, minv)
  approx(__LOC__, 222, maxv)
  let v = N.remove(v, 3)
  let (minv, maxv) = (N.minimum(v), N.maximum(v))
  eq(__LOC__, minv, Some(-1))
  eq(__LOC__, maxv, Some(222))
  let v = N.remove(v, 222)
  let (minv, maxv) = (N.minimum(v), N.maximum(v))
  eq(__LOC__, minv, Some(-1))
  eq(__LOC__, maxv, Some(33))
  let v = N.remove(v, -1)
  let (minv, maxv) = (N.minimum(v), N.maximum(v))
  eq(__LOC__, minv, Some(0))
  eq(__LOC__, maxv, Some(33))
  let v = N.remove(v, 0)
  let v = N.remove(v, 33)
  let v = N.remove(v, 2)
  let v = N.remove(v, 3)
  let v = N.remove(v, 4)
  let v = N.remove(v, 1)
  b(__LOC__, N.isEmpty(v))
}

let () = {
  let count = 1_000_000
  let v = A.makeByAndShuffle(count, i => i)
  let u = N.fromArray(v)
  N.checkInvariantInternal(u)
  let firstHalf = A.slice(v, ~offset=0, ~len=2_000)
  let xx = A.reduce(firstHalf, u, N.remove)
  N.checkInvariantInternal(u)
  b(
    __LOC__,
    {
      open N
      eq(union(fromArray(firstHalf), xx), u)
    },
  )
}

let () = {
  let aa = N.fromArray(I.randomRange(0, 100))
  let bb = N.fromArray(I.randomRange(0, 200))
  let cc = N.fromArray(I.randomRange(120, 200))
  let dd = N.union(aa, cc)
  b(__LOC__, N.subset(aa, bb))
  b(__LOC__, N.subset(dd, bb))
  b(__LOC__, N.subset(N.add(dd, 200), bb))
  b(__LOC__, N.add(dd, 200) === dd)
  b(__LOC__, N.add(dd, 0) === dd)
  b(__LOC__, !N.subset(N.add(dd, 201), bb))
}

let () = {
  let aa = N.fromArray(I.randomRange(0, 100))
  let bb = N.fromArray(I.randomRange(0, 100))
  let cc = N.add(bb, 101)
  let dd = N.remove(bb, 99)
  let ee = N.add(dd, 101)
  b(__LOC__, N.eq(aa, bb))
  b(__LOC__, !N.eq(aa, cc))
  b(__LOC__, !N.eq(dd, cc))
  b(__LOC__, !N.eq(bb, ee))
}

let () = {
  let a0 = N.empty
  let a1 = N.mergeMany(a0, I.randomRange(0, 100))
  let a2 = N.removeMany(a1, I.randomRange(40, 100))
  let a3 = N.fromArray(I.randomRange(0, 39))
  let ((a4, a5), pres) = N.split(a1, 40)
  b(__LOC__, N.eq(a1, N.fromArray(I.randomRange(0, 100))))
  b(__LOC__, N.eq(a2, a3))
  b(__LOC__, pres)
  b(__LOC__, N.eq(a3, a4))
  let a6 = N.remove(N.removeMany(a1, I.randomRange(0, 39)), 40)
  b(__LOC__, N.eq(a5, a6))
  let a7 = N.remove(a1, 40)
  let ((a8, a9), pres2) = N.split(a7, 40)
  b(__LOC__, !pres2)
  b(__LOC__, N.eq(a4, a8))
  b(__LOC__, N.eq(a5, a9))
  let a10 = N.removeMany(a9, I.randomRange(42, 2000))
  eq(__LOC__, N.size(a10), 1)
  let a11 = N.removeMany(a9, I.randomRange(0, 2000))
  b(__LOC__, N.isEmpty(a11))
}

let () = {
  let ((aa, bb), pres) = N.split(N.empty, 0)
  b(__LOC__, N.isEmpty(aa))
  b(__LOC__, N.isEmpty(bb))
  b(__LOC__, !pres)
}

let () = {
  let v = N.fromArray(I.randomRange(0, 2_000))
  let v0 = N.fromArray(I.randomRange(0, 2_000))
  let v1 = N.fromArray(I.randomRange(1, 2_001))
  let v2 = N.fromArray(I.randomRange(3, 2_002))
  let v3 = N.removeMany(v2, [2_002, 2_001])
  let us = A.map(I.randomRange(1_000, 3_000), x => N.has(v, x))
  let counted = A.reduce(us, 0, (acc, x) =>
    if x {
      acc + 1
    } else {
      acc
    }
  )
  eq(__LOC__, counted, 1_001)
  b(__LOC__, N.eq(v, v0))
  b(__LOC__, N.cmp(v, v0) == 0)
  b(__LOC__, N.cmp(v, v1) < 0)
  b(__LOC__, N.cmp(v, v2) > 0)
  b(__LOC__, N.subset(v3, v0))
  b(__LOC__, !N.subset(v1, v0))
  eq(__LOC__, N.get(v, 30), Some(30))
  eq(__LOC__, N.get(v, 3_000), None)
}
Mt.from_pair_suites(__MODULE__, suites.contents)

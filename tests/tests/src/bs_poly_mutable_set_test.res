let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)
let b = (loc, x) => Mt.bool_suites(~test_id, ~suites, loc, x)

module N = Belt.MutableSet
module I = Array_data_util
module A = Belt.Array
module IntCmp = unpack(Belt.Id.comparable(~cmp=(x: int, y) => compare(x, y)))
module L = Belt.List
let fromArray = N.fromArray(~id=module(IntCmp), ...)
let empty = () => N.make(~id=module(IntCmp))

let () = {
  let u = fromArray(I.range(0, 30))
  b(__LOC__, N.removeCheck(u, 0))
  b(__LOC__, !N.removeCheck(u, 0))
  b(__LOC__, N.removeCheck(u, 30))
  b(__LOC__, N.removeCheck(u, 20))
  eq(__LOC__, N.size(u), 28)
  let r = I.randomRange(0, 30)
  b(__LOC__, Js.eqUndefined(29, N.maxUndefined(u)))
  b(__LOC__, Js.eqUndefined(1, N.minUndefined(u)))
  N.add(u, 3)
  for i in 0 to A.length(r) - 1 {
    N.remove(u, A.getUnsafe(r, i))
  }
  b(__LOC__, N.isEmpty(u))
  N.add(u, 0)
  N.add(u, 1)
  N.add(u, 2)
  N.add(u, 0)
  eq(__LOC__, N.size(u), 3)
  b(__LOC__, !N.isEmpty(u))
  for i in 0 to 3 {
    N.remove(u, i)
  }
  b(__LOC__, N.isEmpty(u))
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
  b(__LOC__, N.has(u, 20000))
  N.removeMany(u, I.randomRange(10_000, 30_000))
  b(__LOC__, N.isEmpty(u))
}

let () = {
  let v = fromArray(I.randomRange(1_000, 2_000))
  let bs = A.map(I.randomRange(500, 1499), x => N.removeCheck(v, x))
  let indeedRemoved = A.reduce(bs, 0, (acc, x) =>
    if x {
      acc + 1
    } else {
      acc
    }
  )
  eq(__LOC__, indeedRemoved, 500)
  eq(__LOC__, N.size(v), 501)
  let cs = A.map(I.randomRange(500, 2_000), x => N.addCheck(v, x))
  let indeedAded = A.reduce(cs, 0, (acc, x) =>
    if x {
      acc + 1
    } else {
      acc
    }
  )
  eq(__LOC__, indeedAded, 1000)
  eq(__LOC__, N.size(v), 1_501)
  b(__LOC__, N.isEmpty(empty()))
  eq(__LOC__, N.minimum(v), Some(500))
  eq(__LOC__, N.maximum(v), Some(2000))
  eq(__LOC__, N.minUndefined(v), Js.Undefined.return(500))
  eq(__LOC__, N.maxUndefined(v), Js.Undefined.return(2000))
  eq(__LOC__, N.reduce(v, 0, (x, y) => x + y), (500 + 2000) / 2 * 1501)
  b(__LOC__, L.eq(N.toList(v), L.makeBy(1_501, i => i + 500), (x, y) => x == y))
  eq(__LOC__, N.toArray(v), I.range(500, 2000))
  N.checkInvariantInternal(v)
  eq(__LOC__, N.get(v, 3), None)
  eq(__LOC__, N.get(v, 1_200), Some(1_200))
  let ((aa, bb), pres) = N.split(v, 1000)
  b(__LOC__, pres)
  b(__LOC__, A.eq(N.toArray(aa), I.range(500, 999), \"=="))
  b(__LOC__, A.eq(N.toArray(bb), I.range(1_001, 2_000), \"=="))
  b(__LOC__, N.subset(aa, v))
  b(__LOC__, N.subset(bb, v))
  b(__LOC__, N.isEmpty(N.intersect(aa, bb)))
  let c = N.removeCheck(v, 1_000)
  b(__LOC__, c)
  let ((aa, bb), pres) = N.split(v, 1_000)
  b(__LOC__, !pres)
  b(__LOC__, A.eq(N.toArray(aa), I.range(500, 999), \"=="))
  b(__LOC__, A.eq(N.toArray(bb), I.range(1_001, 2_000), \"=="))
  b(__LOC__, N.subset(aa, v))
  b(__LOC__, N.subset(bb, v))
  b(__LOC__, N.isEmpty(N.intersect(aa, bb)))
}

let \"++" = N.union
let f = fromArray
let \"=~" = N.eq
let () = {
  let aa = f(I.randomRange(0, 100))
  let bb = f(I.randomRange(40, 120))
  let cc = aa ++ bb
  b(__LOC__, \"=~"(cc, f(I.randomRange(0, 120))))

  b(
    __LOC__,
    N.eq(N.union(f(I.randomRange(0, 20)), f(I.randomRange(21, 40))), f(I.randomRange(0, 40))),
  )
  let dd = N.intersect(aa, bb)
  b(__LOC__, \"=~"(dd, f(I.randomRange(40, 100))))
  b(__LOC__, \"=~"(N.intersect(f(I.randomRange(0, 20)), f(I.randomRange(21, 40))), empty()))
  b(__LOC__, \"=~"(N.intersect(f(I.randomRange(21, 40)), f(I.randomRange(0, 20))), empty()))
  b(__LOC__, \"=~"(N.intersect(f([1, 3, 4, 5, 7, 9]), f([2, 4, 5, 6, 8, 10])), f([4, 5])))
  b(__LOC__, \"=~"(N.diff(aa, bb), f(I.randomRange(0, 39))))
  b(__LOC__, \"=~"(N.diff(bb, aa), f(I.randomRange(101, 120))))
  b(
    __LOC__,
    \"=~"(N.diff(f(I.randomRange(21, 40)), f(I.randomRange(0, 20))), f(I.randomRange(21, 40))),
  )
  b(
    __LOC__,
    \"=~"(N.diff(f(I.randomRange(0, 20)), f(I.randomRange(21, 40))), f(I.randomRange(0, 20))),
  )

  b(
    __LOC__,
    \"=~"(N.diff(f(I.randomRange(0, 20)), f(I.randomRange(0, 40))), f(I.randomRange(0, -1))),
  )
}

let () = {
  let a0 = fromArray(I.randomRange(0, 1000))
  let (a1, a2) = (N.keep(a0, x => mod(x, 2) == 0), N.keep(a0, x => mod(x, 2) != 0))
  let (a3, a4) = N.partition(a0, x => mod(x, 2) == 0)
  b(__LOC__, N.eq(a1, a3))
  b(__LOC__, N.eq(a2, a4))
  L.forEach(list{a0, a1, a2, a3, a4}, x => N.checkInvariantInternal(x))
}

Mt.from_pair_suites(__MODULE__, suites.contents)

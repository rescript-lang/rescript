open Mocha
open Test_utils

module N = Belt.List
module A = Belt.Array

let sum = xs => {
  let v = ref(0)
  N.forEach(xs, x => v := v.contents + x)
  v.contents
}

let sum2 = (xs, ys) => {
  let v = ref(0)
  N.forEach2(xs, ys, (x, y) => v := v.contents + x + y)
  v.contents
}

describe(__MODULE__, () => {
  test("makeBy", () => {
    let u = N.makeBy(5, i => i * i)

    let f = i => eq(__LOC__, N.getExn(u, i), i * i)
    for i in 0 to 4 {
      f(i)
    }
    eq(__LOC__, N.map(u, i => i + 1), list{1, 2, 5, 10, 17})
    eq(__LOC__, N.getBy(list{1, 4, 3, 2}, x => mod(x, 2) == 0), Some(4))
    eq(__LOC__, N.getBy(list{1, 4, 3, 2}, x => mod(x, 5) == 0), None)
  })

  test("flatten", () => {
    eq(
      __LOC__,
      N.flatten(list{list{1}, list{2}, list{3}, list{}, N.makeBy(4, i => i)}),
      list{1, 2, 3, 0, 1, 2, 3},
    )
    eq(__LOC__, N.flatten(list{}), list{})
    eq(__LOC__, N.flatten(list{list{}, list{}, list{2}, list{1}, list{2}, list{}}), list{2, 1, 2})
  })

  test("concatMany", () => {
    eq(
      __LOC__,
      N.concatMany([list{1}, list{2}, list{3}, list{}, N.makeBy(4, i => i)]),
      list{1, 2, 3, 0, 1, 2, 3},
    )
    eq(__LOC__, N.concatMany([]), list{})
    eq(__LOC__, N.concatMany([list{}, list{}, list{2}, list{1}, list{2}, list{}]), list{2, 1, 2})
    eq(
      __LOC__,
      N.concatMany([list{}, list{}, list{2, 3}, list{1}, list{2}, list{}]),
      list{2, 3, 1, 2},
    )
    eq(__LOC__, N.concatMany([list{1, 2, 3}]), list{1, 2, 3})
  })

  test("concat", () => {
    eq(
      __LOC__,
      N.concat(N.makeBy(100, i => i), N.makeBy(100, i => i))->N.toArray,
      A.concat(A.makeBy(100, i => i), A.makeBy(100, i => i)),
    )

    eq(__LOC__, N.concat(list{1}, list{}), list{1})
    eq(__LOC__, N.concat(list{}, list{1}), list{1})
  })

  test("zip", () => {
    eq(__LOC__, N.zip(list{1, 2, 3}, list{3, 4}), list{(1, 3), (2, 4)})
    eq(__LOC__, N.zip(list{}, list{1}), list{})
    eq(__LOC__, N.zip(list{}, list{}), list{})
    eq(__LOC__, N.zip(list{1, 2, 3}, list{}), list{})
    eq(__LOC__, N.zip(list{1, 2, 3}, list{2, 3, 4}), list{(1, 2), (2, 3), (3, 4)})
  })

  let mod2 = x => mod(x, 2) == 0
  let evenIndex = (_x, i) => mod(i, 2) == 0

  test("partition", () => {
    eq(__LOC__, N.partition(list{1, 2, 3, 2, 3, 4}, mod2), (list{2, 2, 4}, list{1, 3, 3}))
    eq(__LOC__, N.partition(list{2, 2, 2, 4}, mod2), (list{2, 2, 2, 4}, list{}))
    eq(__LOC__, N.partition(list{2, 2, 2, 4}, x => !mod2(x)), (list{}, list{2, 2, 2, 4}))
    eq(__LOC__, N.partition(list{}, mod2), (list{}, list{}))
  })

  test("unzip", () => {
    eq(__LOC__, N.unzip(list{}), (list{}, list{}))
    eq(__LOC__, N.unzip(list{(1, 2)}), (list{1}, list{2}))
    eq(__LOC__, N.unzip(list{(1, 2), (3, 4)}), (list{1, 3}, list{2, 4}))
  })

  test("filter", () => {
    eq(__LOC__, N.keep(list{1, 2, 3, 4}, mod2), list{2, 4})
    eq(__LOC__, N.keep(list{1, 3, 41}, mod2), list{})
    eq(__LOC__, N.keep(list{}, mod2), list{})
    eq(__LOC__, N.keep(list{2, 2, 2, 4, 6}, mod2), list{2, 2, 2, 4, 6})
  })

  test("keepWithIndex", () => {
    eq(__LOC__, N.keepWithIndex(list{}, evenIndex), list{})
    eq(__LOC__, N.keepWithIndex(list{1, 2, 3, 4}, evenIndex), list{1, 3})
    eq(__LOC__, N.keepWithIndex(list{0, 1, 2, 3, 4, 5, 6, 7}, evenIndex), list{0, 2, 4, 6})
  })

  let id: int => int = x => x

  test("map", () => {
    eq(__LOC__, N.map(N.makeBy(5, id), x => x * 2), list{0, 2, 4, 6, 8})
    eq(__LOC__, N.map(list{}, id), list{})
    eq(__LOC__, N.map(list{1}, x => -x), list{-1})
  })

  let add = (a, b) => a + b
  let length_10_id = N.makeBy(10, id)
  let length_8_id = N.makeBy(8, id)

  test("mapWithIndex etc.", () => {
    let b = length_10_id
    let c = length_8_id
    let d = N.makeBy(10, x => 2 * x)
    let map2_add = (x, y) => N.zipBy(x, y, add)

    eq(__LOC__, map2_add(length_10_id, b), d)
    eq(__LOC__, map2_add(list{}, list{1}), list{})
    eq(__LOC__, map2_add(list{1}, list{}), list{})
    eq(__LOC__, map2_add(list{}, list{}), list{})
    eq(__LOC__, map2_add(length_10_id, b), N.concat(N.map(c, x => x * 2), list{16, 18}))
    eq(__LOC__, map2_add(length_10_id, length_8_id), N.mapWithIndex(length_8_id, (i, x) => i + x))
    eq(
      __LOC__,
      N.reverse(N.mapReverse2(length_10_id, length_10_id, add)),
      N.map(length_10_id, x => x * 2),
    )
    let xs = N.reverse(N.mapReverse2(length_8_id, length_10_id, add))
    eq(__LOC__, N.length(xs), 8)
    eq(__LOC__, xs, N.zipBy(length_10_id, length_8_id, add))
    eq(__LOC__, N.mapReverse2(list{1, 2, 3}, list{1, 2}, (x, y) => x + y), list{4, 2})
  })

  test("take", () => {
    eq(__LOC__, N.take(list{1, 2, 3}, 2), Some(list{1, 2}))
    eq(__LOC__, N.take(list{}, 1), None)
    eq(__LOC__, N.take(list{1, 2}, 3), None)
    eq(__LOC__, N.take(list{1, 2}, 2), Some(list{1, 2}))
    eq(__LOC__, N.take(length_10_id, 8), Some(length_8_id))
    eq(__LOC__, N.take(length_10_id, 0), Some(list{}))
    eq(__LOC__, N.take(length_8_id, -2), None)
  })

  test("droo", () => {
    eq(__LOC__, N.drop(length_10_id, 10), Some(list{}))
    eq(__LOC__, N.drop(length_10_id, 8), Some(list{8, 9}))
    eq(__LOC__, N.drop(length_10_id, 0), Some(length_10_id))
    eq(__LOC__, N.drop(length_8_id, -1), None)
  })

  test("splitAt", () => {
    let a = N.makeBy(5, id)
    eq(__LOC__, N.splitAt(list{}, 1), None)
    eq(__LOC__, N.splitAt(a, 6), None)
    eq(__LOC__, N.splitAt(a, 5), Some(a, list{}))
    eq(__LOC__, N.splitAt(a, 4), Some(list{0, 1, 2, 3}, list{4}))
    eq(__LOC__, N.splitAt(a, 3), Some(list{0, 1, 2}, list{3, 4}))
    eq(__LOC__, N.splitAt(a, 2), Some(list{0, 1}, list{2, 3, 4}))
    eq(__LOC__, N.splitAt(a, 1), Some(list{0}, list{1, 2, 3, 4}))
    eq(__LOC__, N.splitAt(a, 0), Some(list{}, a))
    eq(__LOC__, N.splitAt(a, -1), None)
  })

  test("removeAssoc", () => {
    let eqx = (x, y) => (x: int) == y

    ok(__LOC__, N.hasAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 2, \"=="))
    ok(__LOC__, !N.hasAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 4, \"=="))
    ok(__LOC__, N.hasAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 4, (x, y) => x + 1 == y))
    eq(
      __LOC__,
      N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 3, \"=="),
      list{(1, "1"), (2, "2")},
    )
    eq(
      __LOC__,
      N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 1, \"=="),
      list{(2, "2"), (3, "3")},
    )
    eq(
      __LOC__,
      N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 2, \"=="),
      list{(1, "1"), (3, "3")},
    )
    eq(
      __LOC__,
      N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 0, \"=="),
      list{(1, "1"), (2, "2"), (3, "3")},
    )

    eq(__LOC__, N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 3, eqx), list{(1, "1"), (2, "2")})
    eq(__LOC__, N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 1, eqx), list{(2, "2"), (3, "3")})
    eq(__LOC__, N.removeAssoc(list{(1, "1"), (2, "2"), (3, "3")}, 2, eqx), list{(1, "1"), (3, "3")})
    eq(__LOC__, N.removeAssoc(list{}, 2, eqx), list{})
    let ll = list{(1, "1"), (2, "2"), (3, "3")}
    let ll0 = N.removeAssoc(ll, 0, eqx)
    ok(__LOC__, ll === ll0)
    let ll1 = N.setAssoc(ll, 2, "22", \"==")
    eq(__LOC__, ll1, list{(1, "1"), (2, "22"), (3, "3")})
    let ll2 = N.setAssoc(ll1, 22, "2", \"==")
    ok(__LOC__, ll2 == list{(22, "2"), ...ll1})
    ok(__LOC__, N.tailExn(ll2) === ll1)
    ok(
      __LOC__,
      N.setAssoc(list{(1, "a"), (2, "b"), (3, "c")}, 2, "x", \"==") ==
        list{(1, "a"), (2, "x"), (3, "c")},
    )
    ok(
      __LOC__,
      N.setAssoc(list{(1, "a"), (3, "c")}, 2, "2", \"==") == list{(2, "2"), (1, "a"), (3, "c")},
    )
    eq(__LOC__, N.setAssoc(list{}, 1, "1", \"=="), list{(1, "1")})
    eq(__LOC__, N.setAssoc(list{(1, "2")}, 1, "1", \"=="), list{(1, "1")})

    eq(__LOC__, N.setAssoc(list{(0, "0"), (1, "2")}, 1, "1", \"=="), list{(0, "0"), (1, "1")})
    ok(__LOC__, N.getAssoc(list{(1, "a"), (2, "b"), (3, "c")}, 2, \"==") == Some("b"))
    ok(__LOC__, N.getAssoc(list{(1, "a"), (2, "b"), (3, "c")}, 4, \"==") == None)
  })

  test("head/tail etc.", () => {
    let succx = x => x + 1

    eq(__LOC__, (N.head(length_10_id), N.tail(length_10_id)), (Some(0), N.drop(length_10_id, 1)))
    eq(__LOC__, N.head(list{}), None)
    throw(__LOC__, () => N.headExn(list{}))
    throw(__LOC__, () => N.tailExn(list{})->ignore)
    throw(__LOC__, () => N.getExn(list{0, 1}, -1)->ignore)
    throw(__LOC__, () => N.getExn(list{0, 1}, 2)->ignore)
    eq(__LOC__, N.map(list{0, 1}, i => N.getExn(list{0, 1}, i)), list{0, 1})
    eq(__LOC__, N.headExn(list{1}), 1)
    eq(__LOC__, N.tailExn(list{1}), list{})
    N.forEachWithIndex(length_10_id, (i, x) => eq(__LOC__, N.get(length_10_id, i), Some(x)))
    eq(__LOC__, N.tail(list{}), None)
    eq(__LOC__, N.drop(list{}, 3), None)
    eq(__LOC__, N.mapWithIndex(list{}, (i, x) => i + x), list{})
    eq(__LOC__, N.get(length_10_id, -1), None)
    eq(__LOC__, N.get(length_10_id, 12), None)
    eq(__LOC__, sum(list{}), 0)
    eq(__LOC__, sum(length_10_id), 45)
    eq(__LOC__, N.makeBy(0, id), list{})
    eq(__LOC__, N.reverse(N.reverse(length_10_id)), length_10_id)
    eq(__LOC__, N.reverse(N.reverse(length_8_id)), length_8_id)
    eq(__LOC__, N.reverse(list{}), list{})
    eq(__LOC__, N.reverse(N.mapReverse(length_10_id, succx)), N.map(length_10_id, succx))
    eq(__LOC__, N.reduce(length_10_id, 0, add), 45)
    eq(__LOC__, N.reduceReverse(length_10_id, 0, add), 45)
    eq(__LOC__, N.reduceReverse(N.makeBy(10_000, i => i), 0, \"+"), 0 + 9_999 * 5_000)
    /* eq __LOC__
     (N.mapRev2 length_10_id length_8_id add ) */
    eq(__LOC__, sum2(length_10_id, length_10_id), 90)
    eq(__LOC__, sum2(length_8_id, length_10_id), 56)
    eq(__LOC__, sum2(length_10_id, length_8_id), 56)
    eq(__LOC__, N.reduce2(length_10_id, length_8_id, 0, (acc, x, y) => acc + x + y), 56)
    eq(__LOC__, N.reduce2(list{1, 2, 3}, list{2, 4, 6}, 0, (a, b, c) => a + b + c), 18)
    eq(__LOC__, N.reduceReverse2(length_10_id, length_8_id, 0, (acc, x, y) => acc + x + y), 56)
    eq(__LOC__, N.reduceReverse2(length_10_id, length_10_id, 0, (acc, x, y) => acc + x + y), 90)
    eq(__LOC__, N.reduceReverse2(list{1, 2, 3}, list{1, 2}, 0, (acc, x, y) => acc + x + y), 6)
    eq(__LOC__, N.every(list{2, 4, 6}, mod2), true)
    eq(__LOC__, N.every(list{1}, mod2), false)
    eq(__LOC__, N.every(list{}, mod2), true)
    eq(__LOC__, N.some(list{1, 2, 5}, mod2), true)
    eq(__LOC__, N.some(list{1, 3, 5}, mod2), false)
    eq(__LOC__, N.some(list{}, mod2), false)
    eq(__LOC__, N.has(list{1, 2, 3}, "2", (x, s) => Js.Int.toString(x) == s), true)
    eq(__LOC__, N.has(list{1, 2, 3}, "0", (x, s) => Js.Int.toString(x) == s), false)

    ok(__LOC__, N.reduceReverse(list{1, 2, 3, 4}, 0, \"+") == 10)
    ok(__LOC__, N.reduceReverse(list{1, 2, 3, 4}, 10, \"-") == 0)
    ok(__LOC__, N.reduceReverse(list{1, 2, 3, 4}, list{}, N.add) == list{1, 2, 3, 4})
    ok(__LOC__, N.reduce(list{1, 2, 3, 4}, 0, \"+") == 10)
    ok(__LOC__, N.reduce(list{1, 2, 3, 4}, 10, \"-") == 0)
    ok(__LOC__, N.reduce(list{1, 2, 3, 4}, list{}, N.add) == list{4, 3, 2, 1})
    ok(__LOC__, N.reduceWithIndex(list{1, 2, 3, 4}, 0, (acc, x, i) => acc + x + i) == 16)
    ok(__LOC__, N.reduceReverse2(list{1, 2, 3}, list{1, 2}, 0, (acc, x, y) => acc + x + y) == 6)
    let a = N.makeBy(10_000, i => i)
    ok(
      __LOC__,
      N.reduceReverse2(a, list{0, ...a}, 0, (acc, x, y) => acc + x + y) == 9_999 * 10_000 - 9999,
    )
  })

  test("every2", () => {
    eq(__LOC__, N.every2(list{}, list{1}, (x, y) => x > y), true)
    eq(__LOC__, N.every2(list{2, 3}, list{1}, (x, y) => x > y), true)
    eq(__LOC__, N.every2(list{2}, list{1}, (x, y) => x > y), true)
    eq(__LOC__, N.every2(list{2, 3}, list{1, 4}, (x, y) => x > y), false)
    eq(__LOC__, N.every2(list{2, 3}, list{1, 0}, (x, y) => x > y), true)
  })

  test("some2", () => {
    eq(__LOC__, N.some2(list{}, list{1}, (x, y) => x > y), false)
    eq(__LOC__, N.some2(list{2, 3}, list{1}, (x, y) => x > y), true)
    eq(__LOC__, N.some2(list{2, 3}, list{1, 4}, (x, y) => x > y), true)
    eq(__LOC__, N.some2(list{0, 3}, list{1, 4}, (x, y) => x > y), false)
    eq(__LOC__, N.some2(list{0, 3}, list{3, 2}, (x, y) => x > y), true)
    eq(__LOC__, N.some2(list{1, 2, 3}, list{-1, -2}, (x, y) => x == y), false)
  })

  test("add", () => {
    eq(__LOC__, list{}->N.add(3)->N.add(2), list{2, 3})
  })

  test("cmp", () => {
    ok(__LOC__, N.cmp(list{1, 2, 3}, list{0, 1, 2, 3}, compare) > 0)
    ok(__LOC__, N.cmp(list{1, 2, 3, 4}, list{1, 2, 3}, compare) > 0)
    ok(__LOC__, N.cmp(list{1, 2, 3}, list{1, 2, 3, 4}, compare) < 0)
    ok(__LOC__, N.cmp(list{1, 2, 3}, list{0, 1, 2}, (x, y) => compare(x, y)) > 0)
    ok(__LOC__, N.cmp(list{1, 2, 3}, list{1, 2, 3}, (x, y) => compare(x, y)) == 0)
    ok(__LOC__, N.cmp(list{1, 2, 4}, list{1, 2, 3}, (x, y) => compare(x, y)) > 0)
  })

  test("cmpByLength", () => {
    ok(__LOC__, N.cmpByLength(list{}, list{}) == 0)
    ok(__LOC__, N.cmpByLength(list{1}, list{}) > 0)
    ok(__LOC__, N.cmpByLength(list{}, list{1}) < 0)
    ok(__LOC__, N.cmpByLength(list{1, 2}, list{1}) > 0)
    ok(__LOC__, N.cmpByLength(list{1}, list{1, 2}) < 0)
    ok(__LOC__, N.cmpByLength(list{1, 3}, list{1, 2}) == 0)
  })

  test("makeBy", () => {
    let makeTest = n => eq(__LOC__, N.make(n, 3), N.makeBy(n, _ => 3))

    makeTest(0)
    makeTest(1)
    makeTest(2)
    makeTest(3)
  })

  test("sort", () => {
    let cmp = (a, b) => a - b
    eq(__LOC__, N.sort(list{5, 4, 3, 2}, cmp), list{2, 3, 4, 5})
    eq(__LOC__, N.sort(list{3, 9, 37, 3, 1}, cmp), list{1, 3, 3, 9, 37})
  })

  test("eq", () => {
    ok(__LOC__, !N.eq(list{1, 2, 3}, list{1, 2}, (x, y) => x == y))
    ok(__LOC__, N.eq(list{1, 2, 3}, list{1, 2, 3}, (x, y) => x == y))
    ok(__LOC__, !N.eq(list{1, 2, 3}, list{1, 2, 4}, (x, y) => x == y))
    ok(__LOC__, !N.eq(list{1, 2, 3}, list{1, 2, 3, 4}, \"=="))
  })

  test("keepMap", () => {
    let u0 = N.makeBy(20, x => x)
    let u1 = u0->N.keepMap(
      x =>
        if mod(x, 7) == 0 {
          Some(x + 1)
        } else {
          None
        },
    )
    eq(__LOC__, u1, list{1, 8, 15})
    ok(
      __LOC__,
      {
        open N

        list{1, 2, 3, 4}->keepMap(
          x =>
            if mod(x, 2) == 0 {
              Some(-x)
            } else {
              None
            },
        ) == list{-2, -4}
      },
    )
    ok(
      __LOC__,
      N.keepMap(
        list{1, 2, 3, 4},
        x =>
          if mod(x, 5) == 0 {
            Some(x)
          } else {
            None
          },
      ) == list{},
    )
  })
})

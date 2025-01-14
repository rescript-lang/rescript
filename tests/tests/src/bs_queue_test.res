let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

let b = (loc, x) => Mt.bool_suites(~test_id, ~suites, loc, x)
/* TEST
 */

module Q = Belt.MutableQueue

let does_raise = (f, q) =>
  try {
    ignore((f(q): int))
    false
  } catch {
  | _ => true
  }
let \"++" = (q, x) => {
  Q.add(q, x)
  q
}

let () = {
  let q = Q.make()
  assert(Q.toArray(q) == [] && Q.size(q) == 0)
  assert(Q.toArray(q ++ 1) == [1] && Q.size(q) == 1)
  assert(Q.toArray(q ++ 2) == [1, 2] && Q.size(q) == 2)
  assert(Q.toArray(q ++ 3) == [1, 2, 3] && Q.size(q) == 3)
  assert(Q.toArray(q ++ 4) == [1, 2, 3, 4] && Q.size(q) == 4)
  assert(Q.popExn(q) == 1)
  assert(Q.toArray(q) == [2, 3, 4] && Q.size(q) == 3)
  assert(Q.popExn(q) == 2)
  assert(Q.toArray(q) == [3, 4] && Q.size(q) == 2)
  assert(Q.popExn(q) == 3)
  assert(Q.toArray(q) == [4] && Q.size(q) == 1)
  assert(Q.popExn(q) == 4)
  assert(Q.toArray(q) == [] && Q.size(q) == 0)
  assert(does_raise(Q.popExn, q))
}

let () = {
  let q = Q.make()
  assert(Q.popExn(q ++ 1) == 1)
  assert(does_raise(Q.popExn, q))
  assert(Q.popExn(q ++ 2) == 2)
  assert(does_raise(Q.popExn, q))
  assert(Q.size(q) == 0)
}

let () = {
  let q = Q.make()
  assert(Q.peekExn(q ++ 1) == 1)
  assert(Q.peekExn(q ++ 2) == 1)
  assert(Q.peekExn(q ++ 3) == 1)
  assert(Q.peekExn(q) == 1)
  assert(Q.popExn(q) == 1)
  assert(Q.peekExn(q) == 2)
  assert(Q.popExn(q) == 2)
  assert(Q.peekExn(q) == 3)
  assert(Q.popExn(q) == 3)
  assert(does_raise(Q.peekExn, q))
  assert(does_raise(Q.peekExn, q))
}

let () = {
  let q = Q.make()
  for i in 1 to 10 {
    Q.add(q, i)
  }
  Q.clear(q)
  assert(Q.size(q) == 0)
  assert(does_raise(Q.popExn, q))
  assert(q == Q.make())
  Q.add(q, 42)
  assert(Q.popExn(q) == 42)
}

let () = {
  let q1 = Q.make()
  for i in 1 to 10 {
    Q.add(q1, i)
  }
  let q2 = Q.copy(q1)
  assert(Q.toArray(q1) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  assert(Q.toArray(q2) == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  assert(Q.size(q1) == 10)
  assert(Q.size(q2) == 10)
  for i in 1 to 10 {
    assert(Q.popExn(q1) == i)
  }
  for i in 1 to 10 {
    assert(Q.popExn(q2) == i)
  }
}

let () = {
  let q = Q.make()
  assert(Q.isEmpty(q))
  for i in 1 to 10 {
    Q.add(q, i)
    assert(Q.size(q) == i)
    assert(!Q.isEmpty(q))
  }
  for i in 10 downto 1 {
    assert(Q.size(q) == i)
    assert(!Q.isEmpty(q))
    ignore((Q.popExn(q): int))
  }
  assert(Q.size(q) == 0)
  assert(Q.isEmpty(q))
}

let () = {
  let q = Q.make()
  for i in 1 to 10 {
    Q.add(q, i)
  }
  let i = ref(1)
  Q.forEach(q, j => {
    assert(i.contents == j)
    incr(i)
  })
}

let () = {
  let q1 = Q.make() and q2 = Q.make()
  assert(Q.size(q1) == 0)
  assert(Q.toArray(q1) == [])
  assert(Q.size(q2) == 0)
  assert(Q.toArray(q2) == [])
  Q.transfer(q1, q2)
  assert(Q.size(q1) == 0)
  assert(Q.toArray(q1) == [])
  assert(Q.size(q2) == 0)
  assert(Q.toArray(q2) == [])
}

let () = {
  let q1 = Q.make() and q2 = Q.make()
  for i in 1 to 4 {
    Q.add(q1, i)
  }
  assert(Q.size(q1) == 4)
  assert(Q.toArray(q1) == [1, 2, 3, 4])
  assert(Q.size(q2) == 0)
  assert(Q.toArray(q2) == [])
  Q.transfer(q1, q2)
  assert(Q.size(q1) == 0)
  assert(Q.toArray(q1) == [])
  assert(Q.size(q2) == 4)
  assert(Q.toArray(q2) == [1, 2, 3, 4])
}

let () = {
  let q1 = Q.make() and q2 = Q.make()
  for i in 5 to 8 {
    Q.add(q2, i)
  }
  assert(Q.size(q1) == 0)
  assert(Q.toArray(q1) == [])
  assert(Q.size(q2) == 4)
  assert(Q.toArray(q2) == [5, 6, 7, 8])
  Q.transfer(q1, q2)
  assert(Q.size(q1) == 0)
  assert(Q.toArray(q1) == [])
  assert(Q.size(q2) == 4)
  assert(Q.toArray(q2) == [5, 6, 7, 8])
}

let () = {
  let q1 = Q.make() and q2 = Q.make()
  for i in 1 to 4 {
    Q.add(q1, i)
  }
  for i in 5 to 8 {
    Q.add(q2, i)
  }
  assert(Q.size(q1) == 4)
  assert(Q.toArray(q1) == [1, 2, 3, 4])
  assert(Q.size(q2) == 4)
  assert(Q.toArray(q2) == [5, 6, 7, 8])
  Q.transfer(q1, q2)
  assert(Q.size(q1) == 0)
  assert(Q.toArray(q1) == [])
  let v = [5, 6, 7, 8, 1, 2, 3, 4]
  assert(Q.size(q2) == 8)
  assert(Q.toArray(q2) == v)

  assert(Q.reduce(q2, 0, (x, y) => x - y) == Belt.Array.reduce(v, 0, (x, y) => x - y))
}

let () = Js.log("OK")

let () = {
  let q = Q.fromArray([1, 2, 3, 4])
  let q1 = Q.map(q, x => x - 1)
  eq(__LOC__, Q.toArray(q1), [0, 1, 2, 3])
  b(__LOC__, Q.isEmpty(Q.fromArray([])))
  b(__LOC__, Q.isEmpty(Q.map(Q.fromArray([]), x => x + 1)))
}

Mt.from_pair_suites(__MODULE__, suites.contents)

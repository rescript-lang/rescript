let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

type rec node = {
  value: int,
  left: t,
  right: t,
}
@deriving(abstract) and t = Js.undefined<node>

module S = Belt.MutableStack
module Q = Belt.MutableQueue

let inOrder = (v: t): array<int> => {
  let current = ref(v)
  let s: S.t<node> = S.make()
  let q: Q.t<int> = Q.make()
  while current.contents !== Js.undefined {
    let v = Js.Undefined.getUnsafe(current.contents)
    S.push(s, v)
    current := leftGet(v)
  }
  while !S.isEmpty(s) {
    current := S.popUndefined(s)
    let v = Js.Undefined.getUnsafe(current.contents)
    Q.add(q, valueGet(v))
    current := rightGet(v)
    while current.contents !== Js.undefined {
      let v = Js.Undefined.getUnsafe(current.contents)
      S.push(s, v)
      current := leftGet(v)
    }
  }
  Q.toArray(q)
}

let inOrder3 = (v: t): array<int> => {
  let current = ref(v)
  let s: S.t<node> = S.make()
  let q: Q.t<int> = Q.make()
  while current.contents !== Js.undefined {
    let v = Js.Undefined.getUnsafe(current.contents)
    S.push(s, v)
    current := leftGet(v)
  }
  S.dynamicPopIter(s, popped => {
    Q.add(q, valueGet(popped))
    let current = ref(rightGet(popped))
    while current.contents !== Js.undefined {
      let v = Js.Undefined.getUnsafe(current.contents)
      S.push(s, v)
      current := leftGet(v)
    }
  })
  Q.toArray(q)
}

let inOrder2 = (v: t) => {
  let todo = ref(true)
  let cursor = ref(v)
  let s: S.t<node> = S.make()
  let q: Q.t<int> = Q.make()
  while todo.contents {
    if cursor.contents !== Js.undefined {
      let v = Js.Undefined.getUnsafe(cursor.contents)
      S.push(s, v)
      cursor := leftGet(v)
    } else if !S.isEmpty(s) {
      cursor := S.popUndefined(s)
      let current = Js.Undefined.getUnsafe(cursor.contents)
      Q.add(q, valueGet(current))
      cursor := rightGet(current)
    } else {
      todo := false
    }
  }
}

let n = (~l=?, ~r=?, a) =>
  node(~value=a, ~left=Js.Undefined.fromOption(l), ~right=Js.Undefined.fromOption(r))

let test1 = n(1, ~l=n(2, ~l=n(4), ~r=n(5)), ~r=n(3))

let pushAllLeft = (st1, s1) => {
  let current = ref(st1)
  while current.contents !== Js.undefined {
    let v = Js.Undefined.getUnsafe(current.contents)
    S.push(s1, v)
    current := leftGet(v)
  }
}

let test2 = n(3, ~l=n(1, ~l=n(5, ~l=n(2, ~l=n(4)))))

let test3 = n(1, ~l=n(5, ~l=n(2, ~l=n(4))), ~r=n(3))

eq(__LOC__, inOrder(Js.Undefined.return(test1)), [4, 2, 5, 1, 3])
eq(__LOC__, inOrder3(Js.Undefined.return(test1)), [4, 2, 5, 1, 3])

Mt.from_pair_suites(__FILE__, suites.contents)

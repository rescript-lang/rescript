open Mocha
open Test_utils

/*
  This tests Belt.MutableStack with a small binary tree:

        1
       / \
      2   3
     / \
    4   5

  In-order traversal visits left branch, node, then right branch, so the
  expected order is [4, 2, 5, 1, 3]. The two traversal implementations below
  cover both manual stack popping and dynamicPopIter, which keeps popping while
  the callback is allowed to push more work.
*/

type rec node = {
  value: int,
  left: t,
  right: t,
}
@deriving(abstract) and t = option<node>

module S = Belt.MutableStack
module Q = Belt.MutableQueue

let inOrder = (v: t): array<int> => {
  let current = ref(v)
  let s: S.t<node> = S.make()
  let q: Q.t<int> = Q.make()
  while current.contents->Option.isSome {
    let v = current.contents->Option.getUnsafe
    S.push(s, v)
    current := leftGet(v)
  }
  while !S.isEmpty(s) {
    current := Some(S.popOrThrow(s))
    let v = current.contents->Option.getUnsafe
    Q.add(q, valueGet(v))
    current := rightGet(v)
    while current.contents->Option.isSome {
      let v = current.contents->Option.getUnsafe
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
  while current.contents->Option.isSome {
    let v = current.contents->Option.getUnsafe
    S.push(s, v)
    current := leftGet(v)
  }
  S.dynamicPopIter(s, popped => {
    Q.add(q, valueGet(popped))
    let current = ref(rightGet(popped))
    while current.contents->Option.isSome {
      let v = current.contents->Option.getUnsafe
      S.push(s, v)
      current := leftGet(v)
    }
  })
  Q.toArray(q)
}

let n = (~l=?, ~r=?, a) => node(~value=a, ~left=l, ~right=r)

let test1 = n(1, ~l=n(2, ~l=n(4), ~r=n(5)), ~r=n(3))

describe(__MODULE__, () => {
  test("tree in-order traversal", () => {
    eq(__LOC__, inOrder(Some(test1)), [4, 2, 5, 1, 3])
    eq(__LOC__, inOrder3(Some(test1)), [4, 2, 5, 1, 3])
  })
})

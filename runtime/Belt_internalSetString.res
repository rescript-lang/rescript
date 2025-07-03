type value = string
module S = Belt_SortArrayString

module N = Belt_internalAVLset
module A = Belt_Array

type t = N.t<value>

let rec has = (t: t, x: value) =>
  switch t {
  | None => false
  | Some(n) =>
    let v = n.value
    x == v ||
      has(
        if x < v {
          n.left
        } else {
          n.right
        },
        x,
      )
  }

let rec compareAux = (e1, e2) =>
  switch (e1, e2) {
  | (list{h1, ...t1}, list{h2, ...t2}) =>
    let (k1: value, k2) = (h1.N.value, h2.N.value)
    if k1 == k2 {
      compareAux(N.stackAllLeft(h1.right, t1), N.stackAllLeft(h2.right, t2))
    } else if k1 < k2 {
      -1
    } else {
      1
    }
  | (_, _) => 0
  }

let cmp = (s1, s2) => {
  let (len1, len2) = (N.size(s1), N.size(s2))
  if len1 == len2 {
    compareAux(N.stackAllLeft(s1, list{}), N.stackAllLeft(s2, list{}))
  } else if len1 < len2 {
    -1
  } else {
    1
  }
}

let eq = (s1: t, s2) => cmp(s1, s2) == 0

/* This algorithm applies to BST, it does not need to be balanced tree */
let rec subset = (s1: t, s2: t) =>
  switch (s1, s2) {
  | (None, _) => true
  | (_, None) => false
  | (Some(t1), Some(t2)) /* Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) */ =>
    let {N.left: l1, value: v1, right: r1} = t1
    let {N.left: l2, value: v2, right: r2} = t2
    if v1 == v2 {
      subset(l1, l2) && subset(r1, r2)
    } else if v1 < v2 {
      subset(N.create(l1, v1, None), l2) && subset(r1, s2)
    } else {
      subset(N.create(None, v1, r1), r2) && subset(l1, s2)
    }
  }

let rec get = (n: t, x: value) =>
  switch n {
  | None => None
  | Some(t) =>
    let v = t.value
    if x == v {
      Some(v)
    } else {
      get(
        if x < v {
          t.left
        } else {
          t.right
        },
        x,
      )
    }
  }

let rec getUndefined = (n: t, x: value) =>
  switch n {
  | None => Js.undefined
  | Some(t) =>
    let v = t.value
    if x == v {
      Js.Undefined.return(v)
    } else {
      getUndefined(
        if x < v {
          t.left
        } else {
          t.right
        },
        x,
      )
    }
  }

let rec getOrThrow = (n: t, x: value) =>
  switch n {
  | None => throw(Not_found)
  | Some(t) =>
    let v = t.value
    if x == v {
      v
    } else {
      getOrThrow(
        if x < v {
          t.left
        } else {
          t.right
        },
        x,
      )
    }
  }

/* ************************************************************************** */
let rec addMutate = (t, x: value) =>
  switch t {
  | None => N.singleton(x)
  | Some(nt) =>
    let k = nt.N.value
    if x == k {
      t
    } else {
      let {N.left: l, right: r} = nt
      if x < k {
        nt.left = addMutate(l, x)
      } else {
        nt.right = addMutate(r, x)
      }
      Some(N.balMutate(nt))
    }
  }

let fromArray = (xs: array<value>) => {
  let len = A.length(xs)
  if len == 0 {
    None
  } else {
    let next = ref(S.strictlySortedLength(xs))
    let result = ref(
      if next.contents >= 0 {
        N.fromSortedArrayAux(xs, 0, next.contents)
      } else {
        next.contents = -next.contents
        N.fromSortedArrayRevAux(xs, next.contents - 1, next.contents)
      },
    )
    for i in next.contents to len - 1 {
      result.contents = addMutate(result.contents, A.getUnsafe(xs, i))
    }
    result.contents
  }
}

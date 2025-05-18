@@config({flags: ["-bs-noassertfalse"]})

type key = string

module N = Belt_internalAVLtree
module A = Belt_Array
module S = Belt_SortArray

type t<'a> = N.t<key, 'a>

let rec add = (t, x: key, data: _) =>
  switch t {
  | None => N.singleton(x, data)
  | Some(n) =>
    let k = n.N.key
    if x == k {
      Some(N.updateValue(n, data))
    } else {
      let v = n.N.value
      if x < k {
        N.bal(add(n.N.left, x, data), k, v, n.N.right)
      } else {
        N.bal(n.N.left, k, v, add(n.N.right, x, data))
      }
    }
  }

let rec get = (n, x: key) =>
  switch n {
  | None => None
  | Some(n) =>
    let v = n.N.key
    if x == v {
      Some(n.N.value)
    } else {
      get(
        if x < v {
          n.N.left
        } else {
          n.N.right
        },
        x,
      )
    }
  }

let rec getUndefined = (n, x: key) =>
  switch n {
  | None => Js.undefined
  | Some(n) =>
    let v = n.N.key
    if x == v {
      Js.Undefined.return(n.N.value)
    } else {
      getUndefined(
        if x < v {
          n.N.left
        } else {
          n.N.right
        },
        x,
      )
    }
  }

let rec getOrThrow = (n, x: key) =>
  switch n {
  | None => throw(Not_found)
  | Some(n) =>
    let v = n.N.key
    if x == v {
      n.N.value
    } else {
      getOrThrow(
        if x < v {
          n.N.left
        } else {
          n.N.right
        },
        x,
      )
    }
  }

let rec getWithDefault = (n, x: key, def) =>
  switch n {
  | None => def
  | Some(n) =>
    let v = n.N.key
    if x == v {
      n.N.value
    } else {
      getWithDefault(
        if x < v {
          n.N.left
        } else {
          n.N.right
        },
        x,
        def,
      )
    }
  }

let rec has = (n, x: key) =>
  switch n {
  | None => false
  | Some(n) /* Node(l, v, d, r, _) */ =>
    let v = n.N.key
    x == v ||
      has(
        if x < v {
          n.N.left
        } else {
          n.N.right
        },
        x,
      )
  }

let rec remove = (n, x: key) =>
  switch n {
  | None => n
  | Some(n) =>
    let {N.left: l, key: v, right: r} = n
    if x == v {
      switch (l, r) {
      | (None, _) => r
      | (_, None) => l
      | (_, Some(rn)) =>
        let (kr, vr) = (ref(rn.key), ref(rn.value))
        let r = N.removeMinAuxWithRef(rn, kr, vr)
        N.bal(l, kr.contents, vr.contents, r)
      }
    } else if x < v {
      open N
      bal(remove(l, x), v, n.value, r)
    } else {
      open N
      bal(l, v, n.value, remove(r, x))
    }
  }

let rec splitAux = (x: key, n: N.node<_>): (t<_>, option<_>, t<_>) => {
  let {N.left: l, key: v, value: d, right: r} = n
  if x == v {
    (l, Some(d), r)
  } else if x < v {
    switch l {
    | None => (None, None, Some(n))
    | Some(l) =>
      let (ll, pres, rl) = splitAux(x, l)
      (ll, pres, N.join(rl, v, d, r))
    }
  } else {
    switch r {
    | None => (Some(n), None, None)
    | Some(r) =>
      let (lr, pres, rr) = splitAux(x, r)
      (N.join(l, v, d, lr), pres, rr)
    }
  }
}

let split = (x: key, n) =>
  switch n {
  | None => (None, None, None)
  | Some(n) => splitAux(x, n)
  }

let rec merge = (s1, s2, f) =>
  switch (s1, s2) {
  | (None, None) => None
  | (Some(n) /* (Node (l1, v1, d1, r1, h1), _) */, _)
    if n.N.height >=
    switch s2 {
    | None => 0
    | Some(n) => n.N.height
    } =>
    let {N.left: l1, key: v1, value: d1, right: r1} = n
    let (l2, d2, r2) = split(v1, s2)
    N.concatOrJoin(merge(l1, l2, f), v1, f(v1, Some(d1), d2), merge(r1, r2, f))
  | (_, Some(n)) /* Node (l2, v2, d2, r2, h2) */ =>
    let {N.left: l2, key: v2, value: d2, right: r2} = n
    let (l1, d1, r1) = split(v2, s1)
    N.concatOrJoin(merge(l1, l2, f), v2, f(v2, d1, Some(d2)), merge(r1, r2, f))
  | _ => assert(false)
  }

let rec compareAux = (e1, e2, vcmp) =>
  switch (e1, e2) {
  | (list{h1, ...t1}, list{h2, ...t2}) =>
    let c = Pervasives.compare((h1.N.key: key), h2.N.key)
    if c == 0 {
      let cx = vcmp(h1.N.value, h2.N.value)
      if cx == 0 {
        compareAux(N.stackAllLeft(h1.N.right, t1), N.stackAllLeft(h2.N.right, t2), vcmp)
      } else {
        cx
      }
    } else {
      c
    }
  | (_, _) => 0
  }

let cmp = (s1, s2, cmp) => {
  let (len1, len2) = (N.size(s1), N.size(s2))
  if len1 == len2 {
    compareAux(N.stackAllLeft(s1, list{}), N.stackAllLeft(s2, list{}), cmp)
  } else if len1 < len2 {
    -1
  } else {
    1
  }
}

let rec eqAux = (e1, e2, eq) =>
  switch (e1, e2) {
  | (list{h1, ...t1}, list{h2, ...t2}) =>
    if (h1.N.key: key) == h2.N.key && eq(h1.N.value, h2.N.value) {
      eqAux(N.stackAllLeft(h1.N.right, t1), N.stackAllLeft(h2.N.right, t2), eq)
    } else {
      false
    }
  | (_, _) => true
  } /* end */

let eq = (s1, s2, eq) => {
  let (len1, len2) = (N.size(s1), N.size(s2))
  if len1 == len2 {
    eqAux(N.stackAllLeft(s1, list{}), N.stackAllLeft(s2, list{}), eq)
  } else {
    false
  }
}

let rec addMutate = (t: t<_>, x, data): t<_> =>
  switch t {
  | None => N.singleton(x, data)
  | Some(nt) =>
    let k = nt.N.key

    /* let  c = (Belt_Cmp.getCmpInternal cmp) x k [@bs] in */
    if x == k {
      nt.N.key = x
      nt.value = data
      Some(nt)
    } else {
      let (l, r) = (nt.N.left, nt.N.right)
      if x < k {
        let ll = addMutate(l, x, data)
        nt.left = ll
      } else {
        nt.right = addMutate(r, x, data)
      }
      Some(N.balMutate(nt))
    }
  }

let fromArray = (xs: array<(key, _)>) => {
  let len = A.length(xs)
  if len == 0 {
    None
  } else {
    let next = ref(S.strictlySortedLength(xs, ((x0, _), (y0, _)) => x0 < y0))

    let result = ref(
      if next.contents >= 0 {
        N.fromSortedArrayAux(xs, 0, next.contents)
      } else {
        next.contents = -next.contents
        N.fromSortedArrayRevAux(xs, next.contents - 1, next.contents)
      },
    )
    for i in next.contents to len - 1 {
      let (k, v) = A.getUnsafe(xs, i)
      result.contents = addMutate(result.contents, k, v)
    }
    result.contents
  }
}

let cmpU = cmp
let eqU = eq
let mergeU = merge

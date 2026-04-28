module Comparable: {
  type comparator<'a>
  let getcompare: comparator<'a> => ('a, 'a) => int
  module type C = {
    type id
    type key
    let compare: comparator<key>
  }
  type compare<'key, 'id> = module(C with type key = 'key and type id = 'id)
  module Make: (
    M: {
      type key
      let compare: (key, key) => int
    },
  ) => (C with type key = M.key)
} = {
  type comparator<'a> = ('a, 'a) => int
  let getcompare: comparator<'a> => ('a, 'a) => int = x => x
  module type C = {
    type id
    type key
    let compare: comparator<key>
  }
  type compare<'key, 'id> = module(C with type key = 'key and type id = 'id)

  module Make = (
    M: {
      type key
      let compare: (key, key) => int
    },
  ) => {
    type id
    type key = M.key
    let compare = M.compare
  }
}

type rec t<'a, 'key> =
  | Empty
  | Node(t<'a, 'key>, 'key, 'a, t<'a, 'key>, int)

let height = x =>
  switch x {
  | Empty => 0
  | Node(_, _, _, _, h) => h
  }

let create = (l, x, d, r) => {
  let hl = height(l) and hr = height(r)
  Node(
    l,
    x,
    d,
    r,
    if hl >= hr {
      hl + 1
    } else {
      hr + 1
    },
  )
}

let bal = (l, x, d, r) => {
  let hl = switch l {
  | Empty => 0
  | Node(_, _, _, _, h) => h
  }
  let hr = switch r {
  | Empty => 0
  | Node(_, _, _, _, h) => h
  }
  if hl > hr + 2 {
    switch l {
    | Empty => invalid_arg("Map.bal")
    | Node(ll, lv, ld, lr, _) =>
      if height(ll) >= height(lr) {
        create(ll, lv, ld, create(lr, x, d, r))
      } else {
        switch lr {
        | Empty => invalid_arg("Map.bal")
        | Node(lrl, lrv, lrd, lrr, _) =>
          create(create(ll, lv, ld, lrl), lrv, lrd, create(lrr, x, d, r))
        }
      }
    }
  } else if hr > hl + 2 {
    switch r {
    | Empty => invalid_arg("Map.bal")
    | Node(rl, rv, rd, rr, _) =>
      if height(rr) >= height(rl) {
        create(create(l, x, d, rl), rv, rd, rr)
      } else {
        switch rl {
        | Empty => invalid_arg("Map.bal")
        | Node(rll, rlv, rld, rlr, _) =>
          create(create(l, x, d, rll), rlv, rld, create(rlr, rv, rd, rr))
        }
      }
    }
  } else {
    Node(
      l,
      x,
      d,
      r,
      if hl >= hr {
        hl + 1
      } else {
        hr + 1
      },
    )
  }
}

let rec add = (x, data, compare, x_) =>
  switch x_ {
  | Empty => Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) =>
    let c = compare(x, v)
    if c == 0 {
      Node(l, x, data, r, h)
    } else if c < 0 {
      bal(add(x, data, compare, l), v, d, r)
    } else {
      bal(l, v, d, add(x, data, compare, r))
    }
  }

/* type ('k, 'id) compare = */
/* (module  C with type id  = 'id and type key = 'k) */

type t1<'k, 'v, 'id> = {
  compare: Comparable.compare<'k, 'id>,
  data: t<'v, 'k>,
}

let add = (type k v id, x, data, v: t1<k, v, id>) => {
  module X = unpack(v.compare)
  {
    compare: v.compare,
    data: add(x, data, Comparable.getcompare(X.compare), v.data),
  }
}

let empty = (v: Comparable.compare<_>) => {compare: v, data: Empty}

/* module Make (X : sig type key end) = struct
  type nonrec key = X.key
  type id 
end
module U = struct
  include Make ( struct type key = int end)
  let compare = Pervasives.compare end */

module V0 = Comparable.Make({
  type key = int
  let compare = (x: key, y) => Pervasives.compare((x: key), y)
})
module V1 = Comparable.Make({
  type key = int
  let compare = (x: key, y) => Pervasives.compare(x, y)
})
let v0 = empty(module(V0))
let v1 = empty(module(V1))

let v3 = add(3, "a", v0)
Console.log(v3)
/* let () = v0 = v1 */

/* let v1 = empty */
/* (module (struct type id  type key = int let compare = Pervasives.compare end)) */

/* let v2 = empty [%bs.map compare] */
/* let _ = u = v */

/* local variables: */
/* compile-command: "ocamlc.opt -c xx.ml" */
/* end: */

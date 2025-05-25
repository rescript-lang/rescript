@@config({no_export, flags: ["-w", "@A"]})

let compare = (x: int, y: int) => compare(x, y)

type rec t<'a> =
  | Empty
  | Node(t<'a>, int, 'a, t<'a>, int)

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
    | Empty => assert(false)
    | Node(ll, lv, ld, lr, _) =>
      if height(ll) >= height(lr) {
        create(ll, lv, ld, create(lr, x, d, r))
      } else {
        switch lr {
        | Empty => assert(false)
        | Node(lrl, lrv, lrd, lrr, _) =>
          create(create(ll, lv, ld, lrl), lrv, lrd, create(lrr, x, d, r))
        }
      }
    }
  } else if hr > hl + 2 {
    switch r {
    | Empty => assert(false)
    | Node(rl, rv, rd, rr, _) =>
      if height(rr) >= height(rl) {
        create(create(l, x, d, rl), rv, rd, rr)
      } else {
        switch rl {
        | Empty => assert(false)
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

let rec add = (x, data, tree) =>
  switch tree {
  | Empty => Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) =>
    let c = compare(x, v)
    if c == 0 {
      Node(l, x, data, r, h)
    } else if c < 0 {
      bal(add(x, data, l), v, d, r)
    } else {
      bal(l, v, d, add(x, data, r))
    }
  }

let empty = Empty

/* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*/

/* Same as create and bal, but no assumptions are made on the
 relative heights of l and r. */

/* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. */

/* end */
let m = Belt.List.reduceReverse(list{(10, 'a'), (3, 'b'), (7, 'c'), (20, 'd')}, empty, (
  acc,
  (k, v),
) => add(k, v, acc))

let rec find = (px, x) =>
  switch x {
  | Empty => throw(Not_found)
  | Node(l, v, d, r, _) =>
    let c = compare(px, v)
    if c == 0 {
      d
    } else {
      find(
        px,
        if c < 0 {
          l
        } else {
          r
        },
      )
    }
  }

Mt.from_pair_suites(__MODULE__, list{("find", _ => Mt.Eq(find(10, m), 'a'))})

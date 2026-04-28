type rec t<'a> =
  | Empty
  | Node(t<'a>, 'a, t<'a>, int)
let rec height = x =>
  switch x {
  | Empty => 0
  | Node(_, _, _, h) => h
  }

let create = (l, v, r) => {
  let hl = height(l)
  let hr = height(r)
  Node(
    l,
    v,
    r,
    if hl >= hr {
      hl + 1
    } else {
      hr + 1
    },
  )
}

let bal = (l, v, r) => {
  let hl = height(l)
  let hr = height(r)
  if hl > hr + 2 {
    switch l {
    | Empty => Empty /* impossible */
    | Node(ll, lv, lr, _) =>
      if height(ll) >= height(lr) {
        create(ll, lv, create(lr, v, r))
      } else {
        switch lr {
        | Empty => Empty /* impossible */
        | Node(lrl, lrv, lrr, _) => create(create(ll, lv, lrl), lrv, create(lrr, v, r))
        }
      }
    }
  } else if hr > hl + 2 {
    switch r {
    | Empty => Empty /* impossible */
    | Node(rl, rv, rr, _) =>
      if height(rr) >= height(rl) {
        create(create(l, v, rl), rv, rr)
      } else {
        switch rl {
        | Empty => Empty /* impossible */
        | Node(rll, rlv, rlr, _) => create(create(l, v, rll), rlv, create(rlr, rv, rr))
        }
      }
    }
  } else {
    Node(
      l,
      v,
      r,
      if hl >= hr {
        hl + 1
      } else {
        hr + 1
      },
    )
  }
}

let compare_int = (x: int, y) =>
  if x > y {
    1
  } else if x == y {
    0
  } else {
    -1
  }

let rec add = (x, x_) =>
  switch x_ {
  | Empty => Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t =>
    let c = compare_int(x, v)
    if c == 0 {
      t
    } else if c < 0 {
      bal(add(x, l), v, r)
    } else {
      bal(l, v, add(x, r))
    }
  }

let rec min_elt = (def, x) =>
  switch x {
  | Empty => def
  | Node(Empty, v, r, _) => v
  | Node(l, v, r, _) => min_elt(v, l)
  }

/*
let rec remove_min_elt tree = match tree with  
    | Empty -> Empty (* impossible *)
    | Node(Empty, v, r, _) -> r
    | Node(l, v, r, _) -> bal (remove_min_elt l) v r
*/
let rec remove_min_elt = (l, v, r) =>
  switch l {
  | Empty => r
  | Node(ll, lv, lr, _) => bal(remove_min_elt(ll, lv, lr), v, r)
  }

let internal_merge = (l, r) =>
  switch (l, r) {
  | (Empty, t) => t
  | (t, Empty) => t
  | (_, Node(rl, rv, rr, _)) => bal(l, min_elt(rv, r), remove_min_elt(rl, rv, rr))
  }

let rec remove = (x, tree) =>
  switch tree {
  | Empty => Empty
  | Node(l, v, r, _) =>
    let c = compare_int(x, v)
    if c == 0 {
      internal_merge(l, r)
    } else if c < 0 {
      bal(remove(x, l), v, r)
    } else {
      bal(l, v, remove(x, r))
    }
  }

let rec mem = (x, x_) =>
  switch x_ {
  | Empty => false
  | Node(l, v, r, _) =>
    let c = compare_int(x, v)
    c == 0 ||
      mem(
        x,
        if c < 0 {
          l
        } else {
          r
        },
      )
  }

let () = {
  let v = ref(Empty)
  let iter = 1_00_000
  for i in 0 to iter {
    v := add(i, v.contents)
  }

  for i in 0 to iter {
    if !mem(i, v.contents) {
      Console.log("impossible")
    }
  }
  for i in 0 to iter {
    v := remove(i, v.contents)
  }
  switch v.contents {
  | Empty => ()
  | Node(_) => Console.log("impossible")
  }
}

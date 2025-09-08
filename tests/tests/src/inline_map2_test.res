/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the GNU Library General Public License, with */
/* the special exception on linking described in file ../LICENSE. */
/*  */
/* ********************************************************************* */

module type OrderedType = {
  type t
  let compare: (t, t) => int
}

module type S = {
  type key
  type t<+'a>
  let empty: t<'a>
  let is_empty: t<'a> => bool
  let mem: (key, t<'a>) => bool
  let add: (key, 'a, t<'a>) => t<'a>
  let singleton: (key, 'a) => t<'a>
  let remove: (key, t<'a>) => t<'a>
  let merge: ((key, option<'a>, option<'b>) => option<'c>, t<'a>, t<'b>) => t<'c>
  let compare: (('a, 'a) => int, t<'a>, t<'a>) => int
  let equal: (('a, 'a) => bool, t<'a>, t<'a>) => bool
  let iter: ((key, 'a) => unit, t<'a>) => unit
  let fold: ((key, 'a, 'b) => 'b, t<'a>, 'b) => 'b
  let for_all: ((key, 'a) => bool, t<'a>) => bool
  let exists: ((key, 'a) => bool, t<'a>) => bool
  let filter: ((key, 'a) => bool, t<'a>) => t<'a>
  let partition: ((key, 'a) => bool, t<'a>) => (t<'a>, t<'a>)
  let cardinal: t<'a> => int
  let bindings: t<'a> => list<(key, 'a)>
  let min_binding: t<'a> => (key, 'a)
  let max_binding: t<'a> => (key, 'a)
  let choose: t<'a> => (key, 'a)
  let split: (key, t<'a>) => (t<'a>, option<'a>, t<'a>)
  let find: (key, t<'a>) => 'a
  let map: ('a => 'b, t<'a>) => t<'b>
  let mapi: ((key, 'a) => 'b, t<'a>) => t<'b>
}

module Make = (Ord: OrderedType) => {
  type key = Ord.t

  type rec t<'a> =
    | Empty
    | Node(t<'a>, key, 'a, t<'a>, int)

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

  let singleton = (x, d) => Node(Empty, x, d, Empty, 1)

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

  let empty = Empty

  let is_empty = x =>
    switch x {
    | Empty => true
    | _ => false
    }

  let rec add = (x, data, x_) =>
    switch x_ {
    | Empty => Node(Empty, x, data, Empty, 1)
    | Node(l, v, d, r, h) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        Node(l, x, data, r, h)
      } else if c < 0 {
        bal(add(x, data, l), v, d, r)
      } else {
        bal(l, v, d, add(x, data, r))
      }
    }

  let rec find = (x, x_) =>
    switch x_ {
    | Empty => throw(Not_found)
    | Node(l, v, d, r, _) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        d
      } else {
        find(
          x,
          if c < 0 {
            l
          } else {
            r
          },
        )
      }
    }

  let rec mem = (x, x_) =>
    switch x_ {
    | Empty => false
    | Node(l, v, d, r, _) =>
      let c = Ord.compare(x, v)
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

  let rec min_binding = x =>
    switch x {
    | Empty => throw(Not_found)
    | Node(Empty, x, d, r, _) => (x, d)
    | Node(l, x, d, r, _) => min_binding(l)
    }

  let rec max_binding = x =>
    switch x {
    | Empty => throw(Not_found)
    | Node(l, x, d, Empty, _) => (x, d)
    | Node(l, x, d, r, _) => max_binding(r)
    }

  let rec remove_min_binding = x =>
    switch x {
    | Empty => invalid_arg("Map.remove_min_elt")
    | Node(Empty, x, d, r, _) => r
    | Node(l, x, d, r, _) => bal(remove_min_binding(l), x, d, r)
    }

  let merge = (t1, t2) =>
    switch (t1, t2) {
    | (Empty, t) => t
    | (t, Empty) => t
    | (_, _) =>
      let (x, d) = min_binding(t2)
      bal(t1, x, d, remove_min_binding(t2))
    }

  let rec remove = (x, x_) =>
    switch x_ {
    | Empty => Empty
    | Node(l, v, d, r, h) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        merge(l, r)
      } else if c < 0 {
        bal(remove(x, l), v, d, r)
      } else {
        bal(l, v, d, remove(x, r))
      }
    }

  let rec iter = (f, x) =>
    switch x {
    | Empty => ()
    | Node(l, v, d, r, _) =>
      iter(f, l)
      f(v, d)
      iter(f, r)
    }

  let rec map = (f, x) =>
    switch x {
    | Empty => Empty
    | Node(l, v, d, r, h) =>
      let l' = map(f, l)
      let d' = f(d)
      let r' = map(f, r)
      Node(l', v, d', r', h)
    }

  let rec mapi = (f, x) =>
    switch x {
    | Empty => Empty
    | Node(l, v, d, r, h) =>
      let l' = mapi(f, l)
      let d' = f(v, d)
      let r' = mapi(f, r)
      Node(l', v, d', r', h)
    }

  let rec fold = (f, m, accu) =>
    switch m {
    | Empty => accu
    | Node(l, v, d, r, _) => fold(f, r, f(v, d, fold(f, l, accu)))
    }

  let rec for_all = (p, x) =>
    switch x {
    | Empty => true
    | Node(l, v, d, r, _) => p(v, d) && (for_all(p, l) && for_all(p, r))
    }

  let rec exists = (p, x) =>
    switch x {
    | Empty => false
    | Node(l, v, d, r, _) => p(v, d) || (exists(p, l) || exists(p, r))
    }

  /* Beware: those two functions assume that the added k is *strictly*
       smaller (or bigger) than all the present keys in the tree; it
       does not test for equality with the current min (or max) key.

       Indeed, they are only used during the "join" operation which
       respects this precondition.
 */

  let rec add_min_binding = (k, v, x) =>
    switch x {
    | Empty => singleton(k, v)
    | Node(l, x, d, r, h) => bal(add_min_binding(k, v, l), x, d, r)
    }

  let rec add_max_binding = (k, v, x) =>
    switch x {
    | Empty => singleton(k, v)
    | Node(l, x, d, r, h) => bal(l, x, d, add_max_binding(k, v, r))
    }

  /* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. */

  let rec join = (l, v, d, r) =>
    switch (l, r) {
    | (Empty, _) => add_min_binding(v, d, r)
    | (_, Empty) => add_max_binding(v, d, l)
    | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) =>
      if lh > rh + 2 {
        bal(ll, lv, ld, join(lr, v, d, r))
      } else if rh > lh + 2 {
        bal(join(l, v, d, rl), rv, rd, rr)
      } else {
        create(l, v, d, r)
      }
    }

  /* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. */

  let concat = (t1, t2) =>
    switch (t1, t2) {
    | (Empty, t) => t
    | (t, Empty) => t
    | (_, _) =>
      let (x, d) = min_binding(t2)
      join(t1, x, d, remove_min_binding(t2))
    }

  let concat_or_join = (t1, v, d, t2) =>
    switch d {
    | Some(d) => join(t1, v, d, t2)
    | None => concat(t1, t2)
    }

  let rec split = (x, x_) =>
    switch x_ {
    | Empty => (Empty, None, Empty)
    | Node(l, v, d, r, _) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        (l, Some(d), r)
      } else if c < 0 {
        let (ll, pres, rl) = split(x, l)
        (ll, pres, join(rl, v, d, r))
      } else {
        let (lr, pres, rr) = split(x, r)
        (join(l, v, d, lr), pres, rr)
      }
    }

  let rec merge = (f, s1, s2) =>
    switch (s1, s2) {
    | (Empty, Empty) => Empty
    | (Node(l1, v1, d1, r1, h1), _) if h1 >= height(s2) =>
      let (l2, d2, r2) = split(v1, s2)
      concat_or_join(merge(f, l1, l2), v1, f(v1, Some(d1), d2), merge(f, r1, r2))
    | (_, Node(l2, v2, d2, r2, h2)) =>
      let (l1, d1, r1) = split(v2, s1)
      concat_or_join(merge(f, l1, l2), v2, f(v2, d1, Some(d2)), merge(f, r1, r2))
    | _ => assert(false)
    }

  let rec filter = (p, x) =>
    switch x {
    | Empty => Empty
    | Node(l, v, d, r, _) =>
      /* call [p] in the expected left-to-right order */
      let l' = filter(p, l)
      let pvd = p(v, d)
      let r' = filter(p, r)
      if pvd {
        join(l', v, d, r')
      } else {
        concat(l', r')
      }
    }

  let rec partition = (p, x) =>
    switch x {
    | Empty => (Empty, Empty)
    | Node(l, v, d, r, _) =>
      /* call [p] in the expected left-to-right order */
      let (lt, lf) = partition(p, l)
      let pvd = p(v, d)
      let (rt, rf) = partition(p, r)
      if pvd {
        (join(lt, v, d, rt), concat(lf, rf))
      } else {
        (concat(lt, rt), join(lf, v, d, rf))
      }
    }

  type rec enumeration<'a> = End | More(key, 'a, t<'a>, enumeration<'a>)

  let rec cons_enum = (m, e) =>
    switch m {
    | Empty => e
    | Node(l, v, d, r, _) => cons_enum(l, More(v, d, r, e))
    }

  let compare = (cmp, m1, m2) => {
    let rec compare_aux = (e1, e2) =>
      switch (e1, e2) {
      | (End, End) => 0
      | (End, _) => -1
      | (_, End) => 1
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) =>
        let c = Ord.compare(v1, v2)
        if c != 0 {
          c
        } else {
          let c = cmp(d1, d2)
          if c != 0 {
            c
          } else {
            compare_aux(cons_enum(r1, e1), cons_enum(r2, e2))
          }
        }
      }
    compare_aux(cons_enum(m1, End), cons_enum(m2, End))
  }

  let equal = (cmp, m1, m2) => {
    let rec equal_aux = (e1, e2) =>
      switch (e1, e2) {
      | (End, End) => true
      | (End, _) => false
      | (_, End) => false
      | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) =>
        Ord.compare(v1, v2) == 0 && (cmp(d1, d2) && equal_aux(cons_enum(r1, e1), cons_enum(r2, e2)))
      }
    equal_aux(cons_enum(m1, End), cons_enum(m2, End))
  }

  let rec cardinal = x =>
    switch x {
    | Empty => 0
    | Node(l, _, _, r, _) => cardinal(l) + 1 + cardinal(r)
    }

  let rec bindings_aux = (accu, x) =>
    switch x {
    | Empty => accu
    | Node(l, v, d, r, _) => bindings_aux(list{(v, d), ...bindings_aux(accu, r)}, l)
    }

  let bindings = s => bindings_aux(list{}, s)

  let choose = min_binding
}

module IntMap = Make({
  type t = int
  let compare = (x: t, y) => compare(x, y)
})

let empty = IntMap.empty

let m = Belt.List.reduceReverse(list{(10, 'a'), (3, 'b'), (7, 'c'), (20, 'd')}, empty, (
  acc,
  (k, v),
) => IntMap.add(k, v, acc))

module SMap = Make({
  type t = string
  let compare = (x: t, y) => compare(x, y)
})

let s = Belt.List.reduceReverse(
  list{("10", 'a'), ("3", 'b'), ("7", 'c'), ("20", 'd')},
  SMap.empty,
  (acc, (k, v)) => SMap.add(k, v, acc),
)
@val("console.log") external log: 'a => unit = ""

Mt.from_pair_suites(
  __MODULE__,
  {
    open Mt
    list{
      ("assertion1", _ => Eq(IntMap.find(10, m), 'a')),
      ("assertion2", _ => Eq(SMap.find("10", s), 'a')),
    }
  },
)

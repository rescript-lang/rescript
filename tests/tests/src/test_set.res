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

/* Sets over ordered types */

module type OrderedType = {
  type t
  let compare: (t, t) => int
}

module type S = {
  type elt
  type t
  let empty: t
  let is_empty: t => bool
  let mem: (elt, t) => bool
  let add: (elt, t) => t
  let singleton: elt => t
  let remove: (elt, t) => t
  let union: (t, t) => t
  let inter: (t, t) => t
  let diff: (t, t) => t
  let compare: (t, t) => int
  let equal: (t, t) => bool
  let subset: (t, t) => bool
  let iter: (elt => unit, t) => unit
  let fold: ((elt, 'a) => 'a, t, 'a) => 'a
  let for_all: (elt => bool, t) => bool
  let exists: (elt => bool, t) => bool
  let filter: (elt => bool, t) => t
  let partition: (elt => bool, t) => (t, t)
  let cardinal: t => int
  let elements: t => list<elt>
  let min_elt: t => elt
  let max_elt: t => elt
  let choose: t => elt
  let split: (elt, t) => (t, bool, t)
  let find: (elt, t) => elt
  let of_list: list<elt> => t
}

module Make = (Ord: OrderedType) => {
  type elt = Ord.t
  type rec t = Empty | Node(t, elt, t, int)

  /* Sets are represented by balanced binary trees (the heights of the
   children differ by at most 2 */

  let height = x =>
    switch x {
    | Empty => 0
    | Node(_, _, _, h) => h
    }

  /* Creates a new node with left son l, value v and right son r.
       We must have all elements of l < v < all elements of r.
       l and r must be balanced and | height l - height r | <= 2.
       Inline expansion of height for better speed. */

  let create = (l, v, r) => {
    let hl = switch l {
    | Empty => 0
    | Node(_, _, _, h) => h
    }
    let hr = switch r {
    | Empty => 0
    | Node(_, _, _, h) => h
    }
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

  /* Same as create, but performs one step of rebalancing if necessary.
       Assumes l and r balanced and | height l - height r | <= 3.
       Inline expansion of create for better speed in the most frequent case
       where no rebalancing is required. */

  let bal = (l, v, r) => {
    let hl = switch l {
    | Empty => 0
    | Node(_, _, _, h) => h
    }
    let hr = switch r {
    | Empty => 0
    | Node(_, _, _, h) => h
    }
    if hl > hr + 2 {
      switch l {
      | Empty => invalid_arg("Set.bal")
      | Node(ll, lv, lr, _) =>
        if height(ll) >= height(lr) {
          create(ll, lv, create(lr, v, r))
        } else {
          switch lr {
          | Empty => invalid_arg("Set.bal")
          | Node(lrl, lrv, lrr, _) => create(create(ll, lv, lrl), lrv, create(lrr, v, r))
          }
        }
      }
    } else if hr > hl + 2 {
      switch r {
      | Empty => invalid_arg("Set.bal")
      | Node(rl, rv, rr, _) =>
        if height(rr) >= height(rl) {
          create(create(l, v, rl), rv, rr)
        } else {
          switch rl {
          | Empty => invalid_arg("Set.bal")
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

  /* Insertion of one element */

  let rec add = (x, x_) =>
    switch x_ {
    | Empty => Node(Empty, x, Empty, 1)
    | Node(l, v, r, _) as t =>
      let c = Ord.compare(x, v)
      if c == 0 {
        t
      } else if c < 0 {
        bal(add(x, l), v, r)
      } else {
        bal(l, v, add(x, r))
      }
    }

  let singleton = x => Node(Empty, x, Empty, 1)

  /* Beware: those two functions assume that the added v is *strictly*
       smaller (or bigger) than all the present elements in the tree; it
       does not test for equality with the current min (or max) element.
       Indeed, they are only used during the "join" operation which
       respects this precondition.
 */

  let rec add_min_element = (v, x) =>
    switch x {
    | Empty => singleton(v)
    | Node(l, x, r, h) => bal(add_min_element(v, l), x, r)
    }

  let rec add_max_element = (v, x) =>
    switch x {
    | Empty => singleton(v)
    | Node(l, x, r, h) => bal(l, x, add_max_element(v, r))
    }

  /* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. */

  let rec join = (l, v, r) =>
    switch (l, r) {
    | (Empty, _) => add_min_element(v, r)
    | (_, Empty) => add_max_element(v, l)
    | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) =>
      if lh > rh + 2 {
        bal(ll, lv, join(lr, v, r))
      } else if rh > lh + 2 {
        bal(join(l, v, rl), rv, rr)
      } else {
        create(l, v, r)
      }
    }

  /* Smallest and greatest element of a set */

  let rec min_elt = x =>
    switch x {
    | Empty => throw(Not_found)
    | Node(Empty, v, r, _) => v
    | Node(l, v, r, _) => min_elt(l)
    }

  let rec max_elt = x =>
    switch x {
    | Empty => throw(Not_found)
    | Node(l, v, Empty, _) => v
    | Node(l, v, r, _) => max_elt(r)
    }

  /* Remove the smallest element of the given set */

  let rec remove_min_elt = x =>
    switch x {
    | Empty => invalid_arg("Set.remove_min_elt")
    | Node(Empty, v, r, _) => r
    | Node(l, v, r, _) => bal(remove_min_elt(l), v, r)
    }

  /* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       Assume | height l - height r | <= 2. */

  let merge = (t1, t2) =>
    switch (t1, t2) {
    | (Empty, t) => t
    | (t, Empty) => t
    | (_, _) => bal(t1, min_elt(t2), remove_min_elt(t2))
    }

  /* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. */

  let concat = (t1, t2) =>
    switch (t1, t2) {
    | (Empty, t) => t
    | (t, Empty) => t
    | (_, _) => join(t1, min_elt(t2), remove_min_elt(t2))
    }

  /* Splitting.  split x s returns a triple (l, present, r) where
        - l is the set of elements of s that are < x
        - r is the set of elements of s that are > x
        - present is false if s contains no element equal to x,
          or true if s contains an element equal to x. */

  let rec split = (x, x_) =>
    switch x_ {
    | Empty => (Empty, false, Empty)
    | Node(l, v, r, _) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        (l, true, r)
      } else if c < 0 {
        let (ll, pres, rl) = split(x, l)
        (ll, pres, join(rl, v, r))
      } else {
        let (lr, pres, rr) = split(x, r)
        (join(l, v, lr), pres, rr)
      }
    }

  /* Implementation of the set operations */

  let empty = Empty

  let is_empty = x =>
    switch x {
    | Empty => true
    | _ => false
    }

  let rec mem = (x, x_) =>
    switch x_ {
    | Empty => false
    | Node(l, v, r, _) =>
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

  let rec remove = (x, x_) =>
    switch x_ {
    | Empty => Empty
    | Node(l, v, r, _) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        merge(l, r)
      } else if c < 0 {
        bal(remove(x, l), v, r)
      } else {
        bal(l, v, remove(x, r))
      }
    }

  let rec union = (s1, s2) =>
    switch (s1, s2) {
    | (Empty, t2) => t2
    | (t1, Empty) => t1
    | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) =>
      if h1 >= h2 {
        if h2 == 1 {
          add(v2, s1)
        } else {
          let (l2, _, r2) = split(v1, s2)
          join(union(l1, l2), v1, union(r1, r2))
        }
      } else if h1 == 1 {
        add(v1, s2)
      } else {
        let (l1, _, r1) = split(v2, s1)
        join(union(l1, l2), v2, union(r1, r2))
      }
    }

  let rec inter = (s1, s2) =>
    switch (s1, s2) {
    | (Empty, t2) => Empty
    | (t1, Empty) => Empty
    | (Node(l1, v1, r1, _), t2) =>
      switch split(v1, t2) {
      | (l2, false, r2) => concat(inter(l1, l2), inter(r1, r2))
      | (l2, true, r2) => join(inter(l1, l2), v1, inter(r1, r2))
      }
    }

  let rec diff = (s1, s2) =>
    switch (s1, s2) {
    | (Empty, t2) => Empty
    | (t1, Empty) => t1
    | (Node(l1, v1, r1, _), t2) =>
      switch split(v1, t2) {
      | (l2, false, r2) => join(diff(l1, l2), v1, diff(r1, r2))
      | (l2, true, r2) => concat(diff(l1, l2), diff(r1, r2))
      }
    }

  type rec enumeration = End | More(elt, t, enumeration)

  let rec cons_enum = (s, e) =>
    switch s {
    | Empty => e
    | Node(l, v, r, _) => cons_enum(l, More(v, r, e))
    }

  let rec compare_aux = (e1, e2) =>
    switch (e1, e2) {
    | (End, End) => 0
    | (End, _) => -1
    | (_, End) => 1
    | (More(v1, r1, e1), More(v2, r2, e2)) =>
      let c = Ord.compare(v1, v2)
      if c != 0 {
        c
      } else {
        compare_aux(cons_enum(r1, e1), cons_enum(r2, e2))
      }
    }

  let compare = (s1, s2) => compare_aux(cons_enum(s1, End), cons_enum(s2, End))

  let equal = (s1, s2) => compare(s1, s2) == 0

  let rec subset = (s1, s2) =>
    switch (s1, s2) {
    | (Empty, _) => true
    | (_, Empty) => false
    | (Node(l1, v1, r1, _), Node(l2, v2, r2, _) as t2) =>
      let c = Ord.compare(v1, v2)
      if c == 0 {
        subset(l1, l2) && subset(r1, r2)
      } else if c < 0 {
        subset(Node(l1, v1, Empty, 0), l2) && subset(r1, t2)
      } else {
        subset(Node(Empty, v1, r1, 0), r2) && subset(l1, t2)
      }
    }

  let rec iter = (f, x_) =>
    switch x_ {
    | Empty => ()
    | Node(l, v, r, _) =>
      iter(f, l)
      f(v)
      iter(f, r)
    }

  let rec fold = (f, s, accu) =>
    switch s {
    | Empty => accu
    | Node(l, v, r, _) => fold(f, r, f(v, fold(f, l, accu)))
    }

  let rec for_all = (p, x) =>
    switch x {
    | Empty => true
    | Node(l, v, r, _) => p(v) && (for_all(p, l) && for_all(p, r))
    }

  let rec exists = (p, x) =>
    switch x {
    | Empty => false
    | Node(l, v, r, _) => p(v) || (exists(p, l) || exists(p, r))
    }

  let rec filter = (p, x) =>
    switch x {
    | Empty => Empty
    | Node(l, v, r, _) =>
      /* call [p] in the expected left-to-right order */
      let l' = filter(p, l)
      let pv = p(v)
      let r' = filter(p, r)
      if pv {
        join(l', v, r')
      } else {
        concat(l', r')
      }
    }

  let rec partition = (p, x) =>
    switch x {
    | Empty => (Empty, Empty)
    | Node(l, v, r, _) =>
      /* call [p] in the expected left-to-right order */
      let (lt, lf) = partition(p, l)
      let pv = p(v)
      let (rt, rf) = partition(p, r)
      if pv {
        (join(lt, v, rt), concat(lf, rf))
      } else {
        (concat(lt, rt), join(lf, v, rf))
      }
    }

  let rec cardinal = x =>
    switch x {
    | Empty => 0
    | Node(l, v, r, _) => cardinal(l) + 1 + cardinal(r)
    }

  let rec elements_aux = (accu, x) =>
    switch x {
    | Empty => accu
    | Node(l, v, r, _) => elements_aux(list{v, ...elements_aux(accu, r)}, l)
    }

  let elements = s => elements_aux(list{}, s)

  let choose = min_elt

  let rec find = (x, x_) =>
    switch x_ {
    | Empty => throw(Not_found)
    | Node(l, v, r, _) =>
      let c = Ord.compare(x, v)
      if c == 0 {
        v
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

  let of_sorted_list = l => {
    let rec sub = (n, l) =>
      switch (n, l) {
      | (0, l) => (Empty, l)
      | (1, list{x0, ...l}) => (Node(Empty, x0, Empty, 1), l)
      | (2, list{x0, x1, ...l}) => (Node(Node(Empty, x0, Empty, 1), x1, Empty, 2), l)
      | (3, list{x0, x1, x2, ...l}) => (
          Node(Node(Empty, x0, Empty, 1), x1, Node(Empty, x2, Empty, 1), 2),
          l,
        )
      | (n, l) =>
        let nl = n / 2
        let (left, l) = sub(nl, l)
        switch l {
        | list{} => assert(false)
        | list{mid, ...l} =>
          let (right, l) = sub(n - nl - 1, l)
          (create(left, mid, right), l)
        }
      }

    fst(sub(Belt.List.length(l), l))
  }

  let of_list = l =>
    switch l {
    | list{} => empty
    | list{x0} => singleton(x0)
    | list{x0, x1} => add(x1, singleton(x0))
    | list{x0, x1, x2} => add(x2, add(x1, singleton(x0)))
    | list{x0, x1, x2, x3} => add(x3, add(x2, add(x1, singleton(x0))))
    | list{x0, x1, x2, x3, x4} => add(x4, add(x3, add(x2, add(x1, singleton(x0)))))
    | _ => of_sorted_list(l->Belt.List.sort(Ord.compare))
    }
}
module N = {
  let a = 3
}

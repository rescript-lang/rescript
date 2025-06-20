/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

module String = Ocaml_String

type elt = string
let compare_elt = String.compare
type t = Set_gen.t<elt>

let empty = Set_gen.empty
let is_empty = Set_gen.is_empty
let iter = Set_gen.iter
let fold = Set_gen.fold
let for_all = Set_gen.for_all
let exists = Set_gen.exists
let singleton = Set_gen.singleton
let cardinal = Set_gen.cardinal
let elements = Set_gen.elements
let min_elt = Set_gen.min_elt
let max_elt = Set_gen.max_elt
let choose = Set_gen.choose
let of_sorted_list = Set_gen.of_sorted_list
let of_sorted_array = Set_gen.of_sorted_array
let partition = Set_gen.partition
let filter = Set_gen.filter
let of_sorted_list = Set_gen.of_sorted_list
let of_sorted_array = Set_gen.of_sorted_array

let rec split = (x, tree: Set_gen.t<_>): (Set_gen.t<_>, bool, Set_gen.t<_>) =>
  switch tree {
  | Empty => (Empty, false, Empty)
  | Node(l, v, r, _) =>
    let c = compare_elt(x, v)
    if c == 0 {
      (l, true, r)
    } else if c < 0 {
      let (ll, pres, rl) = split(x, l)
      (ll, pres, Set_gen.internal_join(rl, v, r))
    } else {
      let (lr, pres, rr) = split(x, r)
      (Set_gen.internal_join(l, v, lr), pres, rr)
    }
  }
let rec add = (x, tree: Set_gen.t<_>): Set_gen.t<_> =>
  switch tree {
  | Empty => Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t =>
    let c = compare_elt(x, v)
    if c == 0 {
      t
    } else if c < 0 {
      Set_gen.internal_bal(add(x, l), v, r)
    } else {
      Set_gen.internal_bal(l, v, add(x, r))
    }
  }

let rec union = (s1: Set_gen.t<_>, s2: Set_gen.t<_>): Set_gen.t<_> =>
  switch (s1, s2) {
  | (Empty, t2) => t2
  | (t1, Empty) => t1
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) =>
    if h1 >= h2 {
      if h2 == 1 {
        add(v2, s1)
      } else {
        let (l2, _, r2) = split(v1, s2)
        Set_gen.internal_join(union(l1, l2), v1, union(r1, r2))
      }
    } else if h1 == 1 {
      add(v1, s2)
    } else {
      let (l1, _, r1) = split(v2, s1)
      Set_gen.internal_join(union(l1, l2), v2, union(r1, r2))
    }
  }

let rec inter = (s1: Set_gen.t<_>, s2: Set_gen.t<_>): Set_gen.t<_> =>
  switch (s1, s2) {
  | (Empty, t2) => Empty
  | (t1, Empty) => Empty
  | (Node(l1, v1, r1, _), t2) =>
    switch split(v1, t2) {
    | (l2, false, r2) => Set_gen.internal_concat(inter(l1, l2), inter(r1, r2))
    | (l2, true, r2) => Set_gen.internal_join(inter(l1, l2), v1, inter(r1, r2))
    }
  }

let rec diff = (s1: Set_gen.t<_>, s2: Set_gen.t<_>): Set_gen.t<_> =>
  switch (s1, s2) {
  | (Empty, t2) => Empty
  | (t1, Empty) => t1
  | (Node(l1, v1, r1, _), t2) =>
    switch split(v1, t2) {
    | (l2, false, r2) => Set_gen.internal_join(diff(l1, l2), v1, diff(r1, r2))
    | (l2, true, r2) => Set_gen.internal_concat(diff(l1, l2), diff(r1, r2))
    }
  }

let rec mem = (x, tree: Set_gen.t<_>) =>
  switch tree {
  | Empty => false
  | Node(l, v, r, _) =>
    let c = compare_elt(x, v)
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

let rec remove = (x, tree: Set_gen.t<_>): Set_gen.t<_> =>
  switch tree {
  | Empty => Empty
  | Node(l, v, r, _) =>
    let c = compare_elt(x, v)
    if c == 0 {
      Set_gen.internal_merge(l, r)
    } else if c < 0 {
      Set_gen.internal_bal(remove(x, l), v, r)
    } else {
      Set_gen.internal_bal(l, v, remove(x, r))
    }
  }

let compare = (s1, s2) => Set_gen.compare(compare_elt, s1, s2)

let equal = (s1, s2) => compare(s1, s2) == 0

let rec subset = (s1: Set_gen.t<_>, s2: Set_gen.t<_>) =>
  switch (s1, s2) {
  | (Empty, _) => true
  | (_, Empty) => false
  | (Node(l1, v1, r1, _), Node(l2, v2, r2, _) as t2) =>
    let c = compare_elt(v1, v2)
    if c == 0 {
      subset(l1, l2) && subset(r1, r2)
    } else if c < 0 {
      subset(Node(l1, v1, Empty, 0), l2) && subset(r1, t2)
    } else {
      subset(Node(Empty, v1, r1, 0), r2) && subset(l1, t2)
    }
  }

let rec find = (x, tree: Set_gen.t<_>) =>
  switch tree {
  | Empty => throw(Not_found)
  | Node(l, v, r, _) =>
    let c = compare_elt(x, v)
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

let of_list = l =>
  switch l {
  | list{} => empty
  | list{x0} => singleton(x0)
  | list{x0, x1} => add(x1, singleton(x0))
  | list{x0, x1, x2} => add(x2, add(x1, singleton(x0)))
  | list{x0, x1, x2, x3} => add(x3, add(x2, add(x1, singleton(x0))))
  | list{x0, x1, x2, x3, x4} => add(x4, add(x3, add(x2, add(x1, singleton(x0)))))
  | _ => of_sorted_list(l->Belt.List.sort(compare_elt))
  }

let of_array = l => l->Belt.Array.reduceReverse(empty, (acc, x) => add(x, acc))

/* also check order */
let invariant = t => {
  Set_gen.check(t)
  Set_gen.is_ordered(compare_elt, t)
}

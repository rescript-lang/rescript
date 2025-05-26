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

/***
  balanced tree based on stdlib distribution
*/

type rec t<'a> =
  | Empty
  | Node(t<'a>, 'a, t<'a>, int)

type rec enumeration<'a> = End | More('a, t<'a>, enumeration<'a>)

let rec cons_enum = (s, e) =>
  switch s {
  | Empty => e
  | Node(l, v, r, _) => cons_enum(l, More(v, r, e))
  }

let rec height = x =>
  switch x {
  | Empty => 0
  | Node(_, _, _, h) => h
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

let empty = Empty

let is_empty = x =>
  switch x {
  | Empty => true
  | _ => false
  }

let rec cardinal_aux = (acc, x) =>
  switch x {
  | Empty => acc
  | Node(l, _, r, _) => cardinal_aux(cardinal_aux(acc + 1, r), l)
  }

let cardinal = s => cardinal_aux(0, s)

let rec elements_aux = (accu, x) =>
  switch x {
  | Empty => accu
  | Node(l, v, r, _) => elements_aux(list{v, ...elements_aux(accu, r)}, l)
  }

let elements = s => elements_aux(list{}, s)

let choose = min_elt

let rec iter = (f, x) =>
  switch x {
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

let max_int3 = (a: int, b, c) =>
  if a >= b {
    if a >= c {
      a
    } else {
      c
    }
  } else if b >= c {
    b
  } else {
    c
  }
let max_int_2 = (a: int, b) =>
  if a >= b {
    a
  } else {
    b
  }

exception Height_invariant_broken
exception Height_diff_borken

let rec check_height_and_diff = x =>
  switch x {
  | Empty => 0
  | Node(l, _, r, h) =>
    let hl = check_height_and_diff(l)
    let hr = check_height_and_diff(r)
    if h != max_int_2(hl, hr) + 1 {
      throw(Height_invariant_broken)
    } else {
      let diff = abs(hl - hr)
      if diff > 2 {
        throw(Height_diff_borken)
      } else {
        h
      }
    }
  }

let check = tree => ignore(check_height_and_diff(tree))
/*
    Invariants: 
    1. {[ l < v < r]}
    2. l and r balanced 
    3. [height l] - [height r] <= 2
*/
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
    Invariants:
    1. {[ l < v < r ]}
    2. l and r balanced 
    3. | height l - height r | <= 3.

    Proof by indunction

    Lemma: the height of  [bal l v r] will bounded by [max l r] + 1 
*/
/*
let internal_bal l v r =
  match l with
  | Empty ->
    begin match r with 
      | Empty -> Node(Empty,v,Empty,1)
      | Node(rl,rv,rr,hr) -> 
        if hr > 2 then
          begin match rl with
            | Empty -> create (* create l v rl *) (Node (Empty,v,Empty,1)) rv rr 
            | Node(rll,rlv,rlr,hrl) -> 
              let hrr = height rr in 
              if hrr >= hrl then 
                Node  
                  ((Node (Empty,v,rl,hrl+1))(* create l v rl *),
                   rv, rr, if hrr = hrl then hrr + 2 else hrr + 1) 
              else 
                let hrll = height rll in 
                let hrlr = height rlr in 
                create
                  (Node(Empty,v,rll,hrll + 1)) 
                  (* create l v rll *) 
                  rlv 
                  (Node (rlr,rv,rr, if hrlr > hrr then hrlr + 1 else hrr + 1))
                  (* create rlr rv rr *)    
          end 
        else Node (l,v,r, hr + 1)  
    end
  | Node(ll,lv,lr,hl) ->
    begin match r with 
      | Empty ->
        if hl > 2 then 
          (*if height ll >= height lr then create ll lv (create lr v r)
            else*)
          begin match lr with 
            | Empty -> 
              create ll lv (Node (Empty,v,Empty, 1)) 
            (* create lr v r *)  
            | Node(lrl,lrv,lrr,hlr) -> 
              if height ll >= hlr then 
                create ll lv
                  (Node(lr,v,Empty,hlr+1)) 
                  (*create lr v r*)
              else 
                let hlrr = height lrr in  
                create 
                  (create ll lv lrl)
                  lrv
                  (Node(lrr,v,Empty,hlrr + 1)) 
                  (*create lrr v r*)
          end 
        else Node(l,v,r, hl+1)    
      | Node(rl,rv,rr,hr) ->
        if hl > hr + 2 then           
          begin match lr with 
            | Empty ->   create ll lv (create lr v r)
            | Node(lrl,lrv,lrr,_) ->
              if height ll >= height lr then create ll lv (create lr v r)
              else 
                create (create ll lv lrl) lrv (create lrr v r)
          end 
        else
        if hr > hl + 2 then             
          begin match rl with 
            | Empty ->
              let hrr = height rr in   
              Node(
                (Node (l,v,Empty,hl + 1))
                (*create l v rl*)
                ,
                rv,
                rr,
                if hrr > hr then hrr + 1 else hl + 2 
              )
            | Node(rll,rlv,rlr,_) ->
              let hrr = height rr in 
              let hrl = height rl in 
              if hrr >= hrl then create (create l v rl) rv rr else 
                create (create l v rll) rlv (create rlr rv rr)
          end
        else  
          Node(l,v,r, if hl >= hr then hl+1 else hr + 1)
    end
*/
let internal_bal = (l, v, r) => {
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
    | Empty => assert(false)
    | Node(ll, lv, lr, _) =>
      if height(ll) >= height(lr) {
        /* [ll] >~ [lr] 
           [ll] >~ [r] 
           [ll] ~~ [ lr ^ r]  
 */
        create(ll, lv, create(lr, v, r))
      } else {
        switch lr {
        | Empty => assert(false)
        | Node(lrl, lrv, lrr, _) =>
          /* [lr] >~ [ll]
             [lr] >~ [r]
             [ll ^ lrl] ~~ [lrr ^ r]   
 */
          create(create(ll, lv, lrl), lrv, create(lrr, v, r))
        }
      }
    }
  } else if hr > hl + 2 {
    switch r {
    | Empty => assert(false)
    | Node(rl, rv, rr, _) =>
      if height(rr) >= height(rl) {
        create(create(l, v, rl), rv, rr)
      } else {
        switch rl {
        | Empty => assert(false)
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

let rec remove_min_elt = x =>
  switch x {
  | Empty => invalid_arg("Set.remove_min_elt")
  | Node(Empty, v, r, _) => r
  | Node(l, v, r, _) => internal_bal(remove_min_elt(l), v, r)
  }

let singleton = x => Node(Empty, x, Empty, 1)

/*
   All elements of l must precede the elements of r.
       Assume | height l - height r | <= 2.
   weak form of [concat] 
*/

let internal_merge = (l, r) =>
  switch (l, r) {
  | (Empty, t) => t
  | (t, Empty) => t
  | (_, _) => internal_bal(l, min_elt(r), remove_min_elt(r))
  }

/* Beware: those two functions assume that the added v is *strictly*
    smaller (or bigger) than all the present elements in the tree; it
    does not test for equality with the current min (or max) element.
    Indeed, they are only used during the "join" operation which
    respects this precondition.
*/

let rec add_min_element = (v, x) =>
  switch x {
  | Empty => singleton(v)
  | Node(l, x, r, h) => internal_bal(add_min_element(v, l), x, r)
  }

let rec add_max_element = (v, x) =>
  switch x {
  | Empty => singleton(v)
  | Node(l, x, r, h) => internal_bal(l, x, add_max_element(v, r))
  }

/** 
    Invariants:
    1. l < v < r 
    2. l and r are balanced 

    Proof by induction
    The height of output will be ~~ (max (height l) (height r) + 2)
    Also use the lemma from [bal]
*/
let rec internal_join = (l, v, r) =>
  switch (l, r) {
  | (Empty, _) => add_min_element(v, r)
  | (_, Empty) => add_max_element(v, l)
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) =>
    if lh > rh + 2 {
      /* proof by induction:
         now [height of ll] is [lh - 1] 
 */
      internal_bal(ll, lv, internal_join(lr, v, r))
    } else if rh > lh + 2 {
      internal_bal(internal_join(l, v, rl), rv, rr)
    } else {
      create(l, v, r)
    }
  }

/*
    Required Invariants: 
    [t1] < [t2]  
*/
let internal_concat = (t1, t2) =>
  switch (t1, t2) {
  | (Empty, t) => t
  | (t, Empty) => t
  | (_, _) => internal_join(t1, min_elt(t2), remove_min_elt(t2))
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
      internal_join(l', v, r')
    } else {
      internal_concat(l', r')
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
      (internal_join(lt, v, rt), internal_concat(lf, rf))
    } else {
      (internal_concat(lt, rt), internal_join(lf, v, rf))
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

let of_sorted_array = l => {
  let rec sub = (start, n, l) =>
    if n == 0 {
      Empty
    } else if n == 1 {
      let x0 = Belt.Array.getUnsafe(l, start)
      Node(Empty, x0, Empty, 1)
    } else if n == 2 {
      let x0 = Belt.Array.getUnsafe(l, start)
      let x1 = Belt.Array.getUnsafe(l, start + 1)
      Node(Node(Empty, x0, Empty, 1), x1, Empty, 2)
    } else if n == 3 {
      let x0 = Belt.Array.getUnsafe(l, start)
      let x1 = Belt.Array.getUnsafe(l, start + 1)
      let x2 = Belt.Array.getUnsafe(l, start + 2)
      Node(Node(Empty, x0, Empty, 1), x1, Node(Empty, x2, Empty, 1), 2)
    } else {
      let nl = n / 2
      let left = sub(start, nl, l)
      let mid = start + nl
      let v = Belt.Array.getUnsafe(l, mid)
      let right = sub(mid + 1, n - nl - 1, l)
      create(left, v, right)
    }

  sub(0, Belt.Array.length(l), l)
}

let is_ordered = (cmp, tree) => {
  let rec is_ordered_min_max = tree =>
    switch tree {
    | Empty => #Empty
    | Node(l, v, r, _) =>
      switch is_ordered_min_max(l) {
      | #No => #No
      | #Empty =>
        switch is_ordered_min_max(r) {
        | #No => #No
        | #Empty => #V(v, v)
        | #V(l, r) =>
          if cmp(v, l) < 0 {
            #V(v, r)
          } else {
            #No
          }
        }
      | #V(min_v, max_v) =>
        switch is_ordered_min_max(r) {
        | #No => #No
        | #Empty =>
          if cmp(max_v, v) < 0 {
            #V(min_v, v)
          } else {
            #No
          }
        | #V(min_v_r, max_v_r) =>
          if cmp(max_v, min_v_r) < 0 {
            #V(min_v, max_v_r)
          } else {
            #No
          }
        }
      }
    }
  is_ordered_min_max(tree) != #No
}

let invariant = (cmp, t) => {
  check(t)
  is_ordered(cmp, t)
}

let rec compare_aux = (cmp, e1, e2) =>
  switch (e1, e2) {
  | (End, End) => 0
  | (End, _) => -1
  | (_, End) => 1
  | (More(v1, r1, e1), More(v2, r2, e2)) =>
    let c = cmp(v1, v2)
    if c != 0 {
      c
    } else {
      compare_aux(cmp, cons_enum(r1, e1), cons_enum(r2, e2))
    }
  }

let compare = (cmp, s1, s2) => compare_aux(cmp, cons_enum(s1, End), cons_enum(s2, End))

module type S = {
  type elt
  type t
  let empty: t
  let is_empty: t => bool
  let iter: (elt => unit, t) => unit
  let fold: ((elt, 'a) => 'a, t, 'a) => 'a
  let for_all: (elt => bool, t) => bool
  let exists: (elt => bool, t) => bool
  let singleton: elt => t
  let cardinal: t => int
  let elements: t => list<elt>
  let min_elt: t => elt
  let max_elt: t => elt
  let choose: t => elt
  let of_sorted_list: list<elt> => t
  let of_sorted_array: array<elt> => t
  let partition: (elt => bool, t) => (t, t)

  let mem: (elt, t) => bool
  let add: (elt, t) => t
  let remove: (elt, t) => t
  let union: (t, t) => t
  let inter: (t, t) => t
  let diff: (t, t) => t
  let compare: (t, t) => int
  let equal: (t, t) => bool
  let subset: (t, t) => bool
  let filter: (elt => bool, t) => t

  let split: (elt, t) => (t, bool, t)
  let find: (elt, t) => elt
  let of_list: list<elt> => t
  let of_sorted_list: list<elt> => t
  let of_sorted_array: array<elt> => t
  let invariant: t => bool
}

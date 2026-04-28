/* ********************************************************************* */
/*  */
/* Objective Caml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the Q Public License version 1.0. */
/*  */
/* ********************************************************************* */

/* $Id: bdd.ml 7017 2005-08-12 09:22:04Z xleroy $ */

/* Translated to Caml by Xavier Leroy */
/* Original code written in SML by ... */

type rec bdd = One | Zero | Node(bdd, int, int, bdd)

let rec eval = (bdd, vars) =>
  switch bdd {
  | Zero => false
  | One => true
  | Node(l, v, _, h) =>
    if vars->Array.getUnsafe(v) {
      eval(h, vars)
    } else {
      eval(l, vars)
    }
  }

let getId = bdd =>
  switch bdd {
  | Node(_, _, id, _) => id
  | Zero => 0
  | One => 1
  }

let initSize_1 = 8 * 1024 - 1
let nodeC = ref(1)
let sz_1 = ref(initSize_1)
let htab = ref(Belt.Array.make(sz_1.contents + 1, list{}))
let n_items = ref(0)
let hashVal = (x, y, v) => lsl(x, 1) + y + lsl(v, 2)

let resize = newSize => {
  let arr = htab.contents
  let newSz_1 = newSize - 1
  let newArr = Belt.Array.make(newSize, list{})
  let rec copyBucket = bucket =>
    switch bucket {
    | list{} => ()
    | list{n, ...ns} =>
      switch n {
      | Node(l, v, _, h) =>
        let ind = land(hashVal(getId(l), getId(h), v), newSz_1)

        newArr->Array.setUnsafe(ind, list{n, ...newArr->Array.getUnsafe(ind)})
        copyBucket(ns)
      | _ => assert(false)
      }
    }

  for n in 0 to sz_1.contents {
    copyBucket(arr->Array.getUnsafe(n))
  }
  htab := newArr
  sz_1 := newSz_1
}

let rec insert = (idl, idh, v, ind, bucket, newNode) =>
  if n_items.contents <= sz_1.contents {
    htab.contents->Array.setUnsafe(ind, list{newNode, ...bucket})
    incr(n_items)
  } else {
    resize(sz_1.contents + sz_1.contents + 2)
    let ind = land(hashVal(idl, idh, v), sz_1.contents)

    htab.contents->Array.setUnsafe(ind, list{newNode, ...htab.contents->Array.getUnsafe(ind)})
  }

let resetUnique = () => {
  sz_1 := initSize_1
  htab := Belt.Array.make(sz_1.contents + 1, list{})
  n_items := 0
  nodeC := 1
}

let mkNode = (low, v, high) => {
  let idl = getId(low)
  let idh = getId(high)

  if idl == idh {
    low
  } else {
    let ind = land(hashVal(idl, idh, v), sz_1.contents)
    let bucket = htab.contents->Array.getUnsafe(ind)
    let rec lookup = b =>
      switch b {
      | list{} =>
        let n = Node(
          low,
          v,
          {
            incr(nodeC)
            nodeC.contents
          },
          high,
        )

        insert(getId(low), getId(high), v, ind, bucket, n)
        n
      | list{n, ...ns} =>
        switch n {
        | Node(l, v', id, h) =>
          if v == v' && (idl == getId(l) && idh == getId(h)) {
            n
          } else {
            lookup(ns)
          }
        | _ => assert(false)
        }
      }

    lookup(bucket)
  }
}

type ordering = LESS | EQUAL | GREATER

let cmpVar = (x: int, y: int) =>
  if x < y {
    LESS
  } else if x > y {
    GREATER
  } else {
    EQUAL
  }

let zero = Zero
let one = One

let mkVar = x => mkNode(zero, x, one)

let cacheSize = 1999

let andslot1 = Belt.Array.make(cacheSize, 0)
let andslot2 = Belt.Array.make(cacheSize, 0)
let andslot3 = Belt.Array.make(cacheSize, zero)
let xorslot1 = Belt.Array.make(cacheSize, 0)
let xorslot2 = Belt.Array.make(cacheSize, 0)
let xorslot3 = Belt.Array.make(cacheSize, zero)
let notslot1 = Belt.Array.make(cacheSize, 0)
let notslot2 = Belt.Array.make(cacheSize, one)
let hash = (x, y) => mod(lsl(x, 1) + y, cacheSize)

let rec not = n =>
  switch n {
  | Zero => One
  | One => Zero
  | Node(l, v, id, r) =>
    let h = mod(id, cacheSize)

    if id == notslot1->Array.getUnsafe(h) {
      notslot2->Array.getUnsafe(h)
    } else {
      let f = mkNode(!l, v, !r)

      notslot1->Array.setUnsafe(h, id)
      notslot2->Array.setUnsafe(h, f)
      f
    }
  }

let rec and2 = (n1, n2) =>
  switch n1 {
  | Node(l1, v1, i1, r1) =>
    switch n2 {
    | Node(l2, v2, i2, r2) =>
      let h = hash(i1, i2)

      if i1 == andslot1->Array.getUnsafe(h) && i2 == andslot2->Array.getUnsafe(h) {
        andslot3->Array.getUnsafe(h)
      } else {
        let f = switch cmpVar(v1, v2) {
        | EQUAL => mkNode(and2(l1, l2), v1, and2(r1, r2))
        | LESS => mkNode(and2(l1, n2), v1, and2(r1, n2))
        | GREATER => mkNode(and2(n1, l2), v2, and2(n1, r2))
        }

        andslot1->Array.setUnsafe(h, i1)
        andslot2->Array.setUnsafe(h, i2)
        andslot3->Array.setUnsafe(h, f)
        f
      }
    | Zero => Zero
    | One => n1
    }
  | Zero => Zero
  | One => n2
  }

let rec xor = (n1, n2) =>
  switch n1 {
  | Node(l1, v1, i1, r1) =>
    switch n2 {
    | Node(l2, v2, i2, r2) =>
      let h = hash(i1, i2)

      if i1 == andslot1->Array.getUnsafe(h) && i2 == andslot2->Array.getUnsafe(h) {
        andslot3->Array.getUnsafe(h)
      } else {
        let f = switch cmpVar(v1, v2) {
        | EQUAL => mkNode(xor(l1, l2), v1, xor(r1, r2))
        | LESS => mkNode(xor(l1, n2), v1, xor(r1, n2))
        | GREATER => mkNode(xor(n1, l2), v2, xor(n1, r2))
        }

        andslot1->Array.setUnsafe(h, i1)
        andslot2->Array.setUnsafe(h, i2)
        andslot3->Array.setUnsafe(h, f)
        f
      }
    | Zero => n1
    | One => !n1
    }
  | Zero => n2
  | One => !n2
  }

let hwb = n => {
  let rec h = (i, j) =>
    if i == j {
      mkVar(i)
    } else {
      xor(and2(!mkVar(j), h(i, j - 1)), and2(mkVar(j), g(i, j - 1)))
    }
  and g = (i, j) =>
    if i == j {
      mkVar(i)
    } else {
      xor(and2(!mkVar(i), h(i + 1, j)), and2(mkVar(i), g(i + 1, j)))
    }

  h(0, n - 1)
}

/* Testing */
let seed = ref(0)

let random = () => {
  seed := seed.contents * 25173 + 17431
  land(seed.contents, 1) > 0
}

let random_vars = n => {
  let vars = Belt.Array.make(n, false)
  for i in 0 to n - 1 {
    vars->Array.setUnsafe(i, random())
  }
  vars
}

let bool_equal = (a, b) =>
  switch (a, b) {
  | (true, true)
  | (false, false) => true
  | _ => false
  }

let test_hwb = (bdd, vars) => {
  /* We should have
        eval bdd vars = vars.(n-1) if n > 0
        eval bdd vars = false if n = 0
     where n is the number of "true" elements in vars. */
  let ntrue = ref(0)
  for i in 0 to Belt.Array.length(vars) - 1 {
    if vars->Array.getUnsafe(i) {
      incr(ntrue)
    }
  }
  bool_equal(
    eval(bdd, vars),
    if ntrue.contents > 0 {
      vars->Array.getUnsafe(ntrue.contents - 1)
    } else {
      false
    },
  )
}

let main = () => {
  let n = 22
  /* if Array.length Sys.argv >= 2 then int_of_string Sys.argv.(1) else 22 in */
  let ntests = 100
  /* if Array.length Sys.argv >= 3 then int_of_string Sys.argv.(2) else 100 in */
  let bdd = hwb(n)
  let succeeded = ref(true)
  for i in 1 to ntests {
    succeeded := succeeded.contents && test_hwb(bdd, random_vars(n))
  }
  assert(succeeded.contents)
}

let _ = main()

/* local variables: */
/* compile-command: "../../ocaml/bin/ocamlc.opt -o bdd.byte bdd.ml" */
/* end: */

/* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

/*
  perf is not everything, there are better memory represenations

  ```
  type 'a cell = {
    mutable head : 'a;
    mutable tail : 'a opt_cell
  }

  and 'a opt_cell = 'a cell Js.null

  and 'a t = {
    length : int ;
    data : 'a opt_cell
  }
  ```
  However,
  - people use List not because of its perf, but its
    convenience, in that case, pattern match and compatibility seems
    more attractive, we could keep a mutable list
  - The built in types would indicate that
    its construtor is immutable, a better optimizer would break such code

  ```
  type 'a t = {
    head : 'a;
    mutable tail : 'a t | int
  }
  ```
  In the future, we could come up with a safer version
  ```
  type 'a t =
  | Nil
  | Cons of { hd : 'a ; mutable tail : 'a t }
  ```
*/

@@config({flags: ["-bs-noassertfalse"]})

type t<'a> = list<'a>

module A = {
  @new external makeUninitializedUnsafe: int => array<'a> = "Array"
  external min: ('a, 'a) => 'a = "%bs_min"

  let reduceReverseU = (a, x, f) => {
    let r = ref(x)
    for i in Stdlib_Array.length(a) - 1 downto 0 {
      r.contents = f(r.contents, Stdlib_Array.getUnsafe(a, i))
    }
    r.contents
  }

  let reduceReverse2U = (a, b, x, f) => {
    let r = ref(x)
    let len = min(Stdlib_Array.length(a), Stdlib_Array.length(b))
    for i in len - 1 downto 0 {
      r.contents = f(r.contents, Stdlib_Array.getUnsafe(a, i), Stdlib_Array.getUnsafe(b, i))
    }
    r.contents
  }
}

external mutableCell: ('a, list<'a>) => list<'a> = "#makemutablelist"

/*
  `mutableCell x []` == `x`
  but tell the compiler that is a mutable cell, so it wont
  be mis-inlined in the future
   dont inline a binding to mutable cell, it is mutable
*/
/* INVARIANT: relies on Literals.tl (internal representation) */
@set external unsafeMutateTail: (list<'a>, list<'a>) => unit = "tl"

/*
   - the cell is not empty
*/

let head = x =>
  switch x {
  | list{} => None
  | list{x, ..._} => Some(x)
  }

let headExn = x =>
  switch x {
  | list{} => raise(Not_found)
  | list{x, ..._} => x
  }

let tail = x =>
  switch x {
  | list{} => None
  | list{_, ...xs} => Some(xs)
  }

let tailExn = x =>
  switch x {
  | list{} => raise(Not_found)
  | list{_, ...t} => t
  }

let add = (xs, x) => list{x, ...xs}

/* Assume `n >=0` */
let rec nthAux = (x, n) =>
  switch x {
  | list{h, ...t} =>
    if n == 0 {
      Some(h)
    } else {
      nthAux(t, n - 1)
    }
  | _ => None
  }

let rec nthAuxAssert = (x, n) =>
  switch x {
  | list{h, ...t} =>
    if n == 0 {
      h
    } else {
      nthAuxAssert(t, n - 1)
    }
  | _ => raise(Not_found)
  }

let get = (x, n) =>
  if n < 0 {
    None
  } else {
    nthAux(x, n)
  }

let getExn = (x, n) =>
  if n < 0 {
    raise(Not_found)
  } else {
    nthAuxAssert(x, n)
  }

let rec partitionAux = (p, cell, precX, precY) =>
  switch cell {
  | list{} => ()
  | list{h, ...t} =>
    let next = mutableCell(h, list{})
    if p(h) {
      unsafeMutateTail(precX, next)
      partitionAux(p, t, next, precY)
    } else {
      unsafeMutateTail(precY, next)
      partitionAux(p, t, precX, next)
    }
  }

let rec splitAux = (cell, precX, precY) =>
  switch cell {
  | list{} => ()
  | list{(a, b), ...t} =>
    let nextA = mutableCell(a, list{})
    let nextB = mutableCell(b, list{})
    unsafeMutateTail(precX, nextA)
    unsafeMutateTail(precY, nextB)
    splitAux(t, nextA, nextB)
  }

/* return the tail pointer so it can continue copy other
   list
*/
let rec copyAuxCont = (cellX, prec) =>
  switch cellX {
  | list{} => prec
  | list{h, ...t} =>
    let next = mutableCell(h, list{})
    unsafeMutateTail(prec, next)
    copyAuxCont(t, next)
  }

let rec copyAuxWitFilter = (f, cellX, prec) =>
  switch cellX {
  | list{} => ()
  | list{h, ...t} =>
    if f(h) {
      let next = mutableCell(h, list{})
      unsafeMutateTail(prec, next)
      copyAuxWitFilter(f, t, next)
    } else {
      copyAuxWitFilter(f, t, prec)
    }
  }

let rec copyAuxWithFilterIndex = (f, cellX, prec, i) =>
  switch cellX {
  | list{} => ()
  | list{h, ...t} =>
    if f(h, i) {
      let next = mutableCell(h, list{})
      unsafeMutateTail(prec, next)
      copyAuxWithFilterIndex(f, t, next, i + 1)
    } else {
      copyAuxWithFilterIndex(f, t, prec, i + 1)
    }
  }

let rec copyAuxWitFilterMap = (f, cellX, prec) =>
  switch cellX {
  | list{} => ()
  | list{h, ...t} =>
    switch f(h) {
    | Some(h) =>
      let next = mutableCell(h, list{})
      unsafeMutateTail(prec, next)
      copyAuxWitFilterMap(f, t, next)
    | None => copyAuxWitFilterMap(f, t, prec)
    }
  }

let rec removeAssocAuxWithMap = (cellX, x, prec, f) =>
  switch cellX {
  | list{} => false
  | list{(a, _) as h, ...t} =>
    if f(a, x) {
      unsafeMutateTail(prec, t)
      true
    } else {
      let next = mutableCell(h, list{})
      unsafeMutateTail(prec, next)
      removeAssocAuxWithMap(t, x, next, f)
    }
  }

let rec setAssocAuxWithMap = (cellX, x, k, prec, eq) =>
  switch cellX {
  | list{} => false
  | list{(a, _) as h, ...t} =>
    if eq(a, x) {
      unsafeMutateTail(prec, list{(x, k), ...t})
      true
    } else {
      let next = mutableCell(h, list{})
      unsafeMutateTail(prec, next)
      setAssocAuxWithMap(t, x, k, next, eq)
    }
  }

let rec copyAuxWithMap = (cellX, prec, f) =>
  switch cellX {
  | list{} => ()
  | list{h, ...t} =>
    let next = mutableCell(f(h), list{})
    unsafeMutateTail(prec, next)
    copyAuxWithMap(t, next, f)
  }

let rec zipAux = (cellX, cellY, prec) =>
  switch (cellX, cellY) {
  | (list{h1, ...t1}, list{h2, ...t2}) =>
    let next = mutableCell((h1, h2), list{})
    unsafeMutateTail(prec, next)
    zipAux(t1, t2, next)
  | (list{}, _) | (_, list{}) => ()
  }

let rec copyAuxWithMap2 = (f, cellX, cellY, prec) =>
  switch (cellX, cellY) {
  | (list{h1, ...t1}, list{h2, ...t2}) =>
    let next = mutableCell(f(h1, h2), list{})
    unsafeMutateTail(prec, next)
    copyAuxWithMap2(f, t1, t2, next)
  | (list{}, _) | (_, list{}) => ()
  }

let rec copyAuxWithMapI = (f, i, cellX, prec) =>
  switch cellX {
  | list{h, ...t} =>
    let next = mutableCell(f(h, i), list{})
    unsafeMutateTail(prec, next)
    copyAuxWithMapI(f, i + 1, t, next)
  | list{} => ()
  }

let rec takeAux = (n, cell, prec) =>
  if n == 0 {
    true
  } else {
    switch cell {
    | list{} => false
    | list{x, ...xs} =>
      let cell = mutableCell(x, list{})
      unsafeMutateTail(prec, cell)
      takeAux(n - 1, xs, cell)
    }
  }

let rec splitAtAux = (n, cell, prec) =>
  if n == 0 {
    Some(cell)
  } else {
    switch cell {
    | list{} => None
    | list{x, ...xs} =>
      let cell = mutableCell(x, list{})
      unsafeMutateTail(prec, cell)
      splitAtAux(n - 1, xs, cell)
    }
  }

/* invarint `n >= 0` */
let take = (lst, n) =>
  if n < 0 {
    None
  } else if n == 0 {
    Some(list{})
  } else {
    switch lst {
    | list{} => None
    | list{x, ...xs} =>
      let cell = mutableCell(x, list{})
      let has = takeAux(n - 1, xs, cell)
      if has {
        Some(cell)
      } else {
        None
      }
    }
  }
/* invariant `n >= 0 ` */
let rec dropAux = (l, n) =>
  if n == 0 {
    Some(l)
  } else {
    switch l {
    | list{_, ...tl} => dropAux(tl, n - 1)
    | list{} => None
    }
  }

let drop = (lst, n) =>
  if n < 0 {
    None
  } else {
    dropAux(lst, n)
  }

let splitAt = (lst, n) =>
  if n < 0 {
    None
  } else if n == 0 {
    Some(list{}, lst)
  } else {
    switch lst {
    | list{} => None
    | list{x, ...xs} =>
      let cell = mutableCell(x, list{})
      let rest = splitAtAux(n - 1, xs, cell)
      switch rest {
      | Some(rest) => Some(cell, rest)
      | None => None
      }
    }
  }

let concat = (xs, ys) =>
  switch xs {
  | list{} => ys
  | list{h, ...t} =>
    let cell = mutableCell(h, list{})
    unsafeMutateTail(copyAuxCont(t, cell), ys)
    cell
  }

let map = (xs, f) =>
  switch xs {
  | list{} => list{}
  | list{h, ...t} =>
    let cell = mutableCell(f(h), list{})
    copyAuxWithMap(t, cell, f)
    cell
  }

let zipBy = (l1, l2, f) =>
  switch (l1, l2) {
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    let cell = mutableCell(f(a1, a2), list{})
    copyAuxWithMap2(f, l1, l2, cell)
    cell
  | (list{}, _) | (_, list{}) => list{}
  }

let mapWithIndex = (xs, f) =>
  switch xs {
  | list{} => list{}
  | list{h, ...t} =>
    let cell = mutableCell(f(h, 0), list{})
    copyAuxWithMapI(f, 1, t, cell)
    cell
  }

let fromInitializer = (~length as n, f) =>
  if n <= 0 {
    list{}
  } else {
    let headX = mutableCell(f(0), list{})
    let cur = ref(headX)
    let i = ref(1)
    while i.contents < n {
      let v = mutableCell(f(i.contents), list{})
      unsafeMutateTail(cur.contents, v)
      cur.contents = v
      i.contents = i.contents + 1
    }

    headX
  }

let make = (type a, ~length as n, v: a): list<a> =>
  if n <= 0 {
    list{}
  } else {
    let headX = mutableCell(v, list{})
    let cur = ref(headX)
    let i = ref(1)
    while i.contents < n {
      let v = mutableCell(v, list{})
      unsafeMutateTail(cur.contents, v)
      cur.contents = v
      i.contents = i.contents + 1
    }

    headX
  }

let rec lengthAux = (x, acc) =>
  switch x {
  | list{} => acc
  | list{_, ...t} => lengthAux(t, acc + 1)
  }

let length = xs => lengthAux(xs, 0)
let size = length

let rec fillAux = (arr, i, x) =>
  switch x {
  | list{} => ()
  | list{h, ...t} =>
    Stdlib_Array.setUnsafe(arr, i, h)
    fillAux(arr, i + 1, t)
  }

let rec fromArrayAux = (a, i, res) =>
  if i < 0 {
    res
  } else {
    fromArrayAux(a, i - 1, list{Stdlib_Array.getUnsafe(a, i), ...res})
  }

let fromArray = a => fromArrayAux(a, Stdlib_Array.length(a) - 1, list{})

let toArray = (x: list<_>) => {
  let len = length(x)
  let arr = A.makeUninitializedUnsafe(len)
  fillAux(arr, 0, x)
  arr
}

let shuffle = xs => {
  let v = toArray(xs)
  Stdlib_Array.shuffle(v)
  fromArray(v)
}

@deprecated("Use `shuffle` instead")
let toShuffled = shuffle

let rec reverseConcat = (l1, l2) =>
  switch l1 {
  | list{} => l2
  | list{a, ...l} => reverseConcat(l, list{a, ...l2})
  }

let reverse = l => reverseConcat(l, list{})

let rec flatAux = (prec, xs) =>
  switch xs {
  | list{} => unsafeMutateTail(prec, list{})
  | list{h, ...r} => flatAux(copyAuxCont(h, prec), r)
  }

let rec flat = xs =>
  switch xs {
  | list{} => list{}
  | list{list{}, ...xs} => flat(xs)
  | list{list{h, ...t}, ...r} =>
    let cell = mutableCell(h, list{})
    flatAux(copyAuxCont(t, cell), r)
    cell
  }

let concatMany = xs =>
  switch xs {
  | [] => list{}
  | [x] => x
  | _ =>
    let len = Stdlib_Array.length(xs)
    let v = ref(Stdlib_Array.getUnsafe(xs, len - 1))
    for i in len - 2 downto 0 {
      v.contents = concat(Stdlib_Array.getUnsafe(xs, i), v.contents)
    }
    v.contents
  }

let rec mapRevAux = (f, accu, xs) =>
  switch xs {
  | list{} => accu
  | list{a, ...l} => mapRevAux(f, list{f(a), ...accu}, l)
  }

let mapReverse = (l, f) => mapRevAux(f, list{}, l)

let rec forEach = (xs, f) =>
  switch xs {
  | list{} => ()
  | list{a, ...l} =>
    f(a)
    forEach(l, f)
  }

let rec forEachWithIndexAux = (xs, f, i) =>
  switch xs {
  | list{} => ()
  | list{a, ...l} =>
    f(a, i)
    forEachWithIndexAux(l, f, i + 1)
  }

let forEachWithIndex = (l, f) => forEachWithIndexAux(l, f, 0)

let rec reduce = (l, accu, f) =>
  switch l {
  | list{} => accu
  | list{a, ...l} => reduce(l, f(accu, a), f)
  }

let rec reduceReverseUnsafe = (l, accu, f) =>
  switch l {
  | list{} => accu
  | list{a, ...l} => f(reduceReverseUnsafe(l, accu, f), a)
  }

let reduceReverse = (type a b, l: list<a>, acc: b, f) => {
  let len = length(l)
  if len < 1000 {
    reduceReverseUnsafe(l, acc, f)
  } else {
    A.reduceReverseU(toArray(l), acc, f)
  }
}

let rec reduceWithIndexAux = (l, acc, f, i) =>
  switch l {
  | list{} => acc
  | list{x, ...xs} => reduceWithIndexAux(xs, f(acc, x, i), f, i + 1)
  }

let reduceWithIndex = (l, acc, f) => reduceWithIndexAux(l, acc, f, 0)

let rec mapRevAux2 = (l1, l2, accu, f) =>
  switch (l1, l2) {
  | (list{a1, ...l1}, list{a2, ...l2}) => mapRevAux2(l1, l2, list{f(a1, a2), ...accu}, f)
  | (_, list{}) | (list{}, _) => accu
  }

let mapReverse2 = (l1, l2, f) => mapRevAux2(l1, l2, list{}, f)

let rec forEach2 = (l1, l2, f) =>
  switch (l1, l2) {
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    f(a1, a2)->ignore
    forEach2(l1, l2, f)
  | (list{}, _) | (_, list{}) => ()
  }

let rec reduce2 = (l1, l2, accu, f) =>
  switch (l1, l2) {
  | (list{a1, ...l1}, list{a2, ...l2}) => reduce2(l1, l2, f(accu, a1, a2), f)
  | (list{}, _) | (_, list{}) => accu
  }

let rec reduceReverse2Unsafe = (l1, l2, accu, f) =>
  switch (l1, l2) {
  | (list{}, list{}) => accu
  | (list{a1, ...l1}, list{a2, ...l2}) => f(reduceReverse2Unsafe(l1, l2, accu, f), a1, a2)
  | (_, list{}) | (list{}, _) => accu
  }

let reduceReverse2 = (type a b c, l1: list<a>, l2: list<b>, acc: c, f) => {
  let len = length(l1)
  if len < 1000 {
    reduceReverse2Unsafe(l1, l2, acc, f)
  } else {
    A.reduceReverse2U(toArray(l1), toArray(l2), acc, f)
  }
}

let rec every = (xs, p) =>
  switch xs {
  | list{} => true
  | list{a, ...l} => p(a) && every(l, p)
  }

let rec some = (xs, p) =>
  switch xs {
  | list{} => false
  | list{a, ...l} => p(a) || some(l, p)
  }

let rec every2 = (l1, l2, p) =>
  switch (l1, l2) {
  | (_, list{}) | (list{}, _) => true
  | (list{a1, ...l1}, list{a2, ...l2}) => p(a1, a2) && every2(l1, l2, p)
  }

let rec compareLength = (l1, l2) =>
  switch (l1, l2) {
  | (list{}, list{}) => Stdlib_Ordering.equal
  | (_, list{}) => Stdlib_Ordering.greater
  | (list{}, _) => Stdlib_Ordering.less
  | (list{_, ...l1s}, list{_, ...l2s}) => compareLength(l1s, l2s)
  }

let rec compare = (l1, l2, p) =>
  switch (l1, l2) {
  | (list{}, list{}) => Stdlib_Ordering.equal
  | (_, list{}) => Stdlib_Ordering.greater
  | (list{}, _) => Stdlib_Ordering.less
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    let c = p(a1, a2)
    if c == Stdlib_Ordering.equal {
      compare(l1, l2, p)
    } else {
      c
    }
  }

let rec equal = (l1, l2, p) =>
  switch (l1, l2) {
  | (list{}, list{}) => true
  | (_, list{})
  | (list{}, _) => false
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    if p(a1, a2) {
      equal(l1, l2, p)
    } else {
      false
    }
  }

let rec some2 = (l1, l2, p) =>
  switch (l1, l2) {
  | (list{}, _) | (_, list{}) => false
  | (list{a1, ...l1}, list{a2, ...l2}) => p(a1, a2) || some2(l1, l2, p)
  }

let rec has = (xs, x, eq) =>
  switch xs {
  | list{} => false
  | list{a, ...l} => eq(a, x) || has(l, x, eq)
  }

@deprecated("Use a `Map` instead")
let rec getAssoc = (xs, x, eq) =>
  switch xs {
  | list{} => None
  | list{(a, b), ...l} =>
    if eq(a, x) {
      Some(b)
    } else {
      getAssoc(l, x, eq)
    }
  }

@deprecated("Use a `Map` instead")
let rec hasAssoc = (xs, x, eq) =>
  switch xs {
  | list{} => false
  | list{(a, _), ...l} => eq(a, x) || hasAssoc(l, x, eq)
  }

@deprecated("Use a `Map` instead")
let removeAssoc = (xs, x, eq) =>
  switch xs {
  | list{} => list{}
  | list{(a, _) as pair, ...l} =>
    if eq(a, x) {
      l
    } else {
      let cell = mutableCell(pair, list{})
      let removed = removeAssocAuxWithMap(l, x, cell, eq)
      if removed {
        cell
      } else {
        xs
      }
    }
  }

@deprecated("Use a `Map` instead")
let setAssoc = (xs, x, k, eq) =>
  switch xs {
  | list{} => list{(x, k)}
  | list{(a, _) as pair, ...l} =>
    if eq(a, x) {
      list{(x, k), ...l}
    } else {
      let cell = mutableCell(pair, list{})
      let replaced = setAssocAuxWithMap(l, x, k, cell, eq)
      if replaced {
        cell
      } else {
        list{(x, k), ...xs}
      }
    }
  }

let sort = (xs, cmp) => {
  let arr = toArray(xs)
  Stdlib_Array.sort(arr, cmp)
  fromArray(arr)
}

let rec find = (xs, p) =>
  switch xs {
  | list{} => None
  | list{x, ...l} =>
    if p(x) {
      Some(x)
    } else {
      find(l, p)
    }
  }

let rec filter = (xs, p) =>
  switch xs {
  | list{} => list{}
  | list{h, ...t} =>
    if p(h) {
      let cell = mutableCell(h, list{})
      copyAuxWitFilter(p, t, cell)
      cell
    } else {
      filter(t, p)
    }
  }

let filterWithIndex = (xs, p) => {
  let rec auxFilterWithIndex = (xs, p, i) =>
    switch xs {
    | list{} => list{}
    | list{h, ...t} =>
      if p(h, i) {
        let cell = mutableCell(h, list{})
        copyAuxWithFilterIndex(p, t, cell, i + 1)
        cell
      } else {
        auxFilterWithIndex(t, p, i + 1)
      }
    }
  auxFilterWithIndex(xs, p, 0)
}

let rec filterMap = (xs, p) =>
  switch xs {
  | list{} => list{}
  | list{h, ...t} =>
    switch p(h) {
    | Some(h) =>
      let cell = mutableCell(h, list{})
      copyAuxWitFilterMap(p, t, cell)
      cell
    | None => filterMap(t, p)
    }
  }

let partition = (l, p) =>
  switch l {
  | list{} => (list{}, list{})
  | list{h, ...t} =>
    let nextX = mutableCell(h, list{})
    let nextY = mutableCell(h, list{})
    let b = p(h)
    partitionAux(p, t, nextX, nextY)
    if b {
      (
        nextX,
        switch nextY {
        | list{_, ...tail} => tail
        | list{} => assert(false)
        },
      )
    } else {
      (
        switch nextX {
        | list{_, ...tail} => tail
        | list{} => assert(false)
        },
        nextY,
      )
    }
  }

let unzip = xs =>
  switch xs {
  | list{} => (list{}, list{})
  | list{(x, y), ...l} =>
    let cellX = mutableCell(x, list{})
    let cellY = mutableCell(y, list{})
    splitAux(l, cellX, cellY)
    (cellX, cellY)
  }

let zip = (l1, l2) =>
  switch (l1, l2) {
  | (_, list{}) | (list{}, _) => list{}
  | (list{a1, ...l1}, list{a2, ...l2}) =>
    let cell = mutableCell((a1, a2), list{})
    zipAux(l1, l2, cell)
    cell
  }

external ignore: list<'a> => unit = "%ignore"

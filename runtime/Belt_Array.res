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

/* Array operations */
type t<'a> = array<'a>

external length: t<'a> => int = "%array_length"

external size: t<'a> => int = "%array_length"

external getUnsafe: (t<'a>, int) => 'a = "%array_unsafe_get"

external setUnsafe: (t<'a>, int, 'a) => unit = "%array_unsafe_set"

external getUndefined: (t<'a>, int) => Js.undefined<'a> = "%array_unsafe_get"

/* external get: 'a t -> int -> 'a = "%array_safe_get" */
let get = (arr, i) =>
  if i >= 0 && i < length(arr) {
    Some(getUnsafe(arr, i))
  } else {
    None
  }

let getOrThrow = (arr, i) => {
  assert(i >= 0 && i < length(arr))
  getUnsafe(arr, i)
}

let getExn = getOrThrow

let set = (arr, i, v) =>
  if i >= 0 && i < length(arr) {
    setUnsafe(arr, i, v)
    true
  } else {
    false
  }

let setOrThrow = (arr, i, v) => {
  assert(i >= 0 && i < length(arr))
  setUnsafe(arr, i, v)
}

let setExn = setOrThrow

@set external truncateToLengthUnsafe: (t<'a>, int) => unit = "length"

@new external makeUninitialized: int => array<Js.undefined<'a>> = "Array"

@new external makeUninitializedUnsafe: int => array<'a> = "Array"

@send external copy: (t<'a>, @as(0) _) => t<'a> = "slice"

let swapUnsafe = (xs, i, j) => {
  let tmp = getUnsafe(xs, i)
  setUnsafe(xs, i, getUnsafe(xs, j))
  setUnsafe(xs, j, tmp)
}

@val @scope("Math") external random: unit => float = "random"
@val @scope("Math") external floor: float => int = "floor"
external toFloat: int => float = "%floatofint"

let shuffleInPlace = xs => {
  let len = length(xs)
  let random_int = (min, max) => floor(random() *. toFloat(max - min)) + min
  for i in 0 to len - 1 {
    swapUnsafe(xs, i, random_int(i, len)) /* [i,len) */
  }
}

let shuffle = xs => {
  let result = copy(xs)
  shuffleInPlace(result)

  /* TODO: improve */
  result
}

let reverseAux = (xs, ofs, len) =>
  for i in 0 to len / 2 - 1 {
    swapUnsafe(xs, ofs + i, ofs + len - i - 1)
  }

let reverseInPlace = xs => {
  let len = length(xs)
  reverseAux(xs, 0, len)
}

let reverse = xs => {
  let len = length(xs)
  let result = makeUninitializedUnsafe(len)
  for i in 0 to len - 1 {
    setUnsafe(result, i, getUnsafe(xs, len - 1 - i))
  }
  result
}

let make = (l, f) =>
  if l <= 0 {
    []
  } else {
    let res = makeUninitializedUnsafe(l)
    for i in 0 to l - 1 {
      setUnsafe(res, i, f)
    }
    res
  }

/* See #6575. We could also check for maximum array size, but this depends
 on whether we create a float array or a regular one... */
let makeBy = (l, f) =>
  if l <= 0 {
    []
  } else {
    let res = makeUninitializedUnsafe(l)
    for i in 0 to l - 1 {
      setUnsafe(res, i, f(i))
    }
    res
  }

let makeByAndShuffle = (l, f) => {
  let u = makeBy(l, f)
  shuffleInPlace(u)
  u
}

let range = (start, finish) => {
  let cut = finish - start
  if cut < 0 {
    []
  } else {
    let arr = makeUninitializedUnsafe(cut + 1)
    for i in 0 to cut {
      setUnsafe(arr, i, start + i)
    }
    arr
  }
}

let rangeBy = (start, finish, ~step) => {
  let cut = finish - start
  if cut < 0 || step <= 0 {
    []
  } else {
    let nb = cut / step + 1
    let arr = makeUninitializedUnsafe(nb)
    let cur = ref(start)
    for i in 0 to nb - 1 {
      setUnsafe(arr, i, cur.contents)
      cur.contents = cur.contents + step
    }
    arr
  }
}

let zip = (xs, ys) => {
  let (lenx, leny) = (length(xs), length(ys))
  let len = Pervasives.min(lenx, leny)
  let s = makeUninitializedUnsafe(len)
  for i in 0 to len - 1 {
    setUnsafe(s, i, (getUnsafe(xs, i), getUnsafe(ys, i)))
  }
  s
}

let zipBy = (xs, ys, f) => {
  let (lenx, leny) = (length(xs), length(ys))
  let len = Pervasives.min(lenx, leny)
  let s = makeUninitializedUnsafe(len)
  for i in 0 to len - 1 {
    setUnsafe(s, i, f(getUnsafe(xs, i), getUnsafe(ys, i)))
  }
  s
}

let concat = (a1, a2) => {
  let l1 = length(a1)
  let l2 = length(a2)
  let a1a2 = makeUninitializedUnsafe(l1 + l2)
  for i in 0 to l1 - 1 {
    setUnsafe(a1a2, i, getUnsafe(a1, i))
  }
  for i in 0 to l2 - 1 {
    setUnsafe(a1a2, l1 + i, getUnsafe(a2, i))
  }
  a1a2
}

let concatMany = arrs => {
  let lenArrs = length(arrs)
  let totalLen = ref(0)
  for i in 0 to lenArrs - 1 {
    totalLen.contents = totalLen.contents + length(getUnsafe(arrs, i))
  }
  let result = makeUninitializedUnsafe(totalLen.contents)
  totalLen.contents = 0
  for j in 0 to lenArrs - 1 {
    let cur = getUnsafe(arrs, j)
    for k in 0 to length(cur) - 1 {
      setUnsafe(result, totalLen.contents, getUnsafe(cur, k))
      totalLen.contents = totalLen.contents + 1
    }
  }
  result
}

let slice = (a, ~offset, ~len) =>
  if len <= 0 {
    []
  } else {
    let lena = length(a)
    let ofs = if offset < 0 {
      Pervasives.max(lena + offset, 0)
    } else {
      offset
    }
    let hasLen = lena - ofs
    let copyLength = Pervasives.min(hasLen, len)
    if copyLength <= 0 {
      []
    } else {
      let result = makeUninitializedUnsafe(copyLength)
      for i in 0 to copyLength - 1 {
        setUnsafe(result, i, getUnsafe(a, ofs + i))
      }
      result
    }
  }

let sliceToEnd = (a, offset) => {
  let lena = length(a)
  let ofs = if offset < 0 {
    Pervasives.max(lena + offset, 0)
  } else {
    offset
  }
  let len = if lena > ofs {
    lena - ofs
  } else {
    0
  }
  let result = makeUninitializedUnsafe(len)
  for i in 0 to len - 1 {
    setUnsafe(result, i, getUnsafe(a, ofs + i))
  }
  result
}

let fill = (a, ~offset, ~len, v) =>
  if len > 0 {
    let lena = length(a)
    let ofs = if offset < 0 {
      Pervasives.max(lena + offset, 0)
    } else {
      offset
    }
    let hasLen = lena - ofs
    let fillLength = Pervasives.min(hasLen, len)
    if fillLength > 0 {
      for i in ofs to ofs + fillLength - 1 {
        setUnsafe(a, i, v)
      }
    }
  }

let blitUnsafe = (
  ~src as a1,
  ~srcOffset as srcofs1,
  ~dst as a2,
  ~dstOffset as srcofs2,
  ~len as blitLength,
) =>
  if srcofs2 <= srcofs1 {
    for j in 0 to blitLength - 1 {
      setUnsafe(a2, j + srcofs2, getUnsafe(a1, j + srcofs1))
    }
  } else {
    for j in blitLength - 1 downto 0 {
      setUnsafe(a2, j + srcofs2, getUnsafe(a1, j + srcofs1))
    }
  }

/* We don't need check `blitLength` since when `blitLength < 0` the
   for loop will be nop
*/
let blit = (~src as a1, ~srcOffset as ofs1, ~dst as a2, ~dstOffset as ofs2, ~len) => {
  let lena1 = length(a1)
  let lena2 = length(a2)
  let srcofs1 = if ofs1 < 0 {
    Pervasives.max(lena1 + ofs1, 0)
  } else {
    ofs1
  }
  let srcofs2 = if ofs2 < 0 {
    Pervasives.max(lena2 + ofs2, 0)
  } else {
    ofs2
  }
  let blitLength = Pervasives.min(len, Pervasives.min(lena1 - srcofs1, lena2 - srcofs2))

  /* blitUnsafe a1 srcofs1 a2 srcofs2 blitLength */
  if srcofs2 <= srcofs1 {
    for j in 0 to blitLength - 1 {
      setUnsafe(a2, j + srcofs2, getUnsafe(a1, j + srcofs1))
    }
  } else {
    for j in blitLength - 1 downto 0 {
      setUnsafe(a2, j + srcofs2, getUnsafe(a1, j + srcofs1))
    }
  }
}

let forEach = (a, f) =>
  for i in 0 to length(a) - 1 {
    f(getUnsafe(a, i))
  }

let map = (a, f) => {
  let l = length(a)
  let r = makeUninitializedUnsafe(l)
  for i in 0 to l - 1 {
    setUnsafe(r, i, f(getUnsafe(a, i)))
  }
  r
}

let flatMap = (a, f) => concatMany(map(a, f))

let getBy = (a, p) => {
  let l = length(a)
  let i = ref(0)
  let r = ref(None)
  while r.contents == None && i.contents < l {
    let v = getUnsafe(a, i.contents)
    if p(v) {
      r.contents = Some(v)
    }
    i.contents = i.contents + 1
  }
  r.contents
}

let getIndexBy = (a, p) => {
  let l = length(a)
  let i = ref(0)
  let r = ref(None)
  while r.contents == None && i.contents < l {
    let v = getUnsafe(a, i.contents)
    if p(v) {
      r.contents = Some(i.contents)
    }
    i.contents = i.contents + 1
  }
  r.contents
}

let keep = (a, f) => {
  let l = length(a)
  let r = makeUninitializedUnsafe(l)
  let j = ref(0)
  for i in 0 to l - 1 {
    let v = getUnsafe(a, i)
    if f(v) {
      setUnsafe(r, j.contents, v)
      j.contents = j.contents + 1
    }
  }
  truncateToLengthUnsafe(r, j.contents)
  r
}

let keepWithIndex = (a, f) => {
  let l = length(a)
  let r = makeUninitializedUnsafe(l)
  let j = ref(0)
  for i in 0 to l - 1 {
    let v = getUnsafe(a, i)
    if f(v, i) {
      setUnsafe(r, j.contents, v)
      j.contents = j.contents + 1
    }
  }
  truncateToLengthUnsafe(r, j.contents)
  r
}

let keepMap = (a, f) => {
  let l = length(a)
  let r = makeUninitializedUnsafe(l)
  let j = ref(0)
  for i in 0 to l - 1 {
    let v = getUnsafe(a, i)
    switch f(v) {
    | None => ()
    | Some(v) =>
      setUnsafe(r, j.contents, v)
      j.contents = j.contents + 1
    }
  }
  truncateToLengthUnsafe(r, j.contents)
  r
}

let forEachWithIndex = (a, f) =>
  for i in 0 to length(a) - 1 {
    f(i, getUnsafe(a, i))
  }

let mapWithIndex = (a, f) => {
  let l = length(a)
  let r = makeUninitializedUnsafe(l)
  for i in 0 to l - 1 {
    setUnsafe(r, i, f(i, getUnsafe(a, i)))
  }
  r
}

let reduce = (a, x, f) => {
  let r = ref(x)
  for i in 0 to length(a) - 1 {
    r.contents = f(r.contents, getUnsafe(a, i))
  }
  r.contents
}

let reduceReverse = (a, x, f) => {
  let r = ref(x)
  for i in length(a) - 1 downto 0 {
    r.contents = f(r.contents, getUnsafe(a, i))
  }
  r.contents
}

let reduceReverse2 = (a, b, x, f) => {
  let r = ref(x)
  let len = Pervasives.min(length(a), length(b))
  for i in len - 1 downto 0 {
    r.contents = f(r.contents, getUnsafe(a, i), getUnsafe(b, i))
  }
  r.contents
}

let reduceWithIndex = (a, x, f) => {
  let r = ref(x)
  for i in 0 to length(a) - 1 {
    r.contents = f(r.contents, getUnsafe(a, i), i)
  }
  r.contents
}

let rec everyAux = (arr, i, b, len) =>
  if i == len {
    true
  } else if b(getUnsafe(arr, i)) {
    everyAux(arr, i + 1, b, len)
  } else {
    false
  }

let rec someAux = (arr, i, b, len) =>
  if i == len {
    false
  } else if b(getUnsafe(arr, i)) {
    true
  } else {
    someAux(arr, i + 1, b, len)
  }

let every = (arr, b) => {
  let len = length(arr)
  everyAux(arr, 0, b, len)
}

let some = (arr, b) => {
  let len = length(arr)
  someAux(arr, 0, b, len)
}

let rec everyAux2 = (arr1, arr2, i, b, len) =>
  if i == len {
    true
  } else if b(getUnsafe(arr1, i), getUnsafe(arr2, i)) {
    everyAux2(arr1, arr2, i + 1, b, len)
  } else {
    false
  }

let rec someAux2 = (arr1, arr2, i, b, len) =>
  if i == len {
    false
  } else if b(getUnsafe(arr1, i), getUnsafe(arr2, i)) {
    true
  } else {
    someAux2(arr1, arr2, i + 1, b, len)
  }

let every2 = (a, b, p) => everyAux2(a, b, 0, p, Pervasives.min(length(a), length(b)))

let some2 = (a, b, p) => someAux2(a, b, 0, p, Pervasives.min(length(a), length(b)))

let eq = (a, b, p) => {
  let lena = length(a)
  let lenb = length(b)
  if lena == lenb {
    everyAux2(a, b, 0, p, lena)
  } else {
    false
  }
}

let rec everyCmpAux2 = (arr1, arr2, i, b, len) =>
  if i == len {
    0
  } else {
    let c = b(getUnsafe(arr1, i), getUnsafe(arr2, i))
    if c == 0 {
      everyCmpAux2(arr1, arr2, i + 1, b, len)
    } else {
      c
    }
  }

let cmp = (a, b, p) => {
  let lena = length(a)
  let lenb = length(b)
  if lena > lenb {
    1
  } else if lena < lenb {
    -1
  } else {
    everyCmpAux2(a, b, 0, p, lena)
  }
}

let partition = (a, f) => {
  let l = length(a)
  let i = ref(0)
  let j = ref(0)
  let a1 = makeUninitializedUnsafe(l)
  let a2 = makeUninitializedUnsafe(l)
  for ii in 0 to l - 1 {
    let v = getUnsafe(a, ii)
    if f(v) {
      setUnsafe(a1, i.contents, v)
      i.contents = i.contents + 1
    } else {
      setUnsafe(a2, j.contents, v)
      j.contents = j.contents + 1
    }
  }
  truncateToLengthUnsafe(a1, i.contents)
  truncateToLengthUnsafe(a2, j.contents)
  (a1, a2)
}

let unzip = a => {
  let l = length(a)
  let a1 = makeUninitializedUnsafe(l)
  let a2 = makeUninitializedUnsafe(l)
  for i in 0 to l - 1 {
    let (v1, v2) = getUnsafe(a, i)
    setUnsafe(a1, i, v1)
    setUnsafe(a2, i, v2)
  }
  (a1, a2)
}

let joinWith = (a, sep, toString) =>
  switch length(a) {
  | 0 => ""
  | l =>
    let lastIndex = l - 1
    let rec aux = (i, res) =>
      if i == lastIndex {
        res ++ toString(getUnsafe(a, i))
      } else {
        aux(i + 1, res ++ (toString(getUnsafe(a, i)) ++ sep))
      }

    aux(0, "")
  }

let init = (n, f) => {
  let v = makeUninitializedUnsafe(n)
  for i in 0 to n - 1 {
    setUnsafe(v, i, f(i))
  }
  v
}

let cmpU = cmp
let eqU = eq
let every2U = every2
let everyU = every
let flatMapU = flatMap
let forEachU = forEach
let forEachWithIndexU = forEachWithIndex
let getByU = getBy
let getIndexByU = getIndexBy
let initU = init
let joinWithU = joinWith
let keepMapU = keepMap
let keepU = keep
let keepWithIndexU = keepWithIndex
let makeByAndShuffleU = makeByAndShuffle
let makeByU = makeBy
let mapU = map
let mapWithIndexU = mapWithIndex
let partitionU = partition
let reduceReverse2U = reduceReverse2
let reduceReverseU = reduceReverse
let reduceU = reduce
let reduceWithIndexU = reduceWithIndex
let some2U = some2
let someU = some
let zipByU = zipBy

@send external push: (t<'a>, 'a) => unit = "push"

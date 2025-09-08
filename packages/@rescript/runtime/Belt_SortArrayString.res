type element = string

module A = Belt_Array

let rec sortedLengthAuxMore = (xs: array<element>, prec, acc, len) =>
  if acc >= len {
    acc
  } else {
    let v = A.getUnsafe(xs, acc)
    if prec > v {
      sortedLengthAuxMore(xs, v, acc + 1, len)
    } else {
      acc
    }
  }

let rec sortedLengthAuxLess = (xs: array<element>, prec, acc, len) =>
  if acc >= len {
    acc
  } else {
    let v = A.getUnsafe(xs, acc)
    if prec < v {
      sortedLengthAuxLess(xs, v, acc + 1, len)
    } else {
      acc
    }
  }

let strictlySortedLength = (xs: array<element>) => {
  let len = A.length(xs)
  switch len {
  | 0 | 1 => len
  | _ =>
    let (x0, x1) = (A.getUnsafe(xs, 0), A.getUnsafe(xs, 1))

    /* let c = cmp x0 x1 [@bs]  in */
    if x0 < x1 {
      sortedLengthAuxLess(xs, x1, 2, len)
    } else if x0 > x1 {
      -sortedLengthAuxMore(xs, x1, 2, len)
    } else {
      1
    }
  }
}

let rec isSortedAux = (a: array<element>, i, last_bound) =>
  /* when `i = len - 1`, it reaches the last element */
  if i == last_bound {
    true
  } else if A.getUnsafe(a, i) <= A.getUnsafe(a, i + 1) {
    isSortedAux(a, i + 1, last_bound)
  } else {
    false
  }

let isSorted = a => {
  let len = A.length(a)
  if len == 0 {
    true
  } else {
    isSortedAux(a, 0, len - 1)
  }
}

let cutoff = 5

let merge = (src: array<element>, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) => {
  let src1r = src1ofs + src1len and src2r = src2ofs + src2len
  let rec loop = (i1, s1, i2, s2, d) =>
    if s1 <= s2 {
      A.setUnsafe(dst, d, s1)
      let i1 = i1 + 1
      if i1 < src1r {
        loop(i1, A.getUnsafe(src, i1), i2, s2, d + 1)
      } else {
        A.blitUnsafe(~src=src2, ~srcOffset=i2, ~dst, ~dstOffset=d + 1, ~len=src2r - i2)
      }
    } else {
      A.setUnsafe(dst, d, s2)
      let i2 = i2 + 1
      if i2 < src2r {
        loop(i1, s1, i2, A.getUnsafe(src2, i2), d + 1)
      } else {
        A.blitUnsafe(~src, ~srcOffset=i1, ~dst, ~dstOffset=d + 1, ~len=src1r - i1)
      }
    }

  loop(src1ofs, A.getUnsafe(src, src1ofs), src2ofs, A.getUnsafe(src2, src2ofs), dstofs)
}

let union = (src: array<element>, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) => {
  let src1r = src1ofs + src1len
  let src2r = src2ofs + src2len
  let rec loop = (i1, s1, i2, s2, d) =>
    /* let c = cmp s1 s2 [@bs] in */
    if s1 < s2 {
      /* `s1` is larger than all elements in `d` */
      A.setUnsafe(dst, d, s1)
      let i1 = i1 + 1
      let d = d + 1
      if i1 < src1r {
        loop(i1, A.getUnsafe(src, i1), i2, s2, d)
      } else {
        A.blitUnsafe(~src=src2, ~srcOffset=i2, ~dst, ~dstOffset=d, ~len=src2r - i2)
        d + src2r - i2
      }
    } else if s1 == s2 {
      A.setUnsafe(dst, d, s1)
      let i1 = i1 + 1
      let i2 = i2 + 1
      let d = d + 1
      if i1 < src1r && i2 < src2r {
        loop(i1, A.getUnsafe(src, i1), i2, A.getUnsafe(src2, i2), d)
      } else if i1 == src1r {
        A.blitUnsafe(~src=src2, ~srcOffset=i2, ~dst, ~dstOffset=d, ~len=src2r - i2)
        d + src2r - i2
      } else {
        A.blitUnsafe(~src, ~srcOffset=i1, ~dst, ~dstOffset=d, ~len=src1r - i1)
        d + src1r - i1
      }
    } else {
      A.setUnsafe(dst, d, s2)
      let i2 = i2 + 1
      let d = d + 1
      if i2 < src2r {
        loop(i1, s1, i2, A.getUnsafe(src2, i2), d)
      } else {
        A.blitUnsafe(~src, ~srcOffset=i1, ~dst, ~dstOffset=d, ~len=src1r - i1)
        d + src1r - i1
      }
    }

  loop(src1ofs, A.getUnsafe(src, src1ofs), src2ofs, A.getUnsafe(src2, src2ofs), dstofs)
}

let intersect = (src: array<element>, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) => {
  let src1r = src1ofs + src1len
  let src2r = src2ofs + src2len
  let rec loop = (i1, s1, i2, s2, d) =>
    /* let c = cmp s1 s2 [@bs] in */
    if s1 < s2 {
      /* A.setUnsafe dst d s1; */
      let i1 = i1 + 1
      if i1 < src1r {
        loop(i1, A.getUnsafe(src, i1), i2, s2, d)
      } else {
        d
      }
    } else if s1 == s2 {
      A.setUnsafe(dst, d, s1)
      let i1 = i1 + 1
      let i2 = i2 + 1
      let d = d + 1
      if i1 < src1r && i2 < src2r {
        loop(i1, A.getUnsafe(src, i1), i2, A.getUnsafe(src2, i2), d)
      } else {
        d
      }
    } else {
      /* A.setUnsafe dst d s2; */
      let i2 = i2 + 1
      if i2 < src2r {
        loop(i1, s1, i2, A.getUnsafe(src2, i2), d)
      } else {
        d
      }
    }

  loop(src1ofs, A.getUnsafe(src, src1ofs), src2ofs, A.getUnsafe(src2, src2ofs), dstofs)
}

let diff = (src: array<element>, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) => {
  let src1r = src1ofs + src1len
  let src2r = src2ofs + src2len
  let rec loop = (i1, s1, i2, s2, d) =>
    /* let c = cmp s1 s2 [@bs] in */
    if s1 < s2 {
      A.setUnsafe(dst, d, s1)
      let d = d + 1
      let i1 = i1 + 1
      if i1 < src1r {
        loop(i1, A.getUnsafe(src, i1), i2, s2, d)
      } else {
        d
      }
    } else if s1 == s2 {
      let i1 = i1 + 1
      let i2 = i2 + 1
      if i1 < src1r && i2 < src2r {
        loop(i1, A.getUnsafe(src, i1), i2, A.getUnsafe(src2, i2), d)
      } else if i1 == src1r {
        d
      } else {
        A.blitUnsafe(~src, ~srcOffset=i1, ~dst, ~dstOffset=d, ~len=src1r - i1)
        d + src1r - i1
      }
    } else {
      let i2 = i2 + 1
      if i2 < src2r {
        loop(i1, s1, i2, A.getUnsafe(src2, i2), d)
      } else {
        A.blitUnsafe(~src, ~srcOffset=i1, ~dst, ~dstOffset=d, ~len=src1r - i1)
        d + src1r - i1
      }
    }

  loop(src1ofs, A.getUnsafe(src, src1ofs), src2ofs, A.getUnsafe(src2, src2ofs), dstofs)
}

let insertionSort = (src: array<element>, srcofs, dst, dstofs, len) =>
  for i in 0 to len - 1 {
    let e = A.getUnsafe(src, srcofs + i)
    let j = ref(dstofs + i - 1)
    while j.contents >= dstofs && A.getUnsafe(dst, j.contents) > e {
      A.setUnsafe(dst, j.contents + 1, A.getUnsafe(dst, j.contents))
      j.contents = j.contents - 1
    }
    A.setUnsafe(dst, j.contents + 1, e)
  }

let rec sortTo = (src: array<element>, srcofs, dst, dstofs, len) =>
  if len <= cutoff {
    insertionSort(src, srcofs, dst, dstofs, len)
  } else {
    let l1 = len / 2
    let l2 = len - l1
    sortTo(src, srcofs + l1, dst, dstofs + l1, l2)
    sortTo(src, srcofs, src, srcofs + l2, l1)
    merge(src, srcofs + l2, l1, dst, dstofs + l1, l2, dst, dstofs)
  }

let stableSortInPlace = (a: array<element>) => {
  let l = A.length(a)
  if l <= cutoff {
    insertionSort(a, 0, a, 0, l)
  } else {
    let l1 = l / 2
    let l2 = l - l1
    let t = Belt_Array.makeUninitializedUnsafe(l2)
    sortTo(a, l1, t, 0, l2)
    sortTo(a, 0, a, l2, l1)
    merge(a, l2, l1, t, 0, l2, a, 0)
  }
}

let stableSort = a => {
  let b = A.copy(a)
  stableSortInPlace(b)
  b
}

let rec binarySearchAux = (arr: array<element>, lo, hi, key) => {
  let mid = (lo + hi) / 2
  let midVal = A.getUnsafe(arr, mid)

  /* let c = cmp key midVal [@bs] in */
  if key == midVal {
    mid
  } else if key < midVal {
    /* a[lo] =< key < a[mid] <= a[hi] */
    if hi == mid {
      if A.getUnsafe(arr, lo) == key {
        lo
      } else {
        -(hi + 1)
      }
    } else {
      binarySearchAux(arr, lo, mid, key)
    }
  } /* a[lo] =< a[mid] < key <= a[hi] */
  else if lo == mid {
    if A.getUnsafe(arr, hi) == key {
      hi
    } else {
      -(hi + 1)
    }
  } else {
    binarySearchAux(arr, mid, hi, key)
  }
}

let binarySearch = (sorted: array<element>, key): int => {
  let len = A.length(sorted)
  if len == 0 {
    -1
  } else {
    let lo = A.getUnsafe(sorted, 0)

    /* let c = cmp key lo [@bs] in */
    if key < lo {
      -1
    } else {
      let hi = A.getUnsafe(sorted, len - 1)

      /* let c2 = cmp key hi [@bs]in */
      if key > hi {
        -(len + 1)
      } else {
        binarySearchAux(sorted, 0, len - 1, key)
      }
    }
  }
}

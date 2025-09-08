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

module Int = Belt_SortArrayInt

module String = Belt_SortArrayString

module A = Belt_Array

let rec sortedLengthAuxMore = (xs, prec, acc, len, lt) =>
  if acc >= len {
    acc
  } else {
    let v = A.getUnsafe(xs, acc)
    if lt(v, prec) {
      sortedLengthAuxMore(xs, v, acc + 1, len, lt)
    } else {
      acc
    }
  }

let rec sortedLengthAuxLess = (xs, prec, acc, len, lt) =>
  if acc >= len {
    acc
  } else {
    let v = A.getUnsafe(xs, acc)
    if lt(prec, v) {
      sortedLengthAuxLess(xs, v, acc + 1, len, lt)
    } else {
      acc
    }
  }

let strictlySortedLength = (xs, lt) => {
  let len = A.length(xs)
  switch len {
  | 0 | 1 => len
  | _ =>
    let (x0, x1) = (A.getUnsafe(xs, 0), A.getUnsafe(xs, 1))

    /* let c = cmp x0 x1 [@bs]  in */
    if lt(x0, x1) {
      sortedLengthAuxLess(xs, x1, 2, len, lt)
    } else if lt(x1, x0) {
      -sortedLengthAuxMore(xs, x1, 2, len, lt)
    } else {
      1
    }
  }
}

let rec isSortedAux = (a, i, cmp, last_bound) =>
  /* when `i = len - 1`, it reaches the last element */
  if i == last_bound {
    true
  } else if cmp(A.getUnsafe(a, i), A.getUnsafe(a, i + 1)) <= 0 {
    isSortedAux(a, i + 1, cmp, last_bound)
  } else {
    false
  }

let isSorted = (a, cmp) => {
  let len = A.length(a)
  if len == 0 {
    true
  } else {
    isSortedAux(a, 0, cmp, len - 1)
  }
}

let cutoff = 5

let merge = (src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs, cmp) => {
  let src1r = src1ofs + src1len and src2r = src2ofs + src2len
  let rec loop = (i1, s1, i2, s2, d) =>
    if cmp(s1, s2) <= 0 {
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

let union = (src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs, cmp) => {
  let src1r = src1ofs + src1len
  let src2r = src2ofs + src2len
  let rec loop = (i1, s1, i2, s2, d) => {
    let c = cmp(s1, s2)
    if c < 0 {
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
    } else if c == 0 {
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
  }

  loop(src1ofs, A.getUnsafe(src, src1ofs), src2ofs, A.getUnsafe(src2, src2ofs), dstofs)
}

let intersect = (src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs, cmp) => {
  let src1r = src1ofs + src1len
  let src2r = src2ofs + src2len
  let rec loop = (i1, s1, i2, s2, d) => {
    let c = cmp(s1, s2)
    if c < 0 {
      /* A.setUnsafe dst d s1; */
      let i1 = i1 + 1
      if i1 < src1r {
        loop(i1, A.getUnsafe(src, i1), i2, s2, d)
      } else {
        d
      }
    } else if c == 0 {
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
  }

  loop(src1ofs, A.getUnsafe(src, src1ofs), src2ofs, A.getUnsafe(src2, src2ofs), dstofs)
}

let diff = (src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs, cmp) => {
  let src1r = src1ofs + src1len
  let src2r = src2ofs + src2len
  let rec loop = (i1, s1, i2, s2, d) => {
    let c = cmp(s1, s2)
    if c < 0 {
      A.setUnsafe(dst, d, s1)
      let d = d + 1
      let i1 = i1 + 1
      if i1 < src1r {
        loop(i1, A.getUnsafe(src, i1), i2, s2, d)
      } else {
        d
      }
    } else if c == 0 {
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
  }

  loop(src1ofs, A.getUnsafe(src, src1ofs), src2ofs, A.getUnsafe(src2, src2ofs), dstofs)
}

/* `<=` alone is not enough for stable sort */
let insertionSort = (src, srcofs, dst, dstofs, len, cmp) =>
  for i in 0 to len - 1 {
    let e = A.getUnsafe(src, srcofs + i)
    let j = ref(dstofs + i - 1)
    while j.contents >= dstofs && cmp(A.getUnsafe(dst, j.contents), e) > 0 {
      A.setUnsafe(dst, j.contents + 1, A.getUnsafe(dst, j.contents))
      j.contents = j.contents - 1
    }
    A.setUnsafe(dst, j.contents + 1, e)
  }

let rec sortTo = (src, srcofs, dst, dstofs, len, cmp) =>
  if len <= cutoff {
    insertionSort(src, srcofs, dst, dstofs, len, cmp)
  } else {
    let l1 = len / 2
    let l2 = len - l1
    sortTo(src, srcofs + l1, dst, dstofs + l1, l2, cmp)
    sortTo(src, srcofs, src, srcofs + l2, l1, cmp)
    merge(src, srcofs + l2, l1, dst, dstofs + l1, l2, dst, dstofs, cmp)
  }

let stableSortInPlaceBy = (a, cmp) => {
  let l = A.length(a)
  if l <= cutoff {
    insertionSort(a, 0, a, 0, l, cmp)
  } else {
    let l1 = l / 2
    let l2 = l - l1
    let t = Belt_Array.makeUninitializedUnsafe(l2)
    sortTo(a, l1, t, 0, l2, cmp)
    sortTo(a, 0, a, l2, l1, cmp)
    merge(a, l2, l1, t, 0, l2, a, 0, cmp)
  }
}

let stableSortBy = (a, cmp) => {
  let b = A.copy(a)
  stableSortInPlaceBy(b, cmp)
  b
}

/*
  `binarySearchAux arr lo hi key cmp`
  range [lo, hi]
  input (lo <= hi)
  `arr[lo] <= key <= arr[hi]` */
let rec binarySearchAux = (arr, lo, hi, key, cmp) => {
  let mid = (lo + hi) / 2
  let midVal = A.getUnsafe(arr, mid)
  let c = cmp(key, midVal)
  if c == 0 {
    mid
  } else if c < 0 {
    /* a[lo] =< key < a[mid] <= a[hi] */
    if hi == mid {
      if cmp(A.getUnsafe(arr, lo), key) == 0 {
        lo
      } else {
        -(hi + 1)
      }
    } else {
      binarySearchAux(arr, lo, mid, key, cmp)
    }
  } /* a[lo] =< a[mid] < key <= a[hi] */
  else if lo == mid {
    if cmp(A.getUnsafe(arr, hi), key) == 0 {
      hi
    } else {
      -(hi + 1)
    }
  } else {
    binarySearchAux(arr, mid, hi, key, cmp)
  }
}

let binarySearchBy = (sorted, key, cmp): int => {
  let len = A.length(sorted)
  if len == 0 {
    -1
  } else {
    let lo = A.getUnsafe(sorted, 0)
    let c = cmp(key, lo)
    if c < 0 {
      -1
    } else {
      let hi = A.getUnsafe(sorted, len - 1)
      let c2 = cmp(key, hi)
      if c2 > 0 {
        -(len + 1)
      } else {
        binarySearchAux(sorted, 0, len - 1, key, cmp)
      }
    }
  }
}

let binarySearchByU = binarySearchBy
let diffU = diff
let intersectU = intersect
let isSortedU = isSorted
let stableSortByU = stableSortBy
let stableSortInPlaceByU = stableSortInPlaceBy
let strictlySortedLengthU = strictlySortedLength
let unionU = union

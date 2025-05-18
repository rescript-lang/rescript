'use strict';

let Belt_Array = require("./Belt_Array.js");

function sortedLengthAuxMore(xs, _prec, _acc, len) {
  while (true) {
    let acc = _acc;
    let prec = _prec;
    if (acc >= len) {
      return acc;
    }
    let v = xs[acc];
    if (prec <= v) {
      return acc;
    }
    _acc = acc + 1 | 0;
    _prec = v;
    continue;
  };
}

function strictlySortedLength(xs) {
  let len = xs.length;
  if (len === 0 || len === 1) {
    return len;
  }
  let x0 = xs[0];
  let x1 = xs[1];
  if (x0 < x1) {
    let _prec = x1;
    let _acc = 2;
    while (true) {
      let acc = _acc;
      let prec = _prec;
      if (acc >= len) {
        return acc;
      }
      let v = xs[acc];
      if (prec >= v) {
        return acc;
      }
      _acc = acc + 1 | 0;
      _prec = v;
      continue;
    };
  } else if (x0 > x1) {
    return -sortedLengthAuxMore(xs, x1, 2, len) | 0;
  } else {
    return 1;
  }
}

function isSorted(a) {
  let len = a.length;
  if (len === 0) {
    return true;
  } else {
    let _i = 0;
    let last_bound = len - 1 | 0;
    while (true) {
      let i = _i;
      if (i === last_bound) {
        return true;
      }
      if (a[i] > a[i + 1 | 0]) {
        return false;
      }
      _i = i + 1 | 0;
      continue;
    };
  }
}

function merge(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) {
  let src1r = src1ofs + src1len | 0;
  let src2r = src2ofs + src2len | 0;
  let _i1 = src1ofs;
  let _s1 = src[src1ofs];
  let _i2 = src2ofs;
  let _s2 = src2[src2ofs];
  let _d = dstofs;
  while (true) {
    let d = _d;
    let s2 = _s2;
    let i2 = _i2;
    let s1 = _s1;
    let i1 = _i1;
    if (s1 <= s2) {
      dst[d] = s1;
      let i1$1 = i1 + 1 | 0;
      if (i1$1 >= src1r) {
        return Belt_Array.blitUnsafe(src2, i2, dst, d + 1 | 0, src2r - i2 | 0);
      }
      _d = d + 1 | 0;
      _s1 = src[i1$1];
      _i1 = i1$1;
      continue;
    }
    dst[d] = s2;
    let i2$1 = i2 + 1 | 0;
    if (i2$1 >= src2r) {
      return Belt_Array.blitUnsafe(src, i1, dst, d + 1 | 0, src1r - i1 | 0);
    }
    _d = d + 1 | 0;
    _s2 = src2[i2$1];
    _i2 = i2$1;
    continue;
  };
}

function union(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) {
  let src1r = src1ofs + src1len | 0;
  let src2r = src2ofs + src2len | 0;
  let _i1 = src1ofs;
  let _s1 = src[src1ofs];
  let _i2 = src2ofs;
  let _s2 = src2[src2ofs];
  let _d = dstofs;
  while (true) {
    let d = _d;
    let s2 = _s2;
    let i2 = _i2;
    let s1 = _s1;
    let i1 = _i1;
    if (s1 < s2) {
      dst[d] = s1;
      let i1$1 = i1 + 1 | 0;
      let d$1 = d + 1 | 0;
      if (i1$1 < src1r) {
        _d = d$1;
        _s1 = src[i1$1];
        _i1 = i1$1;
        continue;
      }
      Belt_Array.blitUnsafe(src2, i2, dst, d$1, src2r - i2 | 0);
      return (d$1 + src2r | 0) - i2 | 0;
    }
    if (s1 === s2) {
      dst[d] = s1;
      let i1$2 = i1 + 1 | 0;
      let i2$1 = i2 + 1 | 0;
      let d$2 = d + 1 | 0;
      if (!(i1$2 < src1r && i2$1 < src2r)) {
        if (i1$2 === src1r) {
          Belt_Array.blitUnsafe(src2, i2$1, dst, d$2, src2r - i2$1 | 0);
          return (d$2 + src2r | 0) - i2$1 | 0;
        } else {
          Belt_Array.blitUnsafe(src, i1$2, dst, d$2, src1r - i1$2 | 0);
          return (d$2 + src1r | 0) - i1$2 | 0;
        }
      }
      _d = d$2;
      _s2 = src2[i2$1];
      _i2 = i2$1;
      _s1 = src[i1$2];
      _i1 = i1$2;
      continue;
    }
    dst[d] = s2;
    let i2$2 = i2 + 1 | 0;
    let d$3 = d + 1 | 0;
    if (i2$2 < src2r) {
      _d = d$3;
      _s2 = src2[i2$2];
      _i2 = i2$2;
      continue;
    }
    Belt_Array.blitUnsafe(src, i1, dst, d$3, src1r - i1 | 0);
    return (d$3 + src1r | 0) - i1 | 0;
  };
}

function intersect(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) {
  let src1r = src1ofs + src1len | 0;
  let src2r = src2ofs + src2len | 0;
  let _i1 = src1ofs;
  let _s1 = src[src1ofs];
  let _i2 = src2ofs;
  let _s2 = src2[src2ofs];
  let _d = dstofs;
  while (true) {
    let d = _d;
    let s2 = _s2;
    let i2 = _i2;
    let s1 = _s1;
    let i1 = _i1;
    if (s1 < s2) {
      let i1$1 = i1 + 1 | 0;
      if (i1$1 >= src1r) {
        return d;
      }
      _s1 = src[i1$1];
      _i1 = i1$1;
      continue;
    }
    if (s1 === s2) {
      dst[d] = s1;
      let i1$2 = i1 + 1 | 0;
      let i2$1 = i2 + 1 | 0;
      let d$1 = d + 1 | 0;
      if (!(i1$2 < src1r && i2$1 < src2r)) {
        return d$1;
      }
      _d = d$1;
      _s2 = src2[i2$1];
      _i2 = i2$1;
      _s1 = src[i1$2];
      _i1 = i1$2;
      continue;
    }
    let i2$2 = i2 + 1 | 0;
    if (i2$2 >= src2r) {
      return d;
    }
    _s2 = src2[i2$2];
    _i2 = i2$2;
    continue;
  };
}

function diff(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) {
  let src1r = src1ofs + src1len | 0;
  let src2r = src2ofs + src2len | 0;
  let _i1 = src1ofs;
  let _s1 = src[src1ofs];
  let _i2 = src2ofs;
  let _s2 = src2[src2ofs];
  let _d = dstofs;
  while (true) {
    let d = _d;
    let s2 = _s2;
    let i2 = _i2;
    let s1 = _s1;
    let i1 = _i1;
    if (s1 < s2) {
      dst[d] = s1;
      let d$1 = d + 1 | 0;
      let i1$1 = i1 + 1 | 0;
      if (i1$1 >= src1r) {
        return d$1;
      }
      _d = d$1;
      _s1 = src[i1$1];
      _i1 = i1$1;
      continue;
    }
    if (s1 === s2) {
      let i1$2 = i1 + 1 | 0;
      let i2$1 = i2 + 1 | 0;
      if (!(i1$2 < src1r && i2$1 < src2r)) {
        if (i1$2 === src1r) {
          return d;
        } else {
          Belt_Array.blitUnsafe(src, i1$2, dst, d, src1r - i1$2 | 0);
          return (d + src1r | 0) - i1$2 | 0;
        }
      }
      _s2 = src2[i2$1];
      _i2 = i2$1;
      _s1 = src[i1$2];
      _i1 = i1$2;
      continue;
    }
    let i2$2 = i2 + 1 | 0;
    if (i2$2 < src2r) {
      _s2 = src2[i2$2];
      _i2 = i2$2;
      continue;
    }
    Belt_Array.blitUnsafe(src, i1, dst, d, src1r - i1 | 0);
    return (d + src1r | 0) - i1 | 0;
  };
}

function insertionSort(src, srcofs, dst, dstofs, len) {
  for (let i = 0; i < len; ++i) {
    let e = src[srcofs + i | 0];
    let j = (dstofs + i | 0) - 1 | 0;
    while (j >= dstofs && dst[j] > e) {
      dst[j + 1 | 0] = dst[j];
      j = j - 1 | 0;
    };
    dst[j + 1 | 0] = e;
  }
}

function sortTo(src, srcofs, dst, dstofs, len) {
  if (len <= 5) {
    return insertionSort(src, srcofs, dst, dstofs, len);
  }
  let l1 = len / 2 | 0;
  let l2 = len - l1 | 0;
  sortTo(src, srcofs + l1 | 0, dst, dstofs + l1 | 0, l2);
  sortTo(src, srcofs, src, srcofs + l2 | 0, l1);
  merge(src, srcofs + l2 | 0, l1, dst, dstofs + l1 | 0, l2, dst, dstofs);
}

function stableSortInPlace(a) {
  let l = a.length;
  if (l <= 5) {
    return insertionSort(a, 0, a, 0, l);
  }
  let l1 = l / 2 | 0;
  let l2 = l - l1 | 0;
  let t = new Array(l2);
  sortTo(a, l1, t, 0, l2);
  sortTo(a, 0, a, l2, l1);
  merge(a, l2, l1, t, 0, l2, a, 0);
}

function stableSort(a) {
  let b = a.slice(0);
  stableSortInPlace(b);
  return b;
}

function binarySearch(sorted, key) {
  let len = sorted.length;
  if (len === 0) {
    return -1;
  }
  let lo = sorted[0];
  if (key < lo) {
    return -1;
  }
  let hi = sorted[len - 1 | 0];
  if (key > hi) {
    return -(len + 1 | 0) | 0;
  } else {
    let _lo = 0;
    let _hi = len - 1 | 0;
    while (true) {
      let hi$1 = _hi;
      let lo$1 = _lo;
      let mid = (lo$1 + hi$1 | 0) / 2 | 0;
      let midVal = sorted[mid];
      if (key === midVal) {
        return mid;
      }
      if (key < midVal) {
        if (hi$1 === mid) {
          if (sorted[lo$1] === key) {
            return lo$1;
          } else {
            return -(hi$1 + 1 | 0) | 0;
          }
        }
        _hi = mid;
        continue;
      }
      if (lo$1 === mid) {
        if (sorted[hi$1] === key) {
          return hi$1;
        } else {
          return -(hi$1 + 1 | 0) | 0;
        }
      }
      _lo = mid;
      continue;
    };
  }
}

exports.strictlySortedLength = strictlySortedLength;
exports.isSorted = isSorted;
exports.stableSortInPlace = stableSortInPlace;
exports.stableSort = stableSort;
exports.binarySearch = binarySearch;
exports.union = union;
exports.intersect = intersect;
exports.diff = diff;
/* No side effect */

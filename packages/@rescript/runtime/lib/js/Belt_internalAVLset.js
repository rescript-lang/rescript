'use strict';

let Primitive_int = require("./Primitive_int.js");
let Belt_SortArray = require("./Belt_SortArray.js");
let Primitive_option = require("./Primitive_option.js");

function copy(n) {
  if (n !== undefined) {
    return {
      v: n.v,
      h: n.h,
      l: copy(n.l),
      r: copy(n.r)
    };
  } else {
    return n;
  }
}

function create(l, v, r) {
  let hl = l !== undefined ? l.h : 0;
  let hr = r !== undefined ? r.h : 0;
  return {
    v: v,
    h: (
      hl >= hr ? hl : hr
    ) + 1 | 0,
    l: l,
    r: r
  };
}

function singleton(x) {
  return {
    v: x,
    h: 1,
    l: undefined,
    r: undefined
  };
}

function heightGe(l, r) {
  if (r !== undefined) {
    if (l !== undefined) {
      return l.h >= r.h;
    } else {
      return false;
    }
  } else {
    return true;
  }
}

function bal(l, v, r) {
  let hl = l !== undefined ? l.h : 0;
  let hr = r !== undefined ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    let ll = l.l;
    let lr = l.r;
    if (heightGe(ll, lr)) {
      return create(ll, l.v, create(lr, v, r));
    } else {
      return create(create(ll, l.v, lr.l), lr.v, create(lr.r, v, r));
    }
  }
  if (hr <= (hl + 2 | 0)) {
    return {
      v: v,
      h: (
        hl >= hr ? hl : hr
      ) + 1 | 0,
      l: l,
      r: r
    };
  }
  let rl = r.l;
  let rr = r.r;
  if (heightGe(rr, rl)) {
    return create(create(l, v, rl), r.v, rr);
  } else {
    return create(create(l, v, rl.l), rl.v, create(rl.r, r.v, rr));
  }
}

function min0Aux(_n) {
  while (true) {
    let n = _n;
    let n$1 = n.l;
    if (n$1 === undefined) {
      return n.v;
    }
    _n = n$1;
    continue;
  };
}

function minimum(n) {
  if (n !== undefined) {
    return Primitive_option.some(min0Aux(n));
  }
}

function minUndefined(n) {
  if (n !== undefined) {
    return min0Aux(n);
  }
}

function max0Aux(_n) {
  while (true) {
    let n = _n;
    let n$1 = n.r;
    if (n$1 === undefined) {
      return n.v;
    }
    _n = n$1;
    continue;
  };
}

function maximum(n) {
  if (n !== undefined) {
    return Primitive_option.some(max0Aux(n));
  }
}

function maxUndefined(n) {
  if (n !== undefined) {
    return max0Aux(n);
  }
}

function removeMinAuxWithRef(n, v) {
  let ln = n.l;
  if (ln !== undefined) {
    return bal(removeMinAuxWithRef(ln, v), n.v, n.r);
  } else {
    v.contents = n.v;
    return n.r;
  }
}

function isEmpty(n) {
  return n === undefined;
}

function stackAllLeft(_v, _s) {
  while (true) {
    let s = _s;
    let v = _v;
    if (v === undefined) {
      return s;
    }
    _s = {
      hd: v,
      tl: s
    };
    _v = v.l;
    continue;
  };
}

function forEach(_n, f) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return;
    }
    forEach(n.l, f);
    f(n.v);
    _n = n.r;
    continue;
  };
}

function reduce(_s, _accu, f) {
  while (true) {
    let accu = _accu;
    let s = _s;
    if (s === undefined) {
      return accu;
    }
    _accu = f(reduce(s.l, accu, f), s.v);
    _s = s.r;
    continue;
  };
}

function every(_n, p) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return true;
    }
    if (!p(n.v)) {
      return false;
    }
    if (!every(n.l, p)) {
      return false;
    }
    _n = n.r;
    continue;
  };
}

function some(_n, p) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return false;
    }
    if (p(n.v)) {
      return true;
    }
    if (some(n.l, p)) {
      return true;
    }
    _n = n.r;
    continue;
  };
}

function addMinElement(n, v) {
  if (n !== undefined) {
    return bal(addMinElement(n.l, v), n.v, n.r);
  } else {
    return singleton(v);
  }
}

function addMaxElement(n, v) {
  if (n !== undefined) {
    return bal(n.l, n.v, addMaxElement(n.r, v));
  } else {
    return singleton(v);
  }
}

function joinShared(ln, v, rn) {
  if (ln === undefined) {
    return addMinElement(rn, v);
  }
  if (rn === undefined) {
    return addMaxElement(ln, v);
  }
  let lh = ln.h;
  let rh = rn.h;
  if (lh > (rh + 2 | 0)) {
    return bal(ln.l, ln.v, joinShared(ln.r, v, rn));
  } else if (rh > (lh + 2 | 0)) {
    return bal(joinShared(ln, v, rn.l), rn.v, rn.r);
  } else {
    return create(ln, v, rn);
  }
}

function concatShared(t1, t2) {
  if (t1 === undefined) {
    return t2;
  }
  if (t2 === undefined) {
    return t1;
  }
  let v = {
    contents: t2.v
  };
  let t2r = removeMinAuxWithRef(t2, v);
  return joinShared(t1, v.contents, t2r);
}

function partitionShared(n, p) {
  if (n === undefined) {
    return [
      undefined,
      undefined
    ];
  }
  let value = n.v;
  let match = partitionShared(n.l, p);
  let lf = match[1];
  let lt = match[0];
  let pv = p(value);
  let match$1 = partitionShared(n.r, p);
  let rf = match$1[1];
  let rt = match$1[0];
  if (pv) {
    return [
      joinShared(lt, value, rt),
      concatShared(lf, rf)
    ];
  } else {
    return [
      concatShared(lt, rt),
      joinShared(lf, value, rf)
    ];
  }
}

function lengthNode(n) {
  let l = n.l;
  let r = n.r;
  let sizeL = l !== undefined ? lengthNode(l) : 0;
  let sizeR = r !== undefined ? lengthNode(r) : 0;
  return (1 + sizeL | 0) + sizeR | 0;
}

function size(n) {
  if (n !== undefined) {
    return lengthNode(n);
  } else {
    return 0;
  }
}

function toListAux(_n, _accu) {
  while (true) {
    let accu = _accu;
    let n = _n;
    if (n === undefined) {
      return accu;
    }
    _accu = {
      hd: n.v,
      tl: toListAux(n.r, accu)
    };
    _n = n.l;
    continue;
  };
}

function toList(s) {
  return toListAux(s, /* [] */0);
}

function checkInvariantInternal(_v) {
  while (true) {
    let v = _v;
    if (v === undefined) {
      return;
    }
    let l = v.l;
    let r = v.r;
    let diff = (
      l !== undefined ? l.h : 0
    ) - (
      r !== undefined ? r.h : 0
    ) | 0;
    if (!(diff <= 2 && diff >= -2)) {
      throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "Belt_internalAVLset.res",
          310,
          4
        ],
        Error: new Error()
      };
    }
    checkInvariantInternal(l);
    _v = r;
    continue;
  };
}

function fillArray(_n, _i, arr) {
  while (true) {
    let i = _i;
    let n = _n;
    let v = n.v;
    let l = n.l;
    let r = n.r;
    let next = l !== undefined ? fillArray(l, i, arr) : i;
    arr[next] = v;
    let rnext = next + 1 | 0;
    if (r === undefined) {
      return rnext;
    }
    _i = rnext;
    _n = r;
    continue;
  };
}

function fillArrayWithPartition(_n, cursor, arr, p) {
  while (true) {
    let n = _n;
    let v = n.v;
    let l = n.l;
    let r = n.r;
    if (l !== undefined) {
      fillArrayWithPartition(l, cursor, arr, p);
    }
    if (p(v)) {
      let c = cursor.forward;
      arr[c] = v;
      cursor.forward = c + 1 | 0;
    } else {
      let c$1 = cursor.backward;
      arr[c$1] = v;
      cursor.backward = c$1 - 1 | 0;
    }
    if (r === undefined) {
      return;
    }
    _n = r;
    continue;
  };
}

function fillArrayWithFilter(_n, _i, arr, p) {
  while (true) {
    let i = _i;
    let n = _n;
    let v = n.v;
    let l = n.l;
    let r = n.r;
    let next = l !== undefined ? fillArrayWithFilter(l, i, arr, p) : i;
    let rnext = p(v) ? (arr[next] = v, next + 1 | 0) : next;
    if (r === undefined) {
      return rnext;
    }
    _i = rnext;
    _n = r;
    continue;
  };
}

function toArray(n) {
  if (n === undefined) {
    return [];
  }
  let size = lengthNode(n);
  let v = new Array(size);
  fillArray(n, 0, v);
  return v;
}

function fromSortedArrayRevAux(arr, off, len) {
  switch (len) {
    case 0 :
      return;
    case 1 :
      return singleton(arr[off]);
    case 2 :
      let x0 = arr[off];
      let x1 = arr[off - 1 | 0];
      return {
        v: x1,
        h: 2,
        l: singleton(x0),
        r: undefined
      };
    case 3 :
      let x0$1 = arr[off];
      let x1$1 = arr[off - 1 | 0];
      let x2 = arr[off - 2 | 0];
      return {
        v: x1$1,
        h: 2,
        l: singleton(x0$1),
        r: singleton(x2)
      };
    default:
      let nl = len / 2 | 0;
      let left = fromSortedArrayRevAux(arr, off, nl);
      let mid = arr[off - nl | 0];
      let right = fromSortedArrayRevAux(arr, (off - nl | 0) - 1 | 0, (len - nl | 0) - 1 | 0);
      return create(left, mid, right);
  }
}

function fromSortedArrayAux(arr, off, len) {
  switch (len) {
    case 0 :
      return;
    case 1 :
      return singleton(arr[off]);
    case 2 :
      let x0 = arr[off];
      let x1 = arr[off + 1 | 0];
      return {
        v: x1,
        h: 2,
        l: singleton(x0),
        r: undefined
      };
    case 3 :
      let x0$1 = arr[off];
      let x1$1 = arr[off + 1 | 0];
      let x2 = arr[off + 2 | 0];
      return {
        v: x1$1,
        h: 2,
        l: singleton(x0$1),
        r: singleton(x2)
      };
    default:
      let nl = len / 2 | 0;
      let left = fromSortedArrayAux(arr, off, nl);
      let mid = arr[off + nl | 0];
      let right = fromSortedArrayAux(arr, (off + nl | 0) + 1 | 0, (len - nl | 0) - 1 | 0);
      return create(left, mid, right);
  }
}

function fromSortedArrayUnsafe(arr) {
  return fromSortedArrayAux(arr, 0, arr.length);
}

function keepShared(n, p) {
  if (n === undefined) {
    return;
  }
  let v = n.v;
  let l = n.l;
  let r = n.r;
  let newL = keepShared(l, p);
  let pv = p(v);
  let newR = keepShared(r, p);
  if (pv) {
    if (l === newL && r === newR) {
      return n;
    } else {
      return joinShared(newL, v, newR);
    }
  } else {
    return concatShared(newL, newR);
  }
}

function keepCopy(n, p) {
  if (n === undefined) {
    return;
  }
  let size = lengthNode(n);
  let v = new Array(size);
  let last = fillArrayWithFilter(n, 0, v, p);
  return fromSortedArrayAux(v, 0, last);
}

function partitionCopy(n, p) {
  if (n === undefined) {
    return [
      undefined,
      undefined
    ];
  }
  let size = lengthNode(n);
  let v = new Array(size);
  let backward = size - 1 | 0;
  let cursor = {
    forward: 0,
    backward: backward
  };
  fillArrayWithPartition(n, cursor, v, p);
  let forwardLen = cursor.forward;
  return [
    fromSortedArrayAux(v, 0, forwardLen),
    fromSortedArrayRevAux(v, backward, size - forwardLen | 0)
  ];
}

function has(_t, x, cmp) {
  while (true) {
    let t = _t;
    if (t === undefined) {
      return false;
    }
    let v = t.v;
    let c = cmp(x, v);
    if (c === 0) {
      return true;
    }
    _t = c < 0 ? t.l : t.r;
    continue;
  };
}

function cmp(s1, s2, cmp$1) {
  let len1 = size(s1);
  let len2 = size(s2);
  if (len1 === len2) {
    let _e1 = stackAllLeft(s1, /* [] */0);
    let _e2 = stackAllLeft(s2, /* [] */0);
    while (true) {
      let e2 = _e2;
      let e1 = _e1;
      if (e1 === 0) {
        return 0;
      }
      if (e2 === 0) {
        return 0;
      }
      let h2 = e2.hd;
      let h1 = e1.hd;
      let c = cmp$1(h1.v, h2.v);
      if (c !== 0) {
        return c;
      }
      _e2 = stackAllLeft(h2.r, e2.tl);
      _e1 = stackAllLeft(h1.r, e1.tl);
      continue;
    };
  } else if (len1 < len2) {
    return -1;
  } else {
    return 1;
  }
}

function eq(s1, s2, c) {
  return cmp(s1, s2, c) === 0;
}

function subset(_s1, _s2, cmp) {
  while (true) {
    let s2 = _s2;
    let s1 = _s1;
    if (s1 === undefined) {
      return true;
    }
    if (s2 === undefined) {
      return false;
    }
    let v1 = s1.v;
    let l1 = s1.l;
    let r1 = s1.r;
    let v2 = s2.v;
    let l2 = s2.l;
    let r2 = s2.r;
    let c = cmp(v1, v2);
    if (c === 0) {
      if (!subset(l1, l2, cmp)) {
        return false;
      }
      _s2 = r2;
      _s1 = r1;
      continue;
    }
    if (c < 0) {
      if (!subset(create(l1, v1, undefined), l2, cmp)) {
        return false;
      }
      _s1 = r1;
      continue;
    }
    if (!subset(create(undefined, v1, r1), r2, cmp)) {
      return false;
    }
    _s1 = l1;
    continue;
  };
}

function get(_n, x, cmp) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return;
    }
    let v = n.v;
    let c = cmp(x, v);
    if (c === 0) {
      return Primitive_option.some(v);
    }
    _n = c < 0 ? n.l : n.r;
    continue;
  };
}

function getUndefined(_n, x, cmp) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return;
    }
    let v = n.v;
    let c = cmp(x, v);
    if (c === 0) {
      return v;
    }
    _n = c < 0 ? n.l : n.r;
    continue;
  };
}

function getOrThrow(_n, x, cmp) {
  while (true) {
    let n = _n;
    if (n !== undefined) {
      let v = n.v;
      let c = cmp(x, v);
      if (c === 0) {
        return v;
      }
      _n = c < 0 ? n.l : n.r;
      continue;
    }
    throw {
      RE_EXN_ID: "Not_found",
      Error: new Error()
    };
  };
}

function rotateWithLeftChild(k2) {
  let k1 = k2.l;
  k2.l = k1.r;
  k1.r = k2;
  let n = k2.l;
  let hlk2 = n !== undefined ? n.h : 0;
  let n$1 = k2.r;
  let hrk2 = n$1 !== undefined ? n$1.h : 0;
  k2.h = Primitive_int.max(hlk2, hrk2) + 1 | 0;
  let n$2 = k1.l;
  let hlk1 = n$2 !== undefined ? n$2.h : 0;
  let hk2 = k2.h;
  k1.h = Primitive_int.max(hlk1, hk2) + 1 | 0;
  return k1;
}

function rotateWithRightChild(k1) {
  let k2 = k1.r;
  k1.r = k2.l;
  k2.l = k1;
  let n = k1.l;
  let hlk1 = n !== undefined ? n.h : 0;
  let n$1 = k1.r;
  let hrk1 = n$1 !== undefined ? n$1.h : 0;
  k1.h = Primitive_int.max(hlk1, hrk1) + 1 | 0;
  let n$2 = k2.r;
  let hrk2 = n$2 !== undefined ? n$2.h : 0;
  let hk1 = k1.h;
  k2.h = Primitive_int.max(hrk2, hk1) + 1 | 0;
  return k2;
}

function doubleWithLeftChild(k3) {
  let k3l = k3.l;
  let v = rotateWithRightChild(k3l);
  k3.l = v;
  return rotateWithLeftChild(k3);
}

function doubleWithRightChild(k2) {
  let k2r = k2.r;
  let v = rotateWithLeftChild(k2r);
  k2.r = v;
  return rotateWithRightChild(k2);
}

function heightUpdateMutate(t) {
  let n = t.l;
  let hlt = n !== undefined ? n.h : 0;
  let n$1 = t.r;
  let hrt = n$1 !== undefined ? n$1.h : 0;
  t.h = Primitive_int.max(hlt, hrt) + 1 | 0;
  return t;
}

function balMutate(nt) {
  let l = nt.l;
  let r = nt.r;
  let hl = l !== undefined ? l.h : 0;
  let hr = r !== undefined ? r.h : 0;
  if (hl > (2 + hr | 0)) {
    let ll = l.l;
    let lr = l.r;
    if (heightGe(ll, lr)) {
      return heightUpdateMutate(rotateWithLeftChild(nt));
    } else {
      return heightUpdateMutate(doubleWithLeftChild(nt));
    }
  }
  if (hr > (2 + hl | 0)) {
    let rl = r.l;
    let rr = r.r;
    if (heightGe(rr, rl)) {
      return heightUpdateMutate(rotateWithRightChild(nt));
    } else {
      return heightUpdateMutate(doubleWithRightChild(nt));
    }
  }
  nt.h = Primitive_int.max(hl, hr) + 1 | 0;
  return nt;
}

function addMutate(cmp, t, x) {
  if (t === undefined) {
    return singleton(x);
  }
  let k = t.v;
  let c = cmp(x, k);
  if (c === 0) {
    return t;
  }
  let l = t.l;
  let r = t.r;
  if (c < 0) {
    let ll = addMutate(cmp, l, x);
    t.l = ll;
  } else {
    t.r = addMutate(cmp, r, x);
  }
  return balMutate(t);
}

function fromArray(xs, cmp) {
  let len = xs.length;
  if (len === 0) {
    return;
  }
  let next = Belt_SortArray.strictlySortedLength(xs, (x, y) => cmp(x, y) < 0);
  let result;
  if (next >= 0) {
    result = fromSortedArrayAux(xs, 0, next);
  } else {
    next = -next | 0;
    result = fromSortedArrayRevAux(xs, next - 1 | 0, next);
  }
  for (let i = next; i < len; ++i) {
    result = addMutate(cmp, result, xs[i]);
  }
  return result;
}

function removeMinAuxWithRootMutate(nt, n) {
  let ln = n.l;
  let rn = n.r;
  if (ln !== undefined) {
    n.l = removeMinAuxWithRootMutate(nt, ln);
    return balMutate(n);
  } else {
    nt.v = n.v;
    return rn;
  }
}

exports.copy = copy;
exports.create = create;
exports.bal = bal;
exports.singleton = singleton;
exports.minimum = minimum;
exports.minUndefined = minUndefined;
exports.maximum = maximum;
exports.maxUndefined = maxUndefined;
exports.removeMinAuxWithRef = removeMinAuxWithRef;
exports.isEmpty = isEmpty;
exports.stackAllLeft = stackAllLeft;
exports.forEach = forEach;
exports.reduce = reduce;
exports.every = every;
exports.some = some;
exports.joinShared = joinShared;
exports.concatShared = concatShared;
exports.keepShared = keepShared;
exports.keepCopy = keepCopy;
exports.partitionShared = partitionShared;
exports.partitionCopy = partitionCopy;
exports.lengthNode = lengthNode;
exports.size = size;
exports.toList = toList;
exports.checkInvariantInternal = checkInvariantInternal;
exports.fillArray = fillArray;
exports.toArray = toArray;
exports.fromSortedArrayAux = fromSortedArrayAux;
exports.fromSortedArrayRevAux = fromSortedArrayRevAux;
exports.fromSortedArrayUnsafe = fromSortedArrayUnsafe;
exports.has = has;
exports.cmp = cmp;
exports.eq = eq;
exports.subset = subset;
exports.get = get;
exports.getUndefined = getUndefined;
exports.getOrThrow = getOrThrow;
exports.fromArray = fromArray;
exports.addMutate = addMutate;
exports.balMutate = balMutate;
exports.removeMinAuxWithRootMutate = removeMinAuxWithRootMutate;
/* No side effect */

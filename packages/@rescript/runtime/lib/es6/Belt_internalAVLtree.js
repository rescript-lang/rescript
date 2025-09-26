

import * as Primitive_int from "./Primitive_int.js";
import * as Belt_SortArray from "./Belt_SortArray.js";
import * as Primitive_option from "./Primitive_option.js";

function treeHeight(n) {
  if (n !== undefined) {
    return n.h;
  } else {
    return 0;
  }
}

function copy(n) {
  if (n !== undefined) {
    return {
      k: n.k,
      v: n.v,
      h: n.h,
      l: copy(n.l),
      r: copy(n.r)
    };
  } else {
    return n;
  }
}

function create(l, x, d, r) {
  let hl = treeHeight(l);
  let hr = treeHeight(r);
  return {
    k: x,
    v: d,
    h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0,
    l: l,
    r: r
  };
}

function singleton(x, d) {
  return {
    k: x,
    v: d,
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

function updateValue(n, newValue) {
  if (n.v === newValue) {
    return n;
  } else {
    return {
      k: n.k,
      v: newValue,
      h: n.h,
      l: n.l,
      r: n.r
    };
  }
}

function bal(l, x, d, r) {
  let hl = l !== undefined ? l.h : 0;
  let hr = r !== undefined ? r.h : 0;
  if (hl > (hr + 2 | 0)) {
    let ll = l.l;
    let lr = l.r;
    if (treeHeight(ll) >= treeHeight(lr)) {
      return create(ll, l.k, l.v, create(lr, x, d, r));
    } else {
      return create(create(ll, l.k, l.v, lr.l), lr.k, lr.v, create(lr.r, x, d, r));
    }
  }
  if (hr <= (hl + 2 | 0)) {
    return {
      k: x,
      v: d,
      h: hl >= hr ? hl + 1 | 0 : hr + 1 | 0,
      l: l,
      r: r
    };
  }
  let rl = r.l;
  let rr = r.r;
  if (treeHeight(rr) >= treeHeight(rl)) {
    return create(create(l, x, d, rl), r.k, r.v, rr);
  } else {
    return create(create(l, x, d, rl.l), rl.k, rl.v, create(rl.r, r.k, r.v, rr));
  }
}

function minKey0Aux(_n) {
  while (true) {
    let n = _n;
    let n$1 = n.l;
    if (n$1 === undefined) {
      return n.k;
    }
    _n = n$1;
    continue;
  };
}

function minKey(n) {
  if (n !== undefined) {
    return Primitive_option.some(minKey0Aux(n));
  }
}

function minKeyUndefined(n) {
  if (n !== undefined) {
    return minKey0Aux(n);
  }
}

function maxKey0Aux(_n) {
  while (true) {
    let n = _n;
    let n$1 = n.r;
    if (n$1 === undefined) {
      return n.k;
    }
    _n = n$1;
    continue;
  };
}

function maxKey(n) {
  if (n !== undefined) {
    return Primitive_option.some(maxKey0Aux(n));
  }
}

function maxKeyUndefined(n) {
  if (n !== undefined) {
    return maxKey0Aux(n);
  }
}

function minKV0Aux(_n) {
  while (true) {
    let n = _n;
    let n$1 = n.l;
    if (n$1 === undefined) {
      return [
        n.k,
        n.v
      ];
    }
    _n = n$1;
    continue;
  };
}

function minimum(n) {
  if (n !== undefined) {
    return minKV0Aux(n);
  }
}

function minUndefined(n) {
  if (n !== undefined) {
    return minKV0Aux(n);
  }
}

function maxKV0Aux(_n) {
  while (true) {
    let n = _n;
    let n$1 = n.r;
    if (n$1 === undefined) {
      return [
        n.k,
        n.v
      ];
    }
    _n = n$1;
    continue;
  };
}

function maximum(n) {
  if (n !== undefined) {
    return maxKV0Aux(n);
  }
}

function maxUndefined(n) {
  if (n !== undefined) {
    return maxKV0Aux(n);
  }
}

function removeMinAuxWithRef(n, kr, vr) {
  let ln = n.l;
  if (ln !== undefined) {
    return bal(removeMinAuxWithRef(ln, kr, vr), n.k, n.v, n.r);
  } else {
    kr.contents = n.k;
    vr.contents = n.v;
    return n.r;
  }
}

function isEmpty(x) {
  return x === undefined;
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

function findFirstBy(n, p) {
  if (n === undefined) {
    return;
  }
  let left = findFirstBy(n.l, p);
  if (left !== undefined) {
    return left;
  }
  let v = n.k;
  let d = n.v;
  let pvd = p(v, d);
  if (pvd) {
    return [
      v,
      d
    ];
  }
  let right = findFirstBy(n.r, p);
  if (right !== undefined) {
    return right;
  }
}

function forEach(_n, f) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return;
    }
    forEach(n.l, f);
    f(n.k, n.v);
    _n = n.r;
    continue;
  };
}

function map(n, f) {
  if (n === undefined) {
    return;
  }
  let newLeft = map(n.l, f);
  let newD = f(n.v);
  let newRight = map(n.r, f);
  return {
    k: n.k,
    v: newD,
    h: n.h,
    l: newLeft,
    r: newRight
  };
}

function mapWithKey(n, f) {
  if (n === undefined) {
    return;
  }
  let key = n.k;
  let newLeft = mapWithKey(n.l, f);
  let newD = f(key, n.v);
  let newRight = mapWithKey(n.r, f);
  return {
    k: key,
    v: newD,
    h: n.h,
    l: newLeft,
    r: newRight
  };
}

function reduce(_m, _accu, f) {
  while (true) {
    let accu = _accu;
    let m = _m;
    if (m === undefined) {
      return accu;
    }
    let v = m.k;
    let d = m.v;
    let l = m.l;
    let r = m.r;
    _accu = f(reduce(l, accu, f), v, d);
    _m = r;
    continue;
  };
}

function every(_n, p) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return true;
    }
    if (!p(n.k, n.v)) {
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
    if (p(n.k, n.v)) {
      return true;
    }
    if (some(n.l, p)) {
      return true;
    }
    _n = n.r;
    continue;
  };
}

function addMinElement(n, k, v) {
  if (n !== undefined) {
    return bal(addMinElement(n.l, k, v), n.k, n.v, n.r);
  } else {
    return singleton(k, v);
  }
}

function addMaxElement(n, k, v) {
  if (n !== undefined) {
    return bal(n.l, n.k, n.v, addMaxElement(n.r, k, v));
  } else {
    return singleton(k, v);
  }
}

function join(ln, v, d, rn) {
  if (ln === undefined) {
    return addMinElement(rn, v, d);
  }
  if (rn === undefined) {
    return addMaxElement(ln, v, d);
  }
  let lv = ln.k;
  let ld = ln.v;
  let lh = ln.h;
  let ll = ln.l;
  let lr = ln.r;
  let rv = rn.k;
  let rd = rn.v;
  let rh = rn.h;
  let rl = rn.l;
  let rr = rn.r;
  if (lh > (rh + 2 | 0)) {
    return bal(ll, lv, ld, join(lr, v, d, rn));
  } else if (rh > (lh + 2 | 0)) {
    return bal(join(ln, v, d, rl), rv, rd, rr);
  } else {
    return create(ln, v, d, rn);
  }
}

function concat(t1, t2) {
  if (t1 === undefined) {
    return t2;
  }
  if (t2 === undefined) {
    return t1;
  }
  let kr = {
    contents: t2.k
  };
  let vr = {
    contents: t2.v
  };
  let t2r = removeMinAuxWithRef(t2, kr, vr);
  return join(t1, kr.contents, vr.contents, t2r);
}

function concatOrJoin(t1, v, d, t2) {
  if (d !== undefined) {
    return join(t1, v, Primitive_option.valFromOption(d), t2);
  } else {
    return concat(t1, t2);
  }
}

function keepShared(n, p) {
  if (n === undefined) {
    return;
  }
  let v = n.k;
  let d = n.v;
  let newLeft = keepShared(n.l, p);
  let pvd = p(v, d);
  let newRight = keepShared(n.r, p);
  if (pvd) {
    return join(newLeft, v, d, newRight);
  } else {
    return concat(newLeft, newRight);
  }
}

function keepMap(n, p) {
  if (n === undefined) {
    return;
  }
  let v = n.k;
  let d = n.v;
  let newLeft = keepMap(n.l, p);
  let pvd = p(v, d);
  let newRight = keepMap(n.r, p);
  if (pvd !== undefined) {
    return join(newLeft, v, Primitive_option.valFromOption(pvd), newRight);
  } else {
    return concat(newLeft, newRight);
  }
}

function partitionShared(n, p) {
  if (n === undefined) {
    return [
      undefined,
      undefined
    ];
  }
  let key = n.k;
  let value = n.v;
  let match = partitionShared(n.l, p);
  let lf = match[1];
  let lt = match[0];
  let pvd = p(key, value);
  let match$1 = partitionShared(n.r, p);
  let rf = match$1[1];
  let rt = match$1[0];
  if (pvd) {
    return [
      join(lt, key, value, rt),
      concat(lf, rf)
    ];
  } else {
    return [
      concat(lt, rt),
      join(lf, key, value, rf)
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
    let k = n.k;
    let v = n.v;
    let l = n.l;
    let r = n.r;
    _accu = {
      hd: [
        k,
        v
      ],
      tl: toListAux(r, accu)
    };
    _n = l;
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
    let diff = treeHeight(l) - treeHeight(r) | 0;
    if (!(diff <= 2 && diff >= -2)) {
      throw {
        RE_EXN_ID: "Assert_failure",
        _1: [
          "Belt_internalAVLtree.res",
          439,
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

function fillArrayKey(_n, _i, arr) {
  while (true) {
    let i = _i;
    let n = _n;
    let v = n.k;
    let l = n.l;
    let r = n.r;
    let next = l !== undefined ? fillArrayKey(l, i, arr) : i;
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

function fillArrayValue(_n, _i, arr) {
  while (true) {
    let i = _i;
    let n = _n;
    let l = n.l;
    let r = n.r;
    let next = l !== undefined ? fillArrayValue(l, i, arr) : i;
    arr[next] = n.v;
    let rnext = next + 1 | 0;
    if (r === undefined) {
      return rnext;
    }
    _i = rnext;
    _n = r;
    continue;
  };
}

function fillArray(_n, _i, arr) {
  while (true) {
    let i = _i;
    let n = _n;
    let l = n.l;
    let v = n.k;
    let r = n.r;
    let next = l !== undefined ? fillArray(l, i, arr) : i;
    arr[next] = [
      v,
      n.v
    ];
    let rnext = next + 1 | 0;
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

function keysToArray(n) {
  if (n === undefined) {
    return [];
  }
  let size = lengthNode(n);
  let v = new Array(size);
  fillArrayKey(n, 0, v);
  return v;
}

function valuesToArray(n) {
  if (n === undefined) {
    return [];
  }
  let size = lengthNode(n);
  let v = new Array(size);
  fillArrayValue(n, 0, v);
  return v;
}

function fromSortedArrayRevAux(arr, off, len) {
  switch (len) {
    case 0 :
      return;
    case 1 :
      let match = arr[off];
      return singleton(match[0], match[1]);
    case 2 :
      let match_0 = arr[off];
      let match_1 = arr[off - 1 | 0];
      let match$1 = match_1;
      let match$2 = match_0;
      return {
        k: match$1[0],
        v: match$1[1],
        h: 2,
        l: singleton(match$2[0], match$2[1]),
        r: undefined
      };
    case 3 :
      let match_0$1 = arr[off];
      let match_1$1 = arr[off - 1 | 0];
      let match_2 = arr[off - 2 | 0];
      let match$3 = match_2;
      let match$4 = match_1$1;
      let match$5 = match_0$1;
      return {
        k: match$4[0],
        v: match$4[1],
        h: 2,
        l: singleton(match$5[0], match$5[1]),
        r: singleton(match$3[0], match$3[1])
      };
    default:
      let nl = len / 2 | 0;
      let left = fromSortedArrayRevAux(arr, off, nl);
      let match$6 = arr[off - nl | 0];
      let right = fromSortedArrayRevAux(arr, (off - nl | 0) - 1 | 0, (len - nl | 0) - 1 | 0);
      return create(left, match$6[0], match$6[1], right);
  }
}

function fromSortedArrayAux(arr, off, len) {
  switch (len) {
    case 0 :
      return;
    case 1 :
      let match = arr[off];
      return singleton(match[0], match[1]);
    case 2 :
      let match_0 = arr[off];
      let match_1 = arr[off + 1 | 0];
      let match$1 = match_1;
      let match$2 = match_0;
      return {
        k: match$1[0],
        v: match$1[1],
        h: 2,
        l: singleton(match$2[0], match$2[1]),
        r: undefined
      };
    case 3 :
      let match_0$1 = arr[off];
      let match_1$1 = arr[off + 1 | 0];
      let match_2 = arr[off + 2 | 0];
      let match$3 = match_2;
      let match$4 = match_1$1;
      let match$5 = match_0$1;
      return {
        k: match$4[0],
        v: match$4[1],
        h: 2,
        l: singleton(match$5[0], match$5[1]),
        r: singleton(match$3[0], match$3[1])
      };
    default:
      let nl = len / 2 | 0;
      let left = fromSortedArrayAux(arr, off, nl);
      let match$6 = arr[off + nl | 0];
      let right = fromSortedArrayAux(arr, (off + nl | 0) + 1 | 0, (len - nl | 0) - 1 | 0);
      return create(left, match$6[0], match$6[1], right);
  }
}

function fromSortedArrayUnsafe(arr) {
  return fromSortedArrayAux(arr, 0, arr.length);
}

function cmp(s1, s2, kcmp, vcmp) {
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
      let c = kcmp(h1.k, h2.k);
      if (c !== 0) {
        return c;
      }
      let cx = vcmp(h1.v, h2.v);
      if (cx !== 0) {
        return cx;
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

function eq(s1, s2, kcmp, veq) {
  let len1 = size(s1);
  let len2 = size(s2);
  if (len1 === len2) {
    let _e1 = stackAllLeft(s1, /* [] */0);
    let _e2 = stackAllLeft(s2, /* [] */0);
    while (true) {
      let e2 = _e2;
      let e1 = _e1;
      if (e1 === 0) {
        return true;
      }
      if (e2 === 0) {
        return true;
      }
      let h2 = e2.hd;
      let h1 = e1.hd;
      if (!(kcmp(h1.k, h2.k) === 0 && veq(h1.v, h2.v))) {
        return false;
      }
      _e2 = stackAllLeft(h2.r, e2.tl);
      _e1 = stackAllLeft(h1.r, e1.tl);
      continue;
    };
  } else {
    return false;
  }
}

function get(_n, x, cmp) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return;
    }
    let v = n.k;
    let c = cmp(x, v);
    if (c === 0) {
      return Primitive_option.some(n.v);
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
    let v = n.k;
    let c = cmp(x, v);
    if (c === 0) {
      return n.v;
    }
    _n = c < 0 ? n.l : n.r;
    continue;
  };
}

function getOrThrow(_n, x, cmp) {
  while (true) {
    let n = _n;
    if (n !== undefined) {
      let v = n.k;
      let c = cmp(x, v);
      if (c === 0) {
        return n.v;
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

function getWithDefault(_n, x, def, cmp) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return def;
    }
    let v = n.k;
    let c = cmp(x, v);
    if (c === 0) {
      return n.v;
    }
    _n = c < 0 ? n.l : n.r;
    continue;
  };
}

function has(_n, x, cmp) {
  while (true) {
    let n = _n;
    if (n === undefined) {
      return false;
    }
    let v = n.k;
    let c = cmp(x, v);
    if (c === 0) {
      return true;
    }
    _n = c < 0 ? n.l : n.r;
    continue;
  };
}

function rotateWithLeftChild(k2) {
  let k1 = k2.l;
  k2.l = k1.r;
  k1.r = k2;
  let hlk2 = treeHeight(k2.l);
  let hrk2 = treeHeight(k2.r);
  k2.h = Primitive_int.max(hlk2, hrk2) + 1 | 0;
  let hlk1 = treeHeight(k1.l);
  let hk2 = k2.h;
  k1.h = Primitive_int.max(hlk1, hk2) + 1 | 0;
  return k1;
}

function rotateWithRightChild(k1) {
  let k2 = k1.r;
  k1.r = k2.l;
  k2.l = k1;
  let hlk1 = treeHeight(k1.l);
  let hrk1 = treeHeight(k1.r);
  k1.h = Primitive_int.max(hlk1, hrk1) + 1 | 0;
  let hrk2 = treeHeight(k2.r);
  let hk1 = k1.h;
  k2.h = Primitive_int.max(hrk2, hk1) + 1 | 0;
  return k2;
}

function doubleWithLeftChild(k3) {
  let x = k3.l;
  let v = rotateWithRightChild(x);
  k3.l = v;
  return rotateWithLeftChild(k3);
}

function doubleWithRightChild(k2) {
  let x = k2.r;
  let v = rotateWithLeftChild(x);
  k2.r = v;
  return rotateWithRightChild(k2);
}

function heightUpdateMutate(t) {
  let hlt = treeHeight(t.l);
  let hrt = treeHeight(t.r);
  t.h = Primitive_int.max(hlt, hrt) + 1 | 0;
  return t;
}

function balMutate(nt) {
  let l = nt.l;
  let r = nt.r;
  let hl = treeHeight(l);
  let hr = treeHeight(r);
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

function updateMutate(t, x, data, cmp) {
  if (t === undefined) {
    return singleton(x, data);
  }
  let k = t.k;
  let c = cmp(x, k);
  if (c === 0) {
    t.v = data;
    return t;
  }
  let l = t.l;
  let r = t.r;
  if (c < 0) {
    let ll = updateMutate(l, x, data, cmp);
    t.l = ll;
  } else {
    t.r = updateMutate(r, x, data, cmp);
  }
  return balMutate(t);
}

function fromArray(xs, cmp) {
  let len = xs.length;
  if (len === 0) {
    return;
  }
  let next = Belt_SortArray.strictlySortedLength(xs, (param, param$1) => cmp(param[0], param$1[0]) < 0);
  let result;
  if (next >= 0) {
    result = fromSortedArrayAux(xs, 0, next);
  } else {
    next = -next | 0;
    result = fromSortedArrayRevAux(xs, next - 1 | 0, next);
  }
  for (let i = next; i < len; ++i) {
    let match = xs[i];
    result = updateMutate(result, match[0], match[1], cmp);
  }
  return result;
}

function removeMinAuxWithRootMutate(nt, n) {
  let rn = n.r;
  let ln = n.l;
  if (ln !== undefined) {
    n.l = removeMinAuxWithRootMutate(nt, ln);
    return balMutate(n);
  } else {
    nt.k = n.k;
    nt.v = n.v;
    return rn;
  }
}

export {
  copy,
  create,
  bal,
  singleton,
  updateValue,
  minKey,
  minKeyUndefined,
  maxKey,
  maxKeyUndefined,
  minimum,
  minUndefined,
  maximum,
  maxUndefined,
  removeMinAuxWithRef,
  isEmpty,
  stackAllLeft,
  findFirstBy,
  forEach,
  map,
  mapWithKey,
  reduce,
  every,
  some,
  join,
  concat,
  concatOrJoin,
  keepShared,
  keepMap,
  partitionShared,
  lengthNode,
  size,
  toList,
  checkInvariantInternal,
  fillArray,
  toArray,
  keysToArray,
  valuesToArray,
  fromSortedArrayAux,
  fromSortedArrayRevAux,
  fromSortedArrayUnsafe,
  cmp,
  eq,
  get,
  getUndefined,
  getWithDefault,
  getOrThrow,
  has,
  fromArray,
  updateMutate,
  balMutate,
  removeMinAuxWithRootMutate,
}
/* No side effect */

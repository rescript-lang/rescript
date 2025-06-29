

import * as Belt_internalAVLset from "./Belt_internalAVLset.js";
import * as Belt_internalSetInt from "./Belt_internalSetInt.js";

function add(t, x) {
  if (t === undefined) {
    return Belt_internalAVLset.singleton(x);
  }
  let v = t.v;
  if (x === v) {
    return t;
  }
  let l = t.l;
  let r = t.r;
  if (x < v) {
    let ll = add(l, x);
    if (ll === l) {
      return t;
    } else {
      return Belt_internalAVLset.bal(ll, v, r);
    }
  }
  let rr = add(r, x);
  if (rr === r) {
    return t;
  } else {
    return Belt_internalAVLset.bal(l, v, rr);
  }
}

function mergeMany(h, arr) {
  let len = arr.length;
  let v = h;
  for (let i = 0; i < len; ++i) {
    let key = arr[i];
    v = add(v, key);
  }
  return v;
}

function remove(t, x) {
  if (t === undefined) {
    return t;
  }
  let v = t.v;
  let l = t.l;
  let r = t.r;
  if (x === v) {
    if (l === undefined) {
      return r;
    }
    if (r === undefined) {
      return l;
    }
    let v$1 = {
      contents: r.v
    };
    let r$1 = Belt_internalAVLset.removeMinAuxWithRef(r, v$1);
    return Belt_internalAVLset.bal(l, v$1.contents, r$1);
  }
  if (x < v) {
    let ll = remove(l, x);
    if (ll === l) {
      return t;
    } else {
      return Belt_internalAVLset.bal(ll, v, r);
    }
  }
  let rr = remove(r, x);
  if (rr === r) {
    return t;
  } else {
    return Belt_internalAVLset.bal(l, v, rr);
  }
}

function removeMany(h, arr) {
  let len = arr.length;
  let v = h;
  for (let i = 0; i < len; ++i) {
    let key = arr[i];
    v = remove(v, key);
  }
  return v;
}

function splitAuxNoPivot(n, x) {
  let v = n.v;
  let l = n.l;
  let r = n.r;
  if (x === v) {
    return [
      l,
      r
    ];
  }
  if (x < v) {
    if (l === undefined) {
      return [
        undefined,
        n
      ];
    }
    let match = splitAuxNoPivot(l, x);
    return [
      match[0],
      Belt_internalAVLset.joinShared(match[1], v, r)
    ];
  }
  if (r === undefined) {
    return [
      n,
      undefined
    ];
  }
  let match$1 = splitAuxNoPivot(r, x);
  return [
    Belt_internalAVLset.joinShared(l, v, match$1[0]),
    match$1[1]
  ];
}

function splitAuxPivot(n, x, pres) {
  let v = n.v;
  let l = n.l;
  let r = n.r;
  if (x === v) {
    pres.contents = true;
    return [
      l,
      r
    ];
  }
  if (x < v) {
    if (l === undefined) {
      return [
        undefined,
        n
      ];
    }
    let match = splitAuxPivot(l, x, pres);
    return [
      match[0],
      Belt_internalAVLset.joinShared(match[1], v, r)
    ];
  }
  if (r === undefined) {
    return [
      n,
      undefined
    ];
  }
  let match$1 = splitAuxPivot(r, x, pres);
  return [
    Belt_internalAVLset.joinShared(l, v, match$1[0]),
    match$1[1]
  ];
}

function split(t, x) {
  if (t === undefined) {
    return [
      [
        undefined,
        undefined
      ],
      false
    ];
  }
  let pres = {
    contents: false
  };
  let v = splitAuxPivot(t, x, pres);
  return [
    v,
    pres.contents
  ];
}

function union(s1, s2) {
  if (s1 === undefined) {
    return s2;
  }
  if (s2 === undefined) {
    return s1;
  }
  let h1 = s1.h;
  let h2 = s2.h;
  if (h1 >= h2) {
    if (h2 === 1) {
      return add(s1, s2.v);
    }
    let v1 = s1.v;
    let l1 = s1.l;
    let r1 = s1.r;
    let match = splitAuxNoPivot(s2, v1);
    return Belt_internalAVLset.joinShared(union(l1, match[0]), v1, union(r1, match[1]));
  }
  if (h1 === 1) {
    return add(s2, s1.v);
  }
  let v2 = s2.v;
  let l2 = s2.l;
  let r2 = s2.r;
  let match$1 = splitAuxNoPivot(s1, v2);
  return Belt_internalAVLset.joinShared(union(match$1[0], l2), v2, union(match$1[1], r2));
}

function intersect(s1, s2) {
  if (s1 === undefined) {
    return;
  }
  if (s2 === undefined) {
    return;
  }
  let v1 = s1.v;
  let l1 = s1.l;
  let r1 = s1.r;
  let pres = {
    contents: false
  };
  let match = splitAuxPivot(s2, v1, pres);
  let ll = intersect(l1, match[0]);
  let rr = intersect(r1, match[1]);
  if (pres.contents) {
    return Belt_internalAVLset.joinShared(ll, v1, rr);
  } else {
    return Belt_internalAVLset.concatShared(ll, rr);
  }
}

function diff(s1, s2) {
  if (s1 === undefined) {
    return s1;
  }
  if (s2 === undefined) {
    return s1;
  }
  let v1 = s1.v;
  let l1 = s1.l;
  let r1 = s1.r;
  let pres = {
    contents: false
  };
  let match = splitAuxPivot(s2, v1, pres);
  let ll = diff(l1, match[0]);
  let rr = diff(r1, match[1]);
  if (pres.contents) {
    return Belt_internalAVLset.concatShared(ll, rr);
  } else {
    return Belt_internalAVLset.joinShared(ll, v1, rr);
  }
}

let empty;

let fromArray = Belt_internalSetInt.fromArray;

let fromSortedArrayUnsafe = Belt_internalAVLset.fromSortedArrayUnsafe;

let isEmpty = Belt_internalAVLset.isEmpty;

let has = Belt_internalSetInt.has;

let subset = Belt_internalSetInt.subset;

let cmp = Belt_internalSetInt.cmp;

let eq = Belt_internalSetInt.eq;

let forEachU = Belt_internalAVLset.forEach;

let forEach = Belt_internalAVLset.forEach;

let reduceU = Belt_internalAVLset.reduce;

let reduce = Belt_internalAVLset.reduce;

let everyU = Belt_internalAVLset.every;

let every = Belt_internalAVLset.every;

let someU = Belt_internalAVLset.some;

let some = Belt_internalAVLset.some;

let keepU = Belt_internalAVLset.keepShared;

let keep = Belt_internalAVLset.keepShared;

let partitionU = Belt_internalAVLset.partitionShared;

let partition = Belt_internalAVLset.partitionShared;

let size = Belt_internalAVLset.size;

let toList = Belt_internalAVLset.toList;

let toArray = Belt_internalAVLset.toArray;

let minimum = Belt_internalAVLset.minimum;

let minUndefined = Belt_internalAVLset.minUndefined;

let maximum = Belt_internalAVLset.maximum;

let maxUndefined = Belt_internalAVLset.maxUndefined;

let get = Belt_internalSetInt.get;

let getUndefined = Belt_internalSetInt.getUndefined;

let getExn = Belt_internalSetInt.getOrThrow;

let getOrThrow = Belt_internalSetInt.getOrThrow;

let checkInvariantInternal = Belt_internalAVLset.checkInvariantInternal;

export {
  empty,
  fromArray,
  fromSortedArrayUnsafe,
  isEmpty,
  has,
  add,
  mergeMany,
  remove,
  removeMany,
  union,
  intersect,
  diff,
  subset,
  cmp,
  eq,
  forEachU,
  forEach,
  reduceU,
  reduce,
  everyU,
  every,
  someU,
  some,
  keepU,
  keep,
  partitionU,
  partition,
  size,
  toList,
  toArray,
  minimum,
  minUndefined,
  maximum,
  maxUndefined,
  get,
  getUndefined,
  getExn,
  getOrThrow,
  split,
  checkInvariantInternal,
}
/* No side effect */
